# Shared internal helpers for the automated modelling pipeline.

.react_escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

.react_quote_name <- function(x) {
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", x)) return(x)
  paste0("`", gsub("`", "\\\\`", x, fixed = TRUE), "`")
}

.react_reformulate_safe <- function(terms, response = NULL) {
  terms <- unique(terms[nzchar(terms)])
  rhs <- if (length(terms)) {
    paste(vapply(terms, .react_quote_name, character(1)), collapse = " + ")
  } else {
    "1"
  }
  if (is.null(response)) {
    stats::as.formula(paste("~", rhs))
  } else {
    stats::as.formula(paste(.react_quote_name(response), "~", rhs))
  }
}

.react_detect_outcome_family <- function(y, outcome = "outcome") {
  non_missing <- unique(stats::na.omit(y))
  if (length(non_missing) < 2L) {
    stop("Less than two unique non-missing values found for outcome '", outcome, "'.")
  }
  if (length(non_missing) == 2L) "binomial" else "gaussian"
}

.react_prepare_analysis_frame <- function(data, vars, drop_missing = TRUE) {
  out <- as.data.frame(data[, vars, drop = FALSE], stringsAsFactors = FALSE)
  for (nm in names(out)) {
    if (is.character(out[[nm]])) out[[nm]] <- factor(out[[nm]])
  }
  if (isTRUE(drop_missing)) {
    keep <- stats::complete.cases(out)
    out <- out[keep, , drop = FALSE]
  }
  for (nm in names(out)) {
    if (is.factor(out[[nm]])) out[[nm]] <- droplevels(out[[nm]])
  }
  out
}

.react_reference_level <- function(x, ref_level = NULL) {
  if (!is.factor(x) || is.ordered(x)) return(NULL)
  lvls <- levels(x)
  if (!length(lvls)) return(NULL)
  if (!is.null(ref_level) && ref_level %in% lvls) return(ref_level)
  lvls[[1]]
}

.react_empty_coef_table <- function() {
  data.frame(
    Level = character(),
    term_raw = character(),
    OR = numeric(),
    Lower = numeric(),
    Upper = numeric(),
    P_value = numeric(),
    is_reference = logical(),
    is_intercept = logical(),
    model_family = character(),
    stringsAsFactors = FALSE
  )
}

.react_fit_glm_captured <- function(formula, data, family, control) {
  warn_store <- character()
  fit <- tryCatch(
    withCallingHandlers(
      stats::glm(formula, data = data, family = family, control = control),
      warning = function(w) {
        warn_store <<- c(warn_store, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(list(`__error__` = conditionMessage(e)), class = "glm_error")
  )
  if (inherits(fit, "glm_error")) {
    return(list(fit = NULL, warnings = warn_store, error = fit$`__error__`))
  }
  list(fit = fit, warnings = warn_store, error = NULL)
}

.react_build_coef_table <- function(mod, ref_level = NULL, dp = 3) {
  if (is.null(mod) || inherits(mod, "try-error")) {
    return(.react_empty_coef_table())
  }

  model_family <- tryCatch(mod$family$family, error = function(e) NULL)
  if (is.null(model_family) || !length(model_family)) {
    model_family <- if (inherits(mod, "lm")) "gaussian" else NA_character_
  }
  exponentiate <- identical(model_family, "binomial")

  if (inherits(mod, "gam")) {
    coef_tab <- tryCatch({
      summ <- mgcv::summary.gam(mod)
      out <- as.data.frame(summ$p.table, stringsAsFactors = FALSE)
      out$term_raw <- rownames(out)
      out$Lower <- out$Estimate - stats::qnorm(0.975) * out$`Std. Error`
      out$Upper <- out$Estimate + stats::qnorm(0.975) * out$`Std. Error`
      out <- out[, c("term_raw", "Estimate", "Lower", "Upper", "Pr(>|z|)"), drop = FALSE]
      names(out) <- c("term_raw", "OR", "Lower", "Upper", "P_value")
      out
    }, error = function(e) NULL)
  } else {
    coef_tab <- tryCatch({
      coef_summ <- summary(mod)$coefficients
      se_col <- grep("Std\\.|Std\\. Error|Standard Error", colnames(coef_summ), ignore.case = TRUE)[1]
      p_col <- grep("Pr\\(", colnames(coef_summ))[1]
      if (is.na(se_col) || is.na(p_col)) stop("Could not locate coefficient summary columns.")
      out <- data.frame(
        term_raw = rownames(coef_summ),
        OR = as.numeric(coef_summ[, 1]),
        Lower = as.numeric(coef_summ[, 1] - stats::qnorm(0.975) * coef_summ[, se_col]),
        Upper = as.numeric(coef_summ[, 1] + stats::qnorm(0.975) * coef_summ[, se_col]),
        P_value = as.numeric(coef_summ[, p_col]),
        stringsAsFactors = FALSE
      )
      out
    }, error = function(e) NULL)
  }

  if (is.null(coef_tab) || !nrow(coef_tab)) {
    return(.react_empty_coef_table())
  }

  if (exponentiate) {
    coef_tab$OR <- exp(coef_tab$OR)
    coef_tab$Lower <- exp(coef_tab$Lower)
    coef_tab$Upper <- exp(coef_tab$Upper)
  }

  coef_tab$OR <- round(coef_tab$OR, dp)
  coef_tab$Lower <- round(coef_tab$Lower, dp)
  coef_tab$Upper <- round(coef_tab$Upper, dp)
  coef_tab$P_value <- round(coef_tab$P_value, 5)
  coef_tab$Level <- coef_tab$term_raw
  coef_tab$is_reference <- FALSE
  coef_tab$is_intercept <- coef_tab$term_raw == "(Intercept)"
  coef_tab$model_family <- model_family
  coef_tab <- coef_tab[, c("Level", "term_raw", "OR", "Lower", "Upper",
                           "P_value", "is_reference", "is_intercept",
                           "model_family"), drop = FALSE]

  if (!is.null(ref_level)) {
    ref_row <- data.frame(
      Level = paste0(ref_level, " [reference]"),
      term_raw = NA_character_,
      OR = NA_real_,
      Lower = NA_real_,
      Upper = NA_real_,
      P_value = NA_real_,
      is_reference = TRUE,
      is_intercept = FALSE,
      model_family = model_family,
      stringsAsFactors = FALSE
    )
    if (any(coef_tab$is_intercept)) {
      int_idx <- which(coef_tab$is_intercept)[1]
      coef_tab <- rbind(
        coef_tab[seq_len(int_idx), , drop = FALSE],
        ref_row,
        coef_tab[-seq_len(int_idx), , drop = FALSE]
      )
    } else {
      coef_tab <- rbind(ref_row, coef_tab)
    }
  }

  rownames(coef_tab) <- NULL
  coef_tab
}

.react_predictor_term_id <- function(fit, predictor) {
  term_labels <- attr(stats::terms(fit), "term.labels")
  if (is.null(term_labels)) return(NA_integer_)
  clean_labels <- gsub("`", "", term_labels, fixed = TRUE)
  match(predictor, clean_labels)
}

.react_extract_predictor_rows <- function(fit, predictor, ref_level = NULL, include_intercept = TRUE) {
  coef_tab <- .react_build_coef_table(fit, ref_level = NULL)
  if (!nrow(coef_tab)) return(coef_tab)

  model_frame <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  model_matrix <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  term_labels <- attr(stats::terms(fit), "term.labels")
  pred_term_id <- .react_predictor_term_id(fit, predictor)

  predictor_rows <- rep(FALSE, nrow(coef_tab))
  if (!is.null(model_matrix) && !is.na(pred_term_id)) {
    assign_index <- attr(model_matrix, "assign")
    matrix_terms <- gsub("`", "", colnames(model_matrix), fixed = TRUE)
    coef_terms <- gsub("`", "", coef_tab$term_raw, fixed = TRUE)
    coef_idx <- match(coef_terms, matrix_terms)
    valid_idx <- !is.na(coef_idx)
    predictor_rows[valid_idx] <- assign_index[coef_idx[valid_idx]] == pred_term_id
  }

  is_categorical <- !is.null(model_frame) &&
    predictor %in% names(model_frame) &&
    is.factor(model_frame[[predictor]]) &&
    !is.ordered(model_frame[[predictor]])

  out_parts <- list()

  if (isTRUE(include_intercept) && any(coef_tab$is_intercept)) {
    intercept_row <- coef_tab[coef_tab$is_intercept, , drop = FALSE]
    intercept_row$Level <- "(Intercept)"
    intercept_row$row_order <- 0L
    out_parts[[length(out_parts) + 1L]] <- intercept_row
  }

  predictor_tab <- coef_tab[predictor_rows, , drop = FALSE]

  if (is_categorical) {
    predictor_ref <- .react_reference_level(model_frame[[predictor]], ref_level = ref_level)
    lvls <- levels(model_frame[[predictor]])
    ref_row <- data.frame(
      Level = paste0(predictor_ref, " [reference]"),
      term_raw = NA_character_,
      OR = NA_real_,
      Lower = NA_real_,
      Upper = NA_real_,
      P_value = NA_real_,
      is_reference = TRUE,
      is_intercept = FALSE,
      model_family = unique(coef_tab$model_family)[1],
      row_order = 1L,
      stringsAsFactors = FALSE
    )
    out_parts[[length(out_parts) + 1L]] <- ref_row

    term_label <- if (!is.null(term_labels) && !is.na(pred_term_id)) term_labels[[pred_term_id]] else predictor
    term_pattern <- paste0("^", .react_escape_regex(term_label))
    predictor_tab$Level <- sub(term_pattern, "", predictor_tab$term_raw)
    predictor_tab$Level[predictor_tab$Level == ""] <- predictor_tab$term_raw[predictor_tab$Level == ""]
    predictor_tab$row_order <- match(predictor_tab$Level, setdiff(lvls, predictor_ref)) + 1L
  } else {
    if (nrow(predictor_tab) == 1L) {
      predictor_tab$Level <- predictor
      predictor_tab$row_order <- 1L
    } else if (nrow(predictor_tab)) {
      predictor_tab$Level <- predictor_tab$term_raw
      predictor_tab$row_order <- seq_len(nrow(predictor_tab)) + if (isTRUE(include_intercept)) 0L else 1L
    }
  }

  if (nrow(predictor_tab)) {
    predictor_tab$is_reference <- FALSE
    predictor_tab$is_intercept <- FALSE
    out_parts[[length(out_parts) + 1L]] <- predictor_tab
  }

  if (!length(out_parts)) return(.react_empty_coef_table())

  out <- dplyr::bind_rows(out_parts)
  if (!"row_order" %in% names(out)) out$row_order <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

.react_format_effect_ci <- function(effect, lower, upper, sf = 2, format = "f", simpleround = FALSE) {
  sprintf(
    "%s (%s,%s)",
    specifyDecimal(effect, sf, format = format, simpleround = simpleround),
    specifyDecimal(lower, sf, format = format, simpleround = simpleround),
    specifyDecimal(upper, sf, format = format, simpleround = simpleround)
  )
}

.react_extract_rd_rows <- function(stage_meta, predictor, n_sim = 100, dp = 3) {
  if (is.null(stage_meta$fit)) return(data.frame())
  if (!identical(stage_meta$model_family, "binomial")) return(data.frame())

  rd_tab <- safe_makeRDTable(
    mod = stage_meta$fit,
    variable_name = predictor,
    ref_level = stage_meta$ref_level,
    dp = dp,
    data = stage_meta$data,
    n_sim = n_sim
  )
  if (is.null(rd_tab) || !nrow(rd_tab)) return(data.frame())

  rd_tab <- as.data.frame(rd_tab, stringsAsFactors = FALSE)
  rownames(rd_tab) <- NULL
  is_categorical <- predictor %in% names(stage_meta$data) &&
    is.factor(stage_meta$data[[predictor]]) &&
    !is.ordered(stage_meta$data[[predictor]])

  if (is_categorical) {
    predictor_ref <- .react_reference_level(stage_meta$data[[predictor]], ref_level = stage_meta$ref_level)
    lvls <- levels(stage_meta$data[[predictor]])
    rd_tab$row_order <- match(rd_tab$Level, c(paste0(predictor_ref, " [reference]"), setdiff(lvls, predictor_ref)))
    rd_tab$term_raw <- ifelse(
      grepl("\\[reference\\]$", rd_tab$Level),
      NA_character_,
      paste0(predictor, rd_tab$Level)
    )
  } else {
    rd_tab$row_order <- seq_len(nrow(rd_tab))
    rd_tab$term_raw <- if (nrow(rd_tab) == 1L) predictor else rd_tab$Level
  }

  rd_tab$is_reference <- grepl("\\[reference\\]$", rd_tab$Level)
  rd_tab$is_intercept <- FALSE
  rd_tab$model_family <- stage_meta$model_family
  rd_tab$model <- stage_meta$model
  rd_tab$stage_id <- stage_meta$stage_id
  rd_tab$wide_col <- sub("_OR$", "_RD", stage_meta$wide_col)
  rd_tab$adjustment <- stage_meta$adjustment
  rd_tab$adjusted_vars <- paste(stage_meta$adjusted_vars, collapse = "+")
  rd_tab$Nobs_in_model <- nrow(stage_meta$data)

  rd_tab[, c("Level", "term_raw", "RD", "Lower", "Upper", "P_value",
             "is_reference", "is_intercept", "model_family", "row_order",
             "model", "stage_id", "wide_col", "adjustment",
             "adjusted_vars", "Nobs_in_model"), drop = FALSE]
}

.react_build_stage_diagnostic <- function(stage_name, adjusted_vars, fit_res, fit_obj,
                                          stage_data, predictor, outcome, model_family) {
  vcov_ok <- FALSE
  if (!is.null(fit_obj)) {
    vcov_ok <- tryCatch({
      vc <- stats::vcov(fit_obj)
      !(is.null(vc) || anyNA(vc))
    }, error = function(e) FALSE)
  }

  separation_msg <- ""
  if (exists("check_perfect_separation", mode = "function")) {
    sep_msg <- tryCatch(
      check_perfect_separation(stage_data, outcome = outcome, predictor = predictor),
      error = function(e) NULL
    )
    if (!is.null(sep_msg)) separation_msg <- sep_msg
  }

  data.frame(
    stage = stage_name,
    adjusted_vars = paste(adjusted_vars, collapse = "+"),
    status = if (!is.null(fit_res$error)) "error" else if (length(fit_res$warnings)) "warning" else "ok",
    error_msg = if (is.null(fit_res$error)) "" else fit_res$error,
    warnings = paste(unique(fit_res$warnings), collapse = " | "),
    nobs = nrow(stage_data),
    vcov_ok = isTRUE(vcov_ok),
    separation_msg = separation_msg,
    rd_rows = NA_integer_,
    model_family = model_family,
    stringsAsFactors = FALSE
  )
}

.react_run_or_stages <- function(data,
                                 predictor,
                                 outcome,
                                 stage_specs,
                                 sample_strategy = c("common_per_predictor", "per_model"),
                                 glm_control = stats::glm.control(maxit = 50),
                                 include_intercept = TRUE,
                                 ref_level = NULL) {
  sample_strategy <- match.arg(sample_strategy)
  model_family <- .react_detect_outcome_family(data[[outcome]], outcome = outcome)

  used_adjusters <- unique(unlist(lapply(stage_specs, `[[`, "adjusted_vars"), use.names = FALSE))
  common_vars <- unique(c(outcome, predictor, used_adjusters))

  common_data <- NULL
  if (identical(sample_strategy, "common_per_predictor")) {
    common_data <- .react_prepare_analysis_frame(data, common_vars, drop_missing = TRUE)
    if (!nrow(common_data)) {
      stop("No complete cases available for predictor '", predictor, "'.")
    }
  }

  rows_by_stage <- vector("list", length(stage_specs))
  stage_meta <- vector("list", length(stage_specs))
  diagnostics <- vector("list", length(stage_specs))

  names(rows_by_stage) <- vapply(stage_specs, `[[`, character(1), "stage_id")
  names(stage_meta) <- names(rows_by_stage)
  names(diagnostics) <- names(rows_by_stage)

  for (i in seq_along(stage_specs)) {
    stage <- stage_specs[[i]]
    stage_vars <- unique(c(outcome, predictor, stage$adjusted_vars))
    stage_data <- if (identical(sample_strategy, "common_per_predictor")) {
      common_data[, stage_vars, drop = FALSE]
    } else {
      .react_prepare_analysis_frame(data, stage_vars, drop_missing = TRUE)
    }

    fit_res <- list(fit = NULL, warnings = character(), error = "No complete cases available.")
    predictor_ref <- NULL
    if (nrow(stage_data)) {
      predictor_ref <- .react_reference_level(stage_data[[predictor]], ref_level = ref_level)
      formula <- .react_reformulate_safe(c(predictor, stage$adjusted_vars), response = outcome)
      fit_res <- .react_fit_glm_captured(formula, data = stage_data, family = model_family, control = glm_control)
    }

    fit_obj <- fit_res$fit
    diagnostics[[i]] <- .react_build_stage_diagnostic(
      stage_name = stage$stage_id,
      adjusted_vars = stage$adjusted_vars,
      fit_res = fit_res,
      fit_obj = fit_obj,
      stage_data = stage_data,
      predictor = predictor,
      outcome = outcome,
      model_family = model_family
    )

    stage_meta[[i]] <- list(
      fit = fit_obj,
      data = stage_data,
      ref_level = predictor_ref,
      model = stage$model,
      stage_id = stage$stage_id,
      wide_col = stage$wide_col,
      adjustment = stage$adjustment,
      adjusted_vars = stage$adjusted_vars,
      model_family = model_family
    )

    if (is.null(fit_obj)) {
      rows_by_stage[[i]] <- .react_empty_coef_table()
      next
    }

    stage_rows <- .react_extract_predictor_rows(
      fit = fit_obj,
      predictor = predictor,
      ref_level = predictor_ref,
      include_intercept = include_intercept
    )
    if (!nrow(stage_rows)) {
      rows_by_stage[[i]] <- stage_rows
      next
    }

    stage_rows$model <- stage$model
    stage_rows$stage_id <- stage$stage_id
    stage_rows$wide_col <- stage$wide_col
    stage_rows$adjustment <- stage$adjustment
    stage_rows$adjusted_vars <- paste(stage$adjusted_vars, collapse = "+")
    stage_rows$Nobs_in_model <- nrow(stage_data)
    rows_by_stage[[i]] <- stage_rows
  }

  list(
    family = model_family,
    rows = dplyr::bind_rows(rows_by_stage),
    stages = stage_meta,
    diagnostics = dplyr::bind_rows(diagnostics)
  )
}

.react_wide_from_long <- function(long_df,
                                  effect_col,
                                  sf = 2,
                                  format = "f",
                                  simpleround = FALSE,
                                  include_nobs = FALSE) {
  if (is.null(long_df) || !nrow(long_df)) return(data.frame())

  long_df <- long_df[order(long_df$row_order, long_df$adjustment, na.last = TRUE), , drop = FALSE]
  long_df$value_formatted <- .react_format_effect_ci(
    effect = long_df[[effect_col]],
    lower = long_df$Lower,
    upper = long_df$Upper,
    sf = sf,
    format = format,
    simpleround = simpleround
  )

  wide <- tidyr::pivot_wider(
    long_df[, c("Level", "row_order", "wide_col", "value_formatted"), drop = FALSE],
    names_from = "wide_col",
    values_from = "value_formatted"
  )
  wide <- as.data.frame(wide, stringsAsFactors = FALSE)
  wide <- wide[order(wide$row_order), , drop = FALSE]
  wide$row_order <- NULL

  if (isTRUE(include_nobs)) {
    nobs_vals <- unique(long_df$Nobs_in_model)
    if (length(nobs_vals) == 1L && !is.na(nobs_vals)) {
      wide$Nobs_in_model <- nobs_vals
      wide <- wide[, c("Level", "Nobs_in_model", setdiff(names(wide), c("Level", "Nobs_in_model"))), drop = FALSE]
    }
  }

  wide
}
