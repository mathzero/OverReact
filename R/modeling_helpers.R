# Shared internal helpers for the automated modelling pipeline.

.react_escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

.react_quote_name <- function(x) {
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", x)) return(x)
  paste0("`", gsub("`", "\\\\`", x, fixed = TRUE), "`")
}

# Formula from term names; `offset` names a column of exposure times entered as
# offset(log(offset)), as Poisson rate models need.
.react_reformulate_safe <- function(terms, response = NULL, offset = NULL) {
  terms <- unique(terms[nzchar(terms)])
  rhs_terms <- c(vapply(terms, .react_quote_name, character(1)),
                 if (!is.null(offset)) paste0("offset(log(", .react_quote_name(offset), "))"))
  rhs <- if (length(rhs_terms)) paste(rhs_terms, collapse = " + ") else "1"
  if (is.null(response)) {
    stats::as.formula(paste("~", rhs))
  } else {
    stats::as.formula(paste(.react_quote_name(response), "~", rhs))
  }
}

.react_prepare_analysis_frame <- function(data, vars, drop_missing = TRUE) {
  out <- as.data.frame(data[, vars, drop = FALSE], stringsAsFactors = FALSE)
  for (nm in names(out)) {
    if (is.character(out[[nm]])) out[[nm]] <- factor(out[[nm]])
    # Ordered factors get treatment contrasts like other factors, so tables report one
    # estimate per level against the reference rather than polynomial trend terms.
    if (is.ordered(out[[nm]])) out[[nm]] <- factor(out[[nm]], ordered = FALSE)
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

.react_build_sequential_stage_specs <- function(adjusters,
                                                predictor,
                                                crude_adjustment = 0L,
                                                crude_wide_col = "crude_mod_OR") {
  stage_specs <- vector("list", length(adjusters) + 1L)
  stage_specs[[1]] <- list(
    stage_id = "crude",
    adjusted_vars = character(0),
    model = "Crude",
    wide_col = crude_wide_col,
    adjustment = crude_adjustment
  )

  if (!length(adjusters)) {
    return(stage_specs)
  }

  for (i in seq_along(adjusters)) {
    cumulative_adjusters <- adjusters[seq_len(i)]
    cumulative_adjusters <- cumulative_adjusters[cumulative_adjusters != predictor]

    stage_specs[[i + 1L]] <- list(
      stage_id = paste0("plus_", adjusters[[i]]),
      adjusted_vars = cumulative_adjusters,
      model = paste0("+", adjusters[[i]]),
      wide_col = paste0("plus_", adjusters[[i]]),
      adjustment = crude_adjustment + i
    )
  }

  stage_specs
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
    measure = character(),
    stringsAsFactors = FALSE
  )
}

# Coefficient table for a fitted model. Estimates are exponentiated for ratio measures
# (OR, RR, IRR); `measure` overrides the one inferred from the model's family and link.
# Multinomial models add an outcome_level column, with one block of rows per level.
.react_build_coef_table <- function(mod, ref_level = NULL, dp = 3, measure = NULL) {
  if (is.null(mod) || inherits(mod, "try-error")) {
    return(.react_empty_coef_table())
  }

  model_family <- tryCatch(mod$family$family, error = function(e) NULL)
  if (is.null(model_family) || !length(model_family)) {
    model_family <- if (inherits(mod, "polr")) "ordinal" else if (inherits(mod, "multinom")) {
      "multinomial"
    } else if (inherits(mod, "lm")) "gaussian" else NA_character_
  }
  if (is.null(measure)) measure <- .react_model_measure(mod)
  exponentiate <- measure %in% .react_ratio_measures

  wald_table <- function(term, est, se, crit = stats::qnorm(0.975),
                         p = 2 * stats::pnorm(-abs(est / se))) {
    data.frame(term_raw = term, OR = as.numeric(est), Lower = as.numeric(est - crit * se),
               Upper = as.numeric(est + crit * se), P_value = as.numeric(p),
               stringsAsFactors = FALSE)
  }
  # Intervals use t quantiles when the model reports t tests (estimated dispersion, as in
  # Gaussian models), so they agree with the p-values; otherwise normal quantiles.
  critical_value <- function(p_col) {
    if (grepl("|t|", p_col, fixed = TRUE)) stats::qt(0.975, df = stats::df.residual(mod)) else stats::qnorm(0.975)
  }

  coef_tab <- tryCatch({
    if (inherits(mod, "gam")) {
      summ <- as.data.frame(mgcv::summary.gam(mod)$p.table, stringsAsFactors = FALSE)
      p_col <- grep("^Pr\\(", names(summ), value = TRUE)[1]
      wald_table(rownames(summ), summ$Estimate, summ$`Std. Error`,
                 crit = critical_value(p_col), p = summ[[p_col]])
    } else if (inherits(mod, "polr")) {
      # Proportional-odds coefficients; the cut-points are not reported.
      summ <- summary(mod)$coefficients[names(stats::coef(mod)), , drop = FALSE]
      wald_table(rownames(summ), summ[, "Value"], summ[, "Std. Error"])
    } else if (inherits(mod, "multinom")) {
      est <- stats::coef(mod)
      se <- summary(mod)$standard.errors
      if (is.null(dim(est))) {
        est <- matrix(est, nrow = 1, dimnames = list(mod$lev[2], names(est)))
        se <- matrix(se, nrow = 1, dimnames = dimnames(est))
      }
      do.call(rbind, lapply(rownames(est), function(k) {
        tab <- wald_table(colnames(est), est[k, ], se[k, ])
        tab$outcome_level <- k
        tab
      }))
    } else if (!is.null(mod$robust_vcov)) {
      # Robust (sandwich) standard errors, as in modified Poisson models.
      est <- stats::coef(mod)
      est <- est[!is.na(est)]
      wald_table(names(est), est, sqrt(diag(mod$robust_vcov))[names(est)])
    } else {
      coef_summ <- summary(mod)$coefficients
      se_col <- grep("Std\\.|Std\\. Error|Standard Error", colnames(coef_summ), ignore.case = TRUE)[1]
      p_col <- grep("Pr\\(", colnames(coef_summ))[1]
      if (is.na(se_col) || is.na(p_col)) stop("Could not locate coefficient summary columns.")
      wald_table(rownames(coef_summ), coef_summ[, 1], coef_summ[, se_col],
                 crit = critical_value(colnames(coef_summ)[p_col]), p = coef_summ[, p_col])
    }
  }, error = function(e) NULL)

  if (is.null(coef_tab) || !nrow(coef_tab)) {
    return(.react_empty_coef_table())
  }

  if (exponentiate) {
    coef_tab$OR <- exp(coef_tab$OR)
    coef_tab$Lower <- exp(coef_tab$Lower)
    coef_tab$Upper <- exp(coef_tab$Upper)
  }

  # dp = NULL keeps full precision (per-unit effects of wide-range predictors would
  # otherwise round to 1). P-values are never rounded, so tiny values do not become 0.
  if (!is.null(dp)) {
    coef_tab$OR <- round(coef_tab$OR, dp)
    coef_tab$Lower <- round(coef_tab$Lower, dp)
    coef_tab$Upper <- round(coef_tab$Upper, dp)
  }
  coef_tab$Level <- coef_tab$term_raw
  coef_tab$is_reference <- FALSE
  coef_tab$is_intercept <- coef_tab$term_raw == "(Intercept)"
  coef_tab$model_family <- model_family
  coef_tab$measure <- measure
  cols <- c("Level", "term_raw", "OR", "Lower", "Upper", "P_value", "is_reference",
            "is_intercept", "model_family", "measure")
  if ("outcome_level" %in% names(coef_tab)) cols <- c("outcome_level", cols)
  coef_tab <- coef_tab[, cols, drop = FALSE]

  if (!is.null(ref_level)) {
    insert_reference <- function(tab) {
      ref_row <- tab[1, , drop = FALSE]
      ref_row$Level <- paste0(ref_level, " [reference]")
      ref_row$term_raw <- NA_character_
      ref_row[c("OR", "Lower", "Upper", "P_value")] <- NA_real_
      ref_row$is_reference <- TRUE
      ref_row$is_intercept <- FALSE
      int_idx <- which(tab$is_intercept)[1]
      if (is.na(int_idx)) return(rbind(ref_row, tab))
      rbind(tab[seq_len(int_idx), , drop = FALSE], ref_row, tab[-seq_len(int_idx), , drop = FALSE])
    }
    coef_tab <- if ("outcome_level" %in% names(coef_tab)) {
      blocks <- split(coef_tab, factor(coef_tab$outcome_level, levels = unique(coef_tab$outcome_level)))
      do.call(rbind, lapply(blocks, insert_reference))
    } else {
      insert_reference(coef_tab)
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

.react_extract_predictor_rows <- function(fit, predictor, ref_level = NULL, include_intercept = TRUE,
                                          level_order = NULL, measure = NULL) {
  coef_tab <- .react_build_coef_table(fit, ref_level = NULL, dp = NULL, measure = measure)
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
  if (is_categorical) {
    predictor_ref <- .react_reference_level(model_frame[[predictor]], ref_level = ref_level)
    # level_order is the predictor's full level set, so a level absent from this model
    # does not shift the rows of the levels after it.
    lvls <- if (is.null(level_order)) levels(model_frame[[predictor]]) else level_order
    term_label <- if (!is.null(term_labels) && !is.na(pred_term_id)) term_labels[[pred_term_id]] else predictor
    term_pattern <- paste0("^", .react_escape_regex(term_label))
  }

  # Multinomial models have one block of rows per non-reference outcome level; offsetting
  # row_order keeps each block together.
  blocks <- if ("outcome_level" %in% names(coef_tab)) unique(coef_tab$outcome_level) else NA_character_
  out_parts <- list()
  for (b in seq_along(blocks)) {
    in_block <- if (is.na(blocks[b])) rep(TRUE, nrow(coef_tab)) else coef_tab$outcome_level == blocks[b]
    order_offset <- (b - 1L) * 100000L

    if (isTRUE(include_intercept) && any(coef_tab$is_intercept & in_block)) {
      intercept_row <- coef_tab[coef_tab$is_intercept & in_block, , drop = FALSE]
      intercept_row$Level <- "(Intercept)"
      intercept_row$row_order <- order_offset
      out_parts[[length(out_parts) + 1L]] <- intercept_row
    }

    predictor_tab <- coef_tab[predictor_rows & in_block, , drop = FALSE]

    if (is_categorical) {
      ref_row <- coef_tab[which(in_block)[1], , drop = FALSE]
      ref_row$Level <- paste0(predictor_ref, " [reference]")
      ref_row$term_raw <- NA_character_
      ref_row[c("OR", "Lower", "Upper", "P_value")] <- NA_real_
      ref_row$is_reference <- TRUE
      ref_row$is_intercept <- FALSE
      ref_row$row_order <- order_offset + 1L
      out_parts[[length(out_parts) + 1L]] <- ref_row

      predictor_tab$Level <- sub(term_pattern, "", predictor_tab$term_raw)
      predictor_tab$Level[predictor_tab$Level == ""] <- predictor_tab$term_raw[predictor_tab$Level == ""]
      predictor_tab$row_order <- order_offset + match(predictor_tab$Level, setdiff(lvls, predictor_ref)) + 1L
    } else if (nrow(predictor_tab) == 1L) {
      predictor_tab$Level <- predictor
      predictor_tab$row_order <- order_offset + 1L
    } else if (nrow(predictor_tab)) {
      predictor_tab$Level <- predictor_tab$term_raw
      predictor_tab$row_order <- order_offset + seq_len(nrow(predictor_tab)) +
        if (isTRUE(include_intercept)) 0L else 1L
    }

    if (nrow(predictor_tab)) {
      predictor_tab$is_reference <- FALSE
      predictor_tab$is_intercept <- FALSE
      out_parts[[length(out_parts) + 1L]] <- predictor_tab
    }
  }

  if (!length(out_parts)) return(.react_empty_coef_table())

  out <- dplyr::bind_rows(out_parts)
  if (!"row_order" %in% names(out)) out$row_order <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

.react_format_effect_ci <- function(effect, lower, upper, sf = 2, format = "f", simpleround = FALSE) {
  out <- sprintf(
    "%s (%s,%s)",
    specifyDecimal(effect, sf, format = format, simpleround = simpleround),
    specifyDecimal(lower, sf, format = format, simpleround = simpleround),
    specifyDecimal(upper, sf, format = format, simpleround = simpleround)
  )
  # Reference rows and failed fits have no estimate.
  out[is.na(effect)] <- "-"
  out
}

# Marginal risk differences (measure = "RD") or risk ratios ("RR") for one stage's model,
# in the long layout of the coefficient rows. Only binary-outcome GLMs qualify.
.react_extract_rd_rows <- function(stage_meta, predictor, n_sim = 100, dp = 3,
                                   ci_method = "delta", measure = "RD") {
  if (is.null(stage_meta$fit) || !inherits(stage_meta$fit, "glm")) return(data.frame())
  if (!isTRUE(stage_meta$binary)) return(data.frame())

  rd_tab <- .react_safe_marginal_table(
    mod = stage_meta$fit,
    variable_name = predictor,
    measure = measure,
    ref_level = stage_meta$ref_level,
    dp = dp,
    data = stage_meta$data,
    n_sim = n_sim,
    ci_method = ci_method
  )
  if (is.null(rd_tab) || !nrow(rd_tab)) return(data.frame())

  rd_tab <- as.data.frame(rd_tab, stringsAsFactors = FALSE)
  rownames(rd_tab) <- NULL
  is_categorical <- predictor %in% names(stage_meta$data) &&
    is.factor(stage_meta$data[[predictor]]) &&
    !is.ordered(stage_meta$data[[predictor]])

  if (is_categorical) {
    predictor_ref <- .react_reference_level(stage_meta$data[[predictor]], ref_level = stage_meta$ref_level)
    lvls <- if (is.null(stage_meta$levels)) levels(stage_meta$data[[predictor]]) else stage_meta$levels
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
  rd_tab$wide_col <- sub("_OR$", paste0("_", measure), stage_meta$wide_col)
  rd_tab$adjustment <- stage_meta$adjustment
  rd_tab$adjusted_vars <- paste(stage_meta$adjusted_vars, collapse = "+")
  rd_tab$Nobs_in_model <- nrow(stage_meta$data)

  rd_tab[, c("Level", "term_raw", measure, "Lower", "Upper", "P_value",
             "is_reference", "is_intercept", "model_family", "row_order",
             "model", "stage_id", "wide_col", "adjustment",
             "adjusted_vars", "Nobs_in_model"), drop = FALSE]
}

.react_build_stage_diagnostic <- function(stage_name, adjusted_vars, fit_res, fit_obj,
                                          stage_data, predictor, outcome, model_family) {
  vcov_ok <- FALSE
  if (!is.null(fit_obj)) {
    vcov_ok <- tryCatch({
      vc <- .react_vcov(fit_obj)
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
                                 ref_level = NULL,
                                 family = "auto",
                                 offset = NULL) {
  sample_strategy <- match.arg(sample_strategy)
  spec <- .react_resolve_family(data[[outcome]], family = family, outcome = outcome)
  .react_check_offset(offset, spec, data)
  data[[outcome]] <- .react_code_outcome(data[[outcome]], spec)

  used_adjusters <- unique(unlist(lapply(stage_specs, `[[`, "adjusted_vars"), use.names = FALSE))
  common_vars <- unique(c(outcome, predictor, used_adjusters, offset))

  common_data <- NULL
  if (identical(sample_strategy, "common_per_predictor")) {
    common_data <- .react_prepare_analysis_frame(data, common_vars, drop_missing = TRUE)
    if (!nrow(common_data)) {
      stop("No complete cases available for predictor '", predictor, "'.")
    }
  }

  # One reference level and level order per predictor, taken from its largest analysis
  # sample. Stages are relevelled to it so they all report the same contrasts; a per-model
  # sample that has lost the reference level is reported as a failed stage instead of
  # silently switching to another reference.
  predictor_frame <- if (is.null(common_data)) {
    .react_prepare_analysis_frame(data, unique(c(outcome, predictor)), drop_missing = TRUE)
  } else {
    common_data
  }
  predictor_ref <- .react_reference_level(predictor_frame[[predictor]], ref_level = ref_level)
  predictor_levels <- if (is.null(predictor_ref)) NULL else levels(predictor_frame[[predictor]])

  rows_by_stage <- vector("list", length(stage_specs))
  stage_meta <- vector("list", length(stage_specs))
  diagnostics <- vector("list", length(stage_specs))

  names(rows_by_stage) <- vapply(stage_specs, `[[`, character(1), "stage_id")
  names(stage_meta) <- names(rows_by_stage)
  names(diagnostics) <- names(rows_by_stage)

  for (i in seq_along(stage_specs)) {
    stage <- stage_specs[[i]]
    stage_vars <- unique(c(outcome, predictor, stage$adjusted_vars, offset))
    stage_data <- if (identical(sample_strategy, "common_per_predictor")) {
      common_data[, stage_vars, drop = FALSE]
    } else {
      .react_prepare_analysis_frame(data, stage_vars, drop_missing = TRUE)
    }

    fit_res <- list(fit = NULL, warnings = character(), error = "No complete cases available.")
    if (nrow(stage_data)) {
      if (!is.null(predictor_ref) && !predictor_ref %in% levels(stage_data[[predictor]])) {
        fit_res$error <- paste0("Reference level '", predictor_ref, "' of '", predictor,
                                "' has no observations in this stage's sample.")
      } else {
        if (!is.null(predictor_ref)) {
          stage_data[[predictor]] <- stats::relevel(stage_data[[predictor]], ref = predictor_ref)
        }
        formula <- .react_reformulate_safe(c(predictor, stage$adjusted_vars), response = outcome,
                                           offset = offset)
        fit_res <- .react_fit_model_captured(formula, data = stage_data, spec = spec, control = glm_control)
      }
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
      model_family = spec$name
    )

    stage_meta[[i]] <- list(
      fit = fit_obj,
      data = stage_data,
      ref_level = predictor_ref,
      levels = predictor_levels,
      model = stage$model,
      stage_id = stage$stage_id,
      wide_col = stage$wide_col,
      adjustment = stage$adjustment,
      adjusted_vars = stage$adjusted_vars,
      model_family = spec$name,
      binary = spec$binary,
      measure = spec$measure
    )

    if (is.null(fit_obj)) {
      rows_by_stage[[i]] <- .react_empty_coef_table()
      next
    }

    stage_rows <- .react_extract_predictor_rows(
      fit = fit_obj,
      predictor = predictor,
      ref_level = predictor_ref,
      include_intercept = include_intercept,
      level_order = predictor_levels,
      measure = spec$measure
    )
    if (!nrow(stage_rows)) {
      rows_by_stage[[i]] <- stage_rows
      next
    }

    stage_rows$model_family <- spec$name
    stage_rows$model <- stage$model
    stage_rows$stage_id <- stage$stage_id
    stage_rows$wide_col <- stage$wide_col
    stage_rows$adjustment <- stage$adjustment
    stage_rows$adjusted_vars <- paste(stage$adjusted_vars, collapse = "+")
    stage_rows$Nobs_in_model <- nrow(stage_data)
    rows_by_stage[[i]] <- stage_rows
  }

  list(
    family = spec$name,
    spec = spec,
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
                                  include_nobs = FALSE,
                                  wide_cols = NULL) {
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

  # Multinomial rows are also identified by their outcome level.
  id_cols <- c(intersect("outcome_level", names(long_df)), "Level")
  wide <- tidyr::pivot_wider(
    long_df[, c(id_cols, "row_order", "wide_col", "value_formatted"), drop = FALSE],
    names_from = "wide_col",
    values_from = "value_formatted"
  )
  wide <- as.data.frame(wide, stringsAsFactors = FALSE)
  wide <- wide[order(wide$row_order), , drop = FALSE]
  wide$row_order <- NULL

  # Keep a column for every stage, in stage order, even when a stage produced no
  # estimates (for example a failed fit), so tables have a stable layout.
  if (!is.null(wide_cols)) {
    wide_cols <- unique(wide_cols)
    for (col in setdiff(wide_cols, names(wide))) wide[[col]] <- NA_character_
    wide <- wide[, c(id_cols, wide_cols, setdiff(names(wide), c(id_cols, wide_cols))), drop = FALSE]
  }

  if (isTRUE(include_nobs)) {
    nobs_vals <- unique(long_df$Nobs_in_model)
    if (length(nobs_vals) == 1L && !is.na(nobs_vals)) {
      wide$Nobs_in_model <- nobs_vals
      wide <- wide[, c(id_cols, "Nobs_in_model", setdiff(names(wide), c(id_cols, "Nobs_in_model"))), drop = FALSE]
    }
  }

  wide
}
