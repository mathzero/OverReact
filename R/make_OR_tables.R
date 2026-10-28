
# Model maker function ----------------------------------------------------

### Main function to create a sequentially adjusted model. Depends on makeORTable
modelMakerSequential <- function(variable_name, data, sf = 2, format = "f", simpleround = FALSE,
                                 outcome = "res", ref_level = NULL,
                                 joint_adjustment_vars = c("age_group_named", "sex", "region_named",
                                                           "ethnic_new", "imd_quintile_cat"),
                                 sample_strategy = c("common_per_predictor", "per_model"),
                                 glm_control = stats::glm.control(maxit = 50)) {
  sample_strategy <- match.arg(sample_strategy)

  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!variable_name %in% names(data)) stop("Variable ", variable_name, " not found in data.")
  if (!outcome %in% names(data)) stop("Outcome ", outcome, " not found in data.")

  found_adj <- intersect(joint_adjustment_vars, names(data))
  missing_adj <- setdiff(joint_adjustment_vars, names(data))
  if (length(missing_adj)) {
    warning("Adjustment variables not found in data: ", paste(missing_adj, collapse = ", "))
  }
  stage_specs <- .react_build_sequential_stage_specs(
    adjusters = found_adj,
    predictor = variable_name,
    crude_adjustment = 0L,
    crude_wide_col = "crude_mod_OR"
  )

  runner <- .react_run_or_stages(
    data = data,
    predictor = variable_name,
    outcome = outcome,
    stage_specs = stage_specs,
    sample_strategy = sample_strategy,
    glm_control = glm_control,
    include_intercept = TRUE,
    ref_level = ref_level
  )

  plot_rows <- runner$rows
  wide_rows <- .react_wide_from_long(
    long_df = plot_rows,
    effect_col = "OR",
    sf = sf,
    format = format,
    simpleround = simpleround,
    include_nobs = FALSE,
    wide_cols = vapply(stage_specs, `[[`, character(1), "wide_col")
  )

  crude_output <- plot_rows[plot_rows$stage_id == "crude", , drop = FALSE]
  adj_outputs <- lapply(found_adj, function(adj_name) {
    plot_rows[plot_rows$stage_id == paste0("plus_", adj_name), , drop = FALSE]
  })
  names(adj_outputs) <- found_adj
  adj_outputs$crude <- crude_output

  list(
    model_df = wide_rows,
    model_df_predictorORs_only = wide_rows,
    crude_model_output = crude_output,
    adj_model_outputs = adj_outputs,
    diagnostics = runner$diagnostics
  )
}


# =============================================================================
# 1. Helper: pretty variable names --------------------------------------------
# =============================================================================
get_pretty_name <- function(var, dat, name_fun = NULL, auto_pretty = TRUE, name_map = NULL) {
  if (is.null(var) || !is.character(var) || length(var) != 1) {
    return(as.character(var))
  }
  # Explicit labels win: name_map (for example cov_name_list), then options(modelmaker.name_map).
  for (dict in list(name_map, getOption("modelmaker.name_map"))) {
    dict <- unlist(dict)
    if (!is.null(dict) && var %in% names(dict) && !is.na(dict[[var]])) {
      return(as.character(dict[[var]]))
    }
  }
  if (!is.null(dat) && var %in% names(dat)) {
    lbl <- attr(dat[[var]], "label", exact = TRUE)
    if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1) return(lbl)
  }
  if (auto_pretty) {
    var <- gsub("_", " ", var)
    var <- tools::toTitleCase(var)
  }
  if (is.function(name_fun)) {
    var <- tryCatch(name_fun(var), error = function(e) var)
  }
  var
}

# Display labels for a set of variables. Labels that collide (e.g. "age_grp" and
# "age grp" both become "Age Grp") get the raw name appended, so results stay separate.
.react_variable_labels <- function(vars, dat, name_map = NULL, name_fun = NULL, auto_pretty = TRUE) {
  labels <- vapply(vars, get_pretty_name, character(1), dat = dat, name_fun = name_fun,
                   auto_pretty = auto_pretty, name_map = name_map, USE.NAMES = FALSE)
  clash <- labels %in% labels[duplicated(labels)]
  labels[clash] <- paste0(labels[clash], " (", vars[clash], ")")
  labels
}

# Run a batch of sequentially-adjusted univariable models (optionally in parallel)
#
# @param dat A data.frame containing the analysis dataset.
# @param list_of_variables_of_interest Character vector of predictor names to iterate over.
# @param outcome Name of the outcome column. Default \code{"res"}.
# @param sf Decimal places shown in the formatted OR/Beta strings (passed to \code{specifyDecimal}).
# @param format Numeric format string passed to \code{specifyDecimal}.
# @param simpleround Logical; hand-off to \code{specifyDecimal}.
# @param joint_adjustment_vars Character vector of covariate names that are added one-by-one.
# @param cov_name_list Optional named vector or list translating raw predictor names to labels.
# @param remove_intercept_from_results Logical; drop the intercept rows?  Default \code{TRUE}.
# @param ncores Integer. If \code{NULL} (default) or \code{<2} the function runs sequentially;
#   otherwise models run in parallel on up to this many cores.
#
# @return A list with \code{df_output} (formatted estimates and CIs, one row per factor
#   level), \code{plot_output} (numeric estimates for forest plots) and \code{diagnostics}.
ModelMakerMulti <- function(dat,
                            list_of_variables_of_interest,
                            outcome                     = "res",
                            sf                          = 2,
                            format                      = "f",
                            simpleround                 = FALSE,
                            joint_adjustment_vars       = c("age_group_named","sex",
                                                            "region_named","ethnic_new",
                                                            "imd_quintile_cat"),
                            cov_name_list               = NULL,
                            name_fun          = NULL,
                            auto_pretty       = TRUE,
                            remove_intercept_from_results = TRUE,
                            ncores                      = NULL,
                            sample_strategy            = c("common_per_predictor", "per_model"),
                            glm_control                = stats::glm.control(maxit = 50)) {
  sample_strategy <- match.arg(sample_strategy)

  if (!is.data.frame(dat)) stop("dat must be a data.frame")
  if (!outcome %in% names(dat)) stop("Outcome ", outcome, " not found in dat.")

  missing_vars <- setdiff(list_of_variables_of_interest, names(dat))
  if (length(missing_vars)) {
    warning("Variables not found in data and skipped: ", paste(missing_vars, collapse = ", "))
    list_of_variables_of_interest <- setdiff(list_of_variables_of_interest, missing_vars)
  }
  list_of_variables_of_interest <- unique(list_of_variables_of_interest)
  if (!length(list_of_variables_of_interest)) stop("No variables available to analyse.")

  found_adj <- intersect(joint_adjustment_vars, names(dat))
  missing_adj <- setdiff(joint_adjustment_vars, names(dat))
  if (length(missing_adj)) {
    warning("Adjustment variables not found in data: ", paste(missing_adj, collapse = ", "))
  }

  family <- .react_detect_outcome_family(dat[[outcome]], outcome = outcome)
  message(
    if (identical(family, "binomial")) "Assuming binomial outcome (logit GLM)."
    else "Assuming gaussian outcome (identity GLM)."
  )

  single_var_runner <- function(pred_name) {
    stage_specs <- .react_build_sequential_stage_specs(
      adjusters = found_adj,
      predictor = pred_name,
      crude_adjustment = 0L,
      crude_wide_col = "crude_mod_OR"
    )

    runner <- .react_run_or_stages(
      data = dat,
      predictor = pred_name,
      outcome = outcome,
      stage_specs = stage_specs,
      sample_strategy = sample_strategy,
      glm_control = glm_control,
      include_intercept = TRUE
    )

    plot_rows <- runner$rows
    if (isTRUE(remove_intercept_from_results) && nrow(plot_rows)) {
      plot_rows <- plot_rows[!plot_rows$is_intercept, , drop = FALSE]
    }

    list(
      tidy_res = .react_wide_from_long(
        long_df = plot_rows,
        effect_col = "OR",
        sf = sf,
        format = format,
        simpleround = simpleround,
        include_nobs = FALSE,
        wide_cols = vapply(stage_specs, `[[`, character(1), "wide_col")
      ),
      tidy_plot = plot_rows,
      diagnostics = runner$diagnostics
    )
  }

  if (is.null(ncores) || ncores < 2L) {
    message("Running sequentially ...")
    results <- lapply(list_of_variables_of_interest, single_var_runner)
  } else {
    workers <- min(as.integer(ncores), future::availableCores())
    message(sprintf("Running in parallel on %d cores ...", workers))
    old_plan <- future::plan()
    on.exit(try(future::plan(old_plan), silent = TRUE), add = TRUE)
    backend <- if (isTRUE(tryCatch(future::supportsMulticore(), error = function(e) FALSE))) {
      future::multicore
    } else {
      future::multisession
    }
    future::plan(backend, workers = workers)
    results <- future.apply::future_lapply(
      list_of_variables_of_interest,
      single_var_runner,
      future.seed = TRUE
    )
  }

  pretty_names <- .react_variable_labels(list_of_variables_of_interest, dat,
                                         name_map = cov_name_list, name_fun = name_fun,
                                         auto_pretty = auto_pretty)

  res_list <- vector("list", length(results))
  plot_list <- vector("list", length(results))
  diag_list <- vector("list", length(results))

  for (i in seq_along(results)) {
    res_df <- results[[i]]$tidy_res
    if (nrow(res_df)) {
      res_df$Variable <- pretty_names[[i]]
      res_df <- res_df[, c("Variable", setdiff(names(res_df), "Variable")), drop = FALSE]
    }
    res_list[[i]] <- res_df

    plot_df <- results[[i]]$tidy_plot
    if (nrow(plot_df)) plot_df$Variable <- pretty_names[[i]]
    plot_list[[i]] <- plot_df

    diag_df <- results[[i]]$diagnostics
    if (nrow(diag_df)) diag_df$Variable <- pretty_names[[i]]
    diag_list[[i]] <- diag_df
  }

  out_df <- dplyr::bind_rows(res_list)
  out_plot <- dplyr::bind_rows(plot_list)
  out_diag <- dplyr::bind_rows(diag_list)

  if (nrow(out_df) && "Level" %in% names(out_df)) out_df <- dplyr::rename(out_df, Category = Level)
  if (nrow(out_plot) && "Level" %in% names(out_plot)) out_plot <- dplyr::rename(out_plot, Category = Level)

  if (identical(family, "gaussian")) {
    if (nrow(out_plot) && "OR" %in% names(out_plot)) out_plot <- dplyr::rename(out_plot, Beta = OR)
    if (nrow(out_df) && "crude_mod_OR" %in% names(out_df)) out_df <- dplyr::rename(out_df, crude_mod_Beta = crude_mod_OR)
  }

  list(
    df_output = out_df,
    plot_output = out_plot,
    diagnostics = out_diag
  )
}


# GAM model maker function ------------------------------------------------

# Sequentially adjusted binomial GAMs. spline_vars indexes the adjusters to smooth.
GAMModelMaker <- function(variable_name, data,
                          outcome = "res", ref_level = NULL, sf = 2, format = "f",
                          spline_vars = NULL,
                          joint_adjustment_vars = c("days_since_first_vaccine", "covida", "age")) {
  vars <- c(variable_name, joint_adjustment_vars)
  missing_vars <- setdiff(c(outcome, vars), names(data))
  if (length(missing_vars)) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  classes <- vapply(data[, vars, drop = FALSE], function(x) class(x)[1], character(1))
  if ("character" %in% classes) {
    message("These variables are character class. Consider converting to factor: ",
            paste(vars[classes == "character"], collapse = ", "))
  }
  ### gams will fail if there is too little data, so this is a quick failsafe.
  counts <- table(data[[variable_name]])
  if (sum(counts) <= length(counts) + 30) {
    warning("May not be enough data to run this model")
    return(NULL)
  }

  # Fit against the labelled reference level, not just the factor's first level.
  if (is.character(data[[variable_name]])) data[[variable_name]] <- factor(data[[variable_name]])
  if (is.factor(data[[variable_name]])) {
    if (is.null(ref_level) || !ref_level %in% levels(data[[variable_name]])) {
      ref_level <- levels(droplevels(data[[variable_name]]))[1]
    }
    data[[variable_name]] <- stats::relevel(factor(data[[variable_name]], ordered = FALSE), ref = ref_level)
  } else {
    ref_level <- NULL
  }

  fit_gam <- function(rhs) {
    mgcv::gam(stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + "))),
              data = data, family = stats::binomial(link = "logit"))
  }
  format_estimates <- function(tab) {
    .react_format_effect_ci(tab$OR, tab$Lower, tab$Upper, sf = sf, format = format)
  }

  tab_univ <- makeORTable(fit_gam(variable_name), ref_level = ref_level)
  tab_univ$model <- "Crude"

  # Smooth the requested adjusters, keeping the order in which adjusters are added.
  splined <- seq_along(joint_adjustment_vars) %in% spline_vars
  adj_terms <- joint_adjustment_vars
  adj_terms[splined] <- paste0("s(as.numeric(", joint_adjustment_vars[splined], "))")

  mod.results.list <- list()
  for (i in seq_along(joint_adjustment_vars)) {
    tab <- makeORTable(fit_gam(unique(c(variable_name, adj_terms[seq_len(i)]))), ref_level = ref_level)
    tab$model <- paste0(if (splined[i]) "+ spline:" else "+", joint_adjustment_vars[[i]])
    mod.results.list[[i]] <- tab
  }

  ### Now we add all the models together into one big DF. Predictor rows come first in
  ### every model, so adjusted rows extend the crude rows.
  full_tab <- if (length(mod.results.list)) mod.results.list[[length(mod.results.list)]] else tab_univ
  df.output <- data.frame(Variable = variable_name, Category = full_tab$Level,
                          stringsAsFactors = FALSE)
  pad <- function(x) c(x, rep(NA_character_, nrow(df.output) - length(x)))
  df.output$crude_mod_OR <- pad(format_estimates(tab_univ))
  for (i in seq_along(joint_adjustment_vars)) {
    col <- paste0("plus_", if (splined[i]) "spline:" else "", joint_adjustment_vars[[i]])
    df.output[[col]] <- pad(format_estimates(mod.results.list[[i]]))
  }
  tab_univ$Variable <- variable_name
  names(tab_univ)[names(tab_univ) == "Level"] <- "Category"

  list(model_df = df.output,
       model_df_predictorORs_only = df.output[stats::complete.cases(df.output), , drop = FALSE],
       crude_model_output = tab_univ,
       adj_model_outputs = mod.results.list)
}
