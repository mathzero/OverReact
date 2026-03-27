
# @import dplyr
# @import stats
# @import mgcv
# @import progress
#
# @param dat data to be modelled
# @param dfRes data to be modelled
# @param datatype Specify the data type (from 'continuous', 'binary', or 'date' )
# @param negval If data is binary, what is the negative value in the uncleaned data?
# @param posval If data is binary, what is the positive value in the uncleaned data?
# @param floornumber If data is continuous, what is the lowest 'real' value in the uncleaned data?
# @param dateFormat If data is date, what is the required output date format?


# Model maker function ----------------------------------------------------




### Main function to create a sequentially adjusted model. Depends on makeORTable
modelMakerSequential <- function(variable_name, data = dfRes, sf = 2, format = "f", simpleround = FALSE,
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
  found_adj <- setdiff(found_adj, variable_name)

  stage_specs <- vector("list", length(found_adj) + 1L)
  stage_specs[[1]] <- list(
    stage_id = "crude",
    adjusted_vars = character(0),
    model = "Crude",
    wide_col = "crude_mod_OR",
    adjustment = 1L
  )
  if (length(found_adj)) {
    for (i in seq_along(found_adj)) {
      stage_specs[[i + 1L]] <- list(
        stage_id = paste0("plus_", found_adj[[i]]),
        adjusted_vars = found_adj[seq_len(i)],
        model = paste0("+", found_adj[[i]]),
        wide_col = paste0("plus_", found_adj[[i]]),
        adjustment = i + 1L
      )
    }
  }

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
    include_nobs = FALSE
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
get_pretty_name <- function(var, dat, name_fun = NULL, auto_pretty = TRUE) {
  if (is.null(var) || !is.character(var) || length(var) != 1) {
    return(as.character(var))
  }
  dict <- getOption("modelmaker.name_map")
  if (!is.null(dict) && var %in% names(dict)) return(dict[[var]])
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

##  Imports - add these to your R/zzz.R or NAMESPACE (roxygen style)
# @import dplyr
# @import stats
# @import mgcv
# @import progress
# @import parallel      # <-- NEW
# @import doParallel    # <-- NEW (needed only for worker registration helpers)
# @import foreach       # <-- NEW (pulled in by doParallel)
#
# Run a batch of sequentially-adjusted univariable models (optionally in parallel)
#
# @param dat A data.frame containing the analysis dataset. Defaults to \code{dfRes}.
# @param list_of_variables_of_interest Character vector of predictor names to iterate over.
# @param outcome Name of the outcome column. Default \code{"res"}.
# @param sf Significant figures to show in the formatted OR/Beta strings (passed to \code{specifyDecimal}).
# @param format Numeric format string passed to \code{specifyDecimal}.
# @param simpleround Logical; hand-off to \code{specifyDecimal}.
# @param joint_adjustment_vars Character vector of covariate names that are added one-by-one.
# @param cov_name_list Optional named vector translating raw predictor names to pretty labels.
# @param remove_intercept_from_results Logical; drop the intercept rows?  Default \code{TRUE}.
# @param ncores Integer. \strong{NEW}.  If \code{NULL} (default) or \code{<2} the function runs sequentially.
#   Otherwise a fork/PSOCK cluster of this size is spun up and models are run in parallel.
#
# @return A list with two elements:
#   \describe{
#     \item{\code{df_output}}{A tidy data.frame of formatted point estimates + CIs, one row per factor level.}
#     \item{\code{plot_output}}{A tidy data.frame of raw OR/Beta + CI columns suitable for forest plots.}
#   }
# @export
#
ModelMakerMulti <- function(dat                         = dfRes,
                            list_of_variables_of_interest,
                            outcome                     = "res",
                            sf                          = 2,
                            format                      = "f",
                            simpleround                 = FALSE,
                            joint_adjustment_vars       = c("age_group_named","sex",
                                                            "region_named","ethnic_new",
                                                            "imd_quintile_cat"),
                            cov_name_list               = NULL,
                            name_fun          = NULL,  # <- new
                            auto_pretty       = TRUE,  # <- new
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
    pred_adjusters <- setdiff(found_adj, pred_name)
    stage_specs <- vector("list", length(pred_adjusters) + 1L)
    stage_specs[[1]] <- list(
      stage_id = "crude",
      adjusted_vars = character(0),
      model = "Crude",
      wide_col = "crude_mod_OR",
      adjustment = 1L
    )
    if (length(pred_adjusters)) {
      for (i in seq_along(pred_adjusters)) {
        stage_specs[[i + 1L]] <- list(
          stage_id = paste0("plus_", pred_adjusters[[i]]),
          adjusted_vars = pred_adjusters[seq_len(i)],
          model = paste0("+", pred_adjusters[[i]]),
          wide_col = paste0("plus_", pred_adjusters[[i]]),
          adjustment = i + 1L
        )
      }
    }

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
        include_nobs = FALSE
      ),
      tidy_plot = plot_rows,
      diagnostics = runner$diagnostics
    )
  }

  if (is.null(ncores) || ncores < 2L) {
    message("Running sequentially ...")
    results <- lapply(list_of_variables_of_interest, single_var_runner)
  } else {
    message(sprintf("Running in parallel on %d cores ...", ncores))
    old_plan <- future::plan()
    on.exit(try(future::plan(old_plan), silent = TRUE), add = TRUE)
    backend <- if (isTRUE(tryCatch(future::supportsMulticore(), error = function(e) FALSE))) {
      future::multicore
    } else {
      future::multisession
    }
    future::plan(backend, workers = ncores)
    results <- future.apply::future_lapply(
      list_of_variables_of_interest,
      single_var_runner,
      future.seed = TRUE
    )
  }

  pretty_names <- vapply(
    list_of_variables_of_interest,
    get_pretty_name,
    FUN.VALUE = character(1),
    dat = dat,
    name_fun = name_fun,
    auto_pretty = auto_pretty
  )

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

GAMModelMaker <- function(variable_name, data=dfRes,
                          outcome="res", ref_level ="Not current cigarette smoker",sf=2,format="f",
                          spline_vars = NULL, ## this refers to which of the adjustment vars should be splined
                          joint_adjustment_vars = c("days_since_first_vaccine","covida","age")){


  classes <- lapply(data[,c(variable_name,joint_adjustment_vars)], class) %>% unlist()

  if("character" %in% classes){
    print("These variables are character class. Consider converting to factor:")
    print(c(variable_name,joint_adjustment_vars)[classes == "character"])

  }
  ### gams will fail is insufficient data to work with, so putting this in as a quick failsafe.
  ### With world enough and time, much better to replace with a trycatch

  if(sum(table(data[,variable_name])) <= length(table(data[,variable_name])) + 30){
    print("May not be enough data to run this model")
  }else{
    f <- as.formula(paste(outcome," ~", variable_name))
    univ_mod <- mgcv::gam(f, data = dat,family = binomial(link = "logit"))
    tab_univ <- makeORTable(univ_mod, ref_level = ref_level,)
    ### Add model name
    tab_univ$model <- "Crude"

    #splinify relevant rvars
    if(!is.null(spline_vars)){
      joint_adjustment_vars_spline <- paste0("s(as.numeric(",joint_adjustment_vars[spline_vars],"))")
      joint_adjustment_vars_spline <- c(joint_adjustment_vars_spline,joint_adjustment_vars[-spline_vars])
    }else{
      joint_adjustment_vars_spline <- joint_adjustment_vars
    }

    mod.results.list <- list()

    for (i in 1:length(joint_adjustment_vars)){
      paste0("Now processing additional covariate:", joint_adjustment_vars[i])

      ### adjusted for age and gender
      f <- as.formula(paste(outcome," ~", paste(unique(c(variable_name,joint_adjustment_vars_spline[1:i])),
                                                collapse = "+")))
      mod <- mgcv::gam(f, data = dat,family = binomial(link = "logit"))

      ### Create OR table
      tab <- makeORTable(mod, ref_level=ref_level)

      ### Add model name
      if(i%in%spline_vars){
        tab$model <- paste0("+ spline:", joint_adjustment_vars[[i]])
      }else{
        tab$model <- paste0("+", joint_adjustment_vars[[i]])

      }
      ### Add to list
      mod.results.list[[i]] <- tab
    }

    ### Now we add all the models together into one big DF
    df.output=data.frame(Level=mod.results.list[[length(mod.results.list)]]$Level)
    df.output$crude_mode_OR <- c(paste0(tab_univ$OR, " (",tab_univ$Lower, ",",tab_univ$Upper, ")"),
                                 rep(NA_character_, nrow(df.output)- nrow(tab_univ)))

    for (i in 1:length(joint_adjustment_vars)){
      if(i %in% spline_vars){
        joint_adjustment_vars[[i]] <- paste0("spline:",joint_adjustment_vars[[i]])
      }
      df.output[,paste0("plus_", joint_adjustment_vars[[i]])] <- c(paste0(mod.results.list[[i]]$OR, " (",mod.results.list[[i]]$Lower, ",",
                                                                          mod.results.list[[i]]$Upper, ")"),
                                                                   rep(NA_character_, nrow(df.output)- nrow(mod.results.list[[i]])))

    }
    df.output <- df.output %>% dplyr::rename(Variable = predictor,
                                             Category = Level)
    tab_univ <- tab_univ %>% dplyr::rename(Variable = predictor,
                                           Category = Level)

    df.output.abbrev <- df.output[complete.cases(df.output),]

    return(list(model_df=df.output,
                model_df_predictorORs_only = df.output.abbrev,
                crude_model_output=tab_univ,
                adj_model_outputs=mod.results.list))

  }
}

#
# ### Mini function to turn a glm model into an odds ratio table
# makeORTable <- function(mod, ref_level = NULL,dp=3){
#   mod_exp=(mod$family$family=="binomial")
#   if(class(mod)[1] == "gam"){
#     tab <- as.data.frame(summary.gam(mod)$p.table)
#     tab$Lower = tab$Estimate - 1.96* tab$`Std. Error`
#     tab$Upper = tab$Estimate + 1.96* tab$`Std. Error`
#     tab <- tab %>% dplyr::select(Estimate,Lower, Upper,`Pr(>|z|)`)
#     tab$Level <- rownames(tab)
#     tab <- tab %>% dplyr::select(Level, everything())
#     colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
#     # tab$Level <- sub("^.*?([A-Z])", "\\1",tab$Level)
#     if(!is.null(ref_level)){
#       tab[1,] <- c(paste0(ref_level," [reference]"), rep(NA_real_, ncol(tab)-1))
#     }
#     tab[,2] <- round(exp(as.numeric(tab[,2])),dp)
#     tab[,3] <- round(exp(as.numeric(tab[,3])),dp)
#     tab[,4] <- round(exp(as.numeric(tab[,4])),dp)
#     tab[,5] <- round(as.numeric(tab[,5]),5)
#   }else{
#     tab <- jtools::summ(mod, exp=mod_exp, ORs = mod_exp)
#     if(mod_exp){
#       tab <- tab$coeftable  %>% as.data.frame() %>% dplyr::select(1,2,3,5)
#     }else{
#       tab <- tab$coeftable  %>% as.data.frame() %>% dplyr::select(1,2,4)
#       tab$Lower = tab$Est.- qnorm(p = 0.975,mean = 0,sd = 1)*tab$S.E.
#       tab$Upper = tab$Est.+ qnorm(p = 0.975,mean = 0,sd = 1)*tab$S.E.
#       tab <- tab[,c(1,4,5,3)]
#     }
#     tab$Level <- rownames(tab)
#     tab <- tab %>% dplyr::select(Level, everything())
#     colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
#     # tab$Level <- sub("^.*?([A-Z])", "\\1",tab$Level)
#     if(!is.null(ref_level)){
#       tab[1,] <- c(paste0(ref_level," [reference]"), rep(NA_real_, ncol(tab)-1))
#     }
#     tab[,2] <- round(as.numeric(tab[,2]),dp)
#     tab[,3] <- round(as.numeric(tab[,3]),dp)
#     tab[,4] <- round(as.numeric(tab[,4]),dp)
#     tab[,5] <- round(as.numeric(tab[,5]),5)
#   }
#   rownames(tab) <- 1:nrow(tab)
#   return(tab)
# }
