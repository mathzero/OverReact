
#' @import dplyr
#' @import stats
#' @import mgcv
#' @import progress
#' @import car

#' @param dat data to be modelled
#' @param dfRes data to be modelled
#' @param datatype Specify the data type (from 'continuous', 'binary', or 'date' )
#' @param negval If data is binary, what is the negative value in the uncleaned data?
#' @param posval If data is binary, what is the positive value in the uncleaned data?
#' @param floornumber If data is continuous, what is the lowest 'real' value in the uncleaned data?
#' @param dateFormat If data is date, what is the required output date format?

# ─────────────────────────────────────────────────────────────
# helper: tidy VIF table with *numeric* adjustment column
# ─────────────────────────────────────────────────────────────
#' Compute a tidy VIF table
#'
#' @param fit      A fitted glm/lm that car::vif() understands
#' @param step_lbl Character label for readability ("Crude", "+age_group", …)
#' @param adj_no   Integer: how many covariates have been added (0 = crude)
#'
#' @return data.frame with columns <model>, <adjustment>, <term>, <vif>
get_vif_df <- function(fit, step_lbl, adj_no) {

  tl <- attr(terms(fit), "term.labels")

  ## single‑predictor models → VIF is identically 1
  if (length(tl) < 2L) {
    return(data.frame(model      = step_lbl,
                      adjustment = as.integer(adj_no),
                      term       = tl,
                      vif        = 1,
                      row.names  = NULL,
                      stringsAsFactors = FALSE))
  }

  vf <- car::vif(fit)
  if (is.matrix(vf)) vf <- diag(vf)     # factors → matrix

  data.frame(model      = step_lbl,
             adjustment = as.integer(adj_no),  # <‑‑ NUMERIC
             term       = rownames(vf),
             vif        = as.numeric(vf),
             row.names  = NULL,
             stringsAsFactors = FALSE)
}




# ──────────────────────────────────────────────────────────────────────────────
# Helper that decides how to spell each variable in the output ----
# ──────────────────────────────────────────────────────────────────────────────

get_pretty_name <- function(var, dat,
                            name_fun = NULL,           # <- optional user function
                            auto_pretty = TRUE) {      # <- toggle for step 3 above

  ## (i)  Session-wide dictionary in options()
  dict <- getOption("modelmaker.name_map")
  if (!is.null(dict) && var %in% names(dict)) return(dict[[var]])

  ## (ii)  Label attribute carried by haven / Hmisc / readstat, etc.
  lbl <- attr(dat[[var]], "label", exact = TRUE)
  if (!is.null(lbl)) return(lbl)

  ## (iii) Automatic prettifier
  if (auto_pretty) {
    var <- gsub("_", " ", var)
    var <- tools::toTitleCase(var)
  }

  ## (iv)  Final chance: a user-supplied function
  if (is.function(name_fun)) var <- name_fun(var)

  var
}


# Model maker function ----------------------------------------------------



# ──────────────────────────────────────────────────────────────────────────────
# SEQUENTIAL MODELLER (now returns an out_vif element)
# ──────────────────────────────────────────────────────────────────────────────

modelMakerSequential <- function(variable_name, data = dfRes, sf = 2,
                                 format = "f", simpleround = FALSE,
                                 outcome = "res", ref_level = NULL,
                                 joint_adjustment_vars =
                                   c("age_group_named","sex","region_named",
                                     "ethnic_new","imd_quintile_cat")) {

  num_y <- length(unique(dplyr::pull(data, outcome)))
  family <- if (num_y == 2L) "binomial" else "gaussian"

  #──── 1. crude model ────#
  f0              <- as.formula(paste(outcome, "~", variable_name))
  crude_mod       <- try(glm(f0, data = data, family = family), silent = TRUE)

  if (inherits(crude_mod, "try-error")) {
    tab_univ      <- data.frame(Level = NA, OR = NA, Lower = NA,
                                Upper = NA, P_value = NA)
    vif_crude_df  <- data.frame(model = "Crude", term = NA, vif = NA)
  } else {
    tab_univ      <- makeORTable(crude_mod, ref_level = ref_level)
    vif_crude_df <- get_vif_df(crude_mod, "Crude", 1)
  }
  tab_univ$model  <- "Crude"
  tab_univ$Level  <- stringr::str_remove(tab_univ$Level, variable_name)

  # objects to collect VIFs and OR tables for each step
  vif_list            <- list(vif_crude_df)
  mod.results.list    <- list()
  mod.results.forplot <- list()

  #──── 2. sequentially add covariates ────#
  for (i in seq_along(joint_adjustment_vars)) {

    step_vars <- paste(unique(c(variable_name, joint_adjustment_vars[1:i])),
                       collapse = " + ")
    f_step    <- as.formula(paste(outcome, "~", step_vars))

    fit <- try(glm(f_step, data = data, family = family), silent = TRUE)
    label <- paste0("+", joint_adjustment_vars[i])

    if (inherits(fit, "try-error")) {
      tab <- data.frame(Level = NA, OR = NA, Lower = NA,
                        Upper = NA, P_value = NA, model = label)
      vif_step_df <- data.frame(model = label, term = NA, vif = NA)
    } else {
      tab <- makeORTable(fit, ref_level = ref_level)
      tab$model <- label
      vif_step_df  <- get_vif_df(fit, label, i+1)
    }

    # save VIF & OR outputs
    vif_list[[i + 1]] <- vif_step_df

    sel_indx <- grepl(variable_name, tab$Level, ignore.case = TRUE)
    sel_indx[1] <- TRUE
    tab$Level <- stringr::str_remove(tab$Level, variable_name)

    mod.results.forplot[[i]] <- tab[sel_indx, ]
    mod.results.list[[i]]    <- tab[sel_indx, ]
  }

  #──── 3. assemble the OR summary frame exactly as before ────#
  df.output <- data.frame(Level = mod.results.list[[1]]$Level)

  df.output$crude_mod_OR <- c(
    paste0(
      specifyDecimal(tab_univ$OR,     k = sf, format = format, simpleround),
      " (",
      specifyDecimal(tab_univ$Lower,  k = sf, format = format, simpleround),
      ",",
      specifyDecimal(tab_univ$Upper,  k = sf, format = format, simpleround),
      ")"
    ),
    rep(NA_character_, nrow(df.output) - nrow(tab_univ))
  )

  names(mod.results.forplot) <- joint_adjustment_vars
  mod.results.forplot$crude  <- tab_univ

  for (i in seq_along(joint_adjustment_vars)) {
    mod.results.list[[i]]$OR_concat <- paste0(
      specifyDecimal(mod.results.list[[i]]$OR,    k = sf, format = format, simpleround),
      " (",
      specifyDecimal(mod.results.list[[i]]$Lower, k = sf, format = format, simpleround),
      ",",
      specifyDecimal(mod.results.list[[i]]$Upper, k = sf, format = format, simpleround),
      ")"
    )
    df.output <- plyr::join(df.output,
                            mod.results.list[[i]][, c("Level", "OR_concat")],
                            by = "Level", type = "left", match = "first")
    names(df.output)[i + 2] <- paste0("plus_", joint_adjustment_vars[i])
  }

  out_vif <- do.call(rbind, vif_list)

  list(model_df                   = df.output,
       model_df_predictorORs_only = df.output,
       crude_model_output         = tab_univ,
       adj_model_outputs          = mod.results.forplot,
       out_vif                    = out_vif)   # <‑‑ NEW ELEMENT
}




##  Imports – add these to your R/zzz.R or NAMESPACE (roxygen style)
#' @import dplyr
#' @import stats
#' @import mgcv
#' @import progress
#' @import parallel      # <‑‑ NEW
#' @import doParallel    # <‑‑ NEW (needed only for worker registration helpers)
#' @import foreach        # <‑‑ NEW (pulled in by doParallel)


#' Run a batch of sequentially–adjusted univariable models (optionally in parallel)
#'
#' @param dat A data.frame containing the analysis dataset. Defaults to \code{dfRes}.
#' @param list_of_variables_of_interest Character vector of predictor names to iterate over.
#' @param outcome Name of the outcome column. Default \code{"res"}.
#' @param sf Significant figures to show in the formatted OR/Beta strings (passed to \code{specifyDecimal}).
#' @param format Numeric format string passed to \code{specifyDecimal}.
#' @param simpleround Logical; hand‑off to \code{specifyDecimal}.
#' @param joint_adjustment_vars Character vector of covariate names that are added one‑by‑one.
#' @param cov_name_list Optional named vector translating raw predictor names to pretty labels.
#' @param remove_intercept_from_results Logical; drop the intercept rows?  Default \code{TRUE}.
#' @param ncores Integer. \strong{NEW}.  If \code{NULL} (default) or \code{< 2} the function runs sequentially.
#'   Otherwise a fork/PSOCK cluster of this size is spun up and models are run in parallel.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{df_output}}{A tidy data.frame of formatted point estimates + CIs, one row per factor level.}
#'     \item{\code{plot_output}}{A tidy data.frame of raw OR/Beta + CI columns suitable for forest plots.}
#'   }
#' @export

# ──────────────────────────────────────────────────────────────────────────────
# BATCH WRAPPER (ModelMakerMulti) – pulls VIFs through to the top level
# ──────────────────────────────────────────────────────────────────────────────
ModelMakerMulti <- function(dat                         = dfRes,
                            list_of_variables_of_interest,
                            outcome                     = "res",
                            sf                          = 2,
                            format                      = "f",
                            simpleround                 = FALSE,
                            joint_adjustment_vars       =
                              c("age_group_named","sex","region_named",
                                "ethnic_new","imd_quintile_cat"),
                            cov_name_list               = NULL,
                            name_fun                    = NULL,
                            auto_pretty                 = TRUE,
                            remove_intercept_from_results = TRUE,
                            ncores                      = NULL) {

  num_y  <- length(unique(dplyr::pull(dat, outcome)))
  family <- if (num_y == 2L) "binomial" else "gaussian"
  if (family == "binomial") {
    message("Assuming binomial outcome (logit GLM).")
  } else {
    message("Assuming gaussian outcome (identity GLM).")
  }

  # inner runner now also returns VIFs
  single_var_runner <- function(pred_name) {

    reflev <- levels(dplyr::pull(dat, !!pred_name))[1]

    mod <- modelMakerSequential(variable_name        = pred_name,
                                data                 = dat,
                                outcome              = outcome,
                                sf                   = sf,
                                format               = format,
                                ref_level            = reflev,
                                joint_adjustment_vars = joint_adjustment_vars)

    names(mod$adj_model_outputs) <- joint_adjustment_vars
    mod$adj_model_outputs        <- mod$adj_model_outputs[
      c(length(mod$adj_model_outputs),
        seq_len(length(mod$adj_model_outputs) - 1))
    ]

    list(
      tidy_res  = mod$model_df_predictorORs_only,
      tidy_plot = dplyr::bind_rows(mod$adj_model_outputs, .id = "adjustment"),
      vif_df    = mod$out_vif
    )
  }

  # run either sequentially or in parallel exactly as before
  if (is.null(ncores) || ncores < 2L) {
    message("Running sequentially …")
    results <- lapply(list_of_variables_of_interest, single_var_runner)
  } else {
    message(sprintf("Running in parallel on %d cores …", ncores))
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      varlist = c("dat","outcome","sf","format","simpleround",
                  "joint_adjustment_vars","modelMakerSequential",
                  "specifyDecimal","makeORTable","get_vif_df"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      library(dplyr); library(stats); library(mgcv); library(stringr);
      library(plyr);  library(car)
    })
    results <- parallel::parLapply(cl,
                                   list_of_variables_of_interest,
                                   single_var_runner)
  }

  pretty_names <- vapply(list_of_variables_of_interest,
                         get_pretty_name,
                         FUN.VALUE = character(1),
                         dat        = dat,
                         name_fun   = name_fun,
                         auto_pretty = auto_pretty)

  names(results) <- pretty_names

  out_df        <- dplyr::bind_rows(lapply(results, `[[`, "tidy_res"),
                                    .id = "Variable")
  out_plot      <- dplyr::bind_rows(lapply(results, `[[`, "tidy_plot"),
                                    .id = "Variable")
  out_vif       <- dplyr::bind_rows(lapply(results, `[[`, "vif_df"),
                                    .id = "Variable")

  out_df  <- dplyr::rename(out_df,  Category = Level)
  out_plot <- dplyr::rename(out_plot, Category = Level)

  if (family == "gaussian") {
    out_plot <- dplyr::rename(out_plot, Beta = OR)
    out_df   <- dplyr::rename(out_df,  crude_mod_Beta = crude_mod_OR)
  }

  if (remove_intercept_from_results) {
    out_df  <- dplyr::filter(out_df,  !grepl("Intercept", Category))
    out_plot <- dplyr::filter(out_plot, !grepl("Intercept", Category))
    out_vif  <- dplyr::filter(out_vif,  term != "(Intercept)")
  }

  list(
    df_output   = out_df,
    plot_output = out_plot,
    vif_output  = out_vif            # <‑‑ NEW OUTPUT
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
    univ_mod <- gam(f, data = dat,family = binomial(link = "logit"))
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
      mod <- gam(f, data = dat,family = binomial(link = "logit"))

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


### Mini function to turn a glm model into an odds ratio table
makeORTable <- function(mod, ref_level = NULL,dp=3){
  mod_exp=(mod$family$family=="binomial")
  if(class(mod)[1] == "gam"){
    tab <- as.data.frame(summary.gam(mod)$p.table)
    tab$Lower = tab$Estimate - 1.96* tab$`Std. Error`
    tab$Upper = tab$Estimate + 1.96* tab$`Std. Error`
    tab <- tab %>% dplyr::select(Estimate,Lower, Upper,`Pr(>|z|)`)
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    # tab$Level <- sub("^.*?([A-Z])", "\\1",tab$Level)
    if(!is.null(ref_level)){
      tab[1,] <- c(paste0(ref_level," [reference]"), rep(NA_real_, ncol(tab)-1))
    }
    tab[,2] <- round(exp(as.numeric(tab[,2])),dp)
    tab[,3] <- round(exp(as.numeric(tab[,3])),dp)
    tab[,4] <- round(exp(as.numeric(tab[,4])),dp)
    tab[,5] <- round(as.numeric(tab[,5]),5)
  }else{
    tab <- jtools::summ(mod, exp=mod_exp, ORs = mod_exp)
    if(mod_exp){
      tab <- tab$coeftable  %>% as.data.frame() %>% dplyr::select(1,2,3,5)
    }else{
      tab <- tab$coeftable  %>% as.data.frame() %>% dplyr::select(1,2,4)
      tab$Lower = tab$Est.- qnorm(p = 0.975,mean = 0,sd = 1)*tab$S.E.
      tab$Upper = tab$Est.+ qnorm(p = 0.975,mean = 0,sd = 1)*tab$S.E.
      tab <- tab[,c(1,4,5,3)]
    }
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    # tab$Level <- sub("^.*?([A-Z])", "\\1",tab$Level)
    if(!is.null(ref_level)){
      tab[1,] <- c(paste0(ref_level," [reference]"), rep(NA_real_, ncol(tab)-1))
    }
    tab[,2] <- round(as.numeric(tab[,2]),dp)
    tab[,3] <- round(as.numeric(tab[,3]),dp)
    tab[,4] <- round(as.numeric(tab[,4]),dp)
    tab[,5] <- round(as.numeric(tab[,5]),5)
  }
  rownames(tab) <- 1:nrow(tab)
  return(tab)
}


