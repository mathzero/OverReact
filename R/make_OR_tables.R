
#' @import dplyr
#' @import stats
#' @import mgcv
#' @import progress


#' @param dat data to be modelled
#' @param dfRes data to be modelled
#' @param datatype Specify the data type (from 'continuous', 'binary', or 'date' )
#' @param negval If data is binary, what is the negative value in the uncleaned data?
#' @param posval If data is binary, what is the positive value in the uncleaned data?
#' @param floornumber If data is continuous, what is the lowest 'real' value in the uncleaned data?
#' @param dateFormat If data is date, what is the required output date format?


# Model maker function ----------------------------------------------------




### Main function to create a sequentially adjusted model. Depends on makeORTable
modelMakerSequential <- function(variable_name, data=dfRes,sf=2,format ="f",simpleround =F,
                                 outcome="res", ref_level =NULL,
                                 joint_adjustment_vars = c("age_group_named","sex","region_named",
                                                           "ethnic_new", "imd_quintile_cat")){

  # determine outcome type
  num_y=length(unique(pull(data,outcome)))
  if(num_y==2){
    # print("Assuming binomial model")
    family="binomial"
  }else if (num_y>2){
    # print("Assuming gaussian model")
    family="gaussian"
  }else{
    print("Less than two unique values of outcome variable are found. Aborting modelling")
    break
  }


  classes <- lapply(data[,c(variable_name,joint_adjustment_vars)], class) %>% unlist()

  if("character" %in% classes){
    print("These variables are character class. Consider converting to factor:")
    print(c(variable_name,joint_adjustment_vars)[classes == "character"])

  }
  ### models will fail if insufficient data to work with, so putting this in as a quick failsafe.
  ### With world enough and time, much better to replace with a trycatch

  # check that we don't have an insufficient amount of data for the number of categories
  # numvals <- length(unique(pull(data,variable_name)))
  # nobs <- sum(!is.na(pull(data,variable_name)))
  #
  # if(class(data[,variable_name]) %in% c("factor","character") & nobs<=numvals+30 ){
  #   print("May not be enough data to run this model")
  # }else{

    ### create univariate model
    f <- as.formula(paste(outcome," ~",variable_name))
    univ.mod.glm <- try(glm(f,  data = data,
                        family = family),silent = T)
    if(is(univ.mod.glm,"try-error")){
      tab_univ <-     data.frame(Level=NA, OR=NA, Lower=NA,
                                 Upper=NA, P_value=NA)

    }else{

    ### Create OR table
    tab_univ <- makeORTable(univ.mod.glm, ref_level=ref_level)

    ### Add model name
    tab_univ$model <- "Crude"
    tab_univ$Level <- stringr::str_remove(tab_univ$Level,variable_name)


    }
    # create empty lists for results
    mod.results.list <- list()
    mod.results.list.forplot <- list()


    for (i in 1:length(joint_adjustment_vars)){


      # print(paste0("Now processing additional covariate:", joint_adjustment_vars[i]))

      ### adjusted for age and gender
      f <- as.formula(paste(outcome," ~", paste(unique(c(variable_name,joint_adjustment_vars[1:i])),
                                                collapse = "+")))
      # run model
      mod <- univ.mod.glm <- try(glm(f,  data = data,
                                 family = family),silent = T)

      if(is(univ.mod.glm,"try-error")){
        tab <-     data.frame(Level=NA, OR=NA, Lower=NA,
                                   Upper=NA, P_value=NA,
                              model=paste0("+", joint_adjustment_vars[[i]]))
        # save for plot
        mod.results.list.forplot[[i]] <- tab

        ### Add to list
        mod.results.list[[i]] <- tab
      }else{

      ### Create OR table
      tab <- makeORTable(mod, ref_level=ref_level)

      ### Add model name
      tab$model <- paste0("+", joint_adjustment_vars[[i]])


      ### Select only the rows that refer to the variable of interest (for plotting)
      sel_indx=grepl(pattern = variable_name,x = tab$Level,ignore.case = T)
      sel_indx[[1]] <- TRUE # we always want the first one

      # remove variable name
      tab$Level <- stringr::str_remove(tab$Level,variable_name)


      # save for plot
      mod.results.list.forplot[[i]] <- tab[sel_indx,]

      ### Add to list
      mod.results.list[[i]] <- tab[sel_indx,]
      }
    }

    ### Now we add all the models together into one big DF
    df.output=data.frame(Level=mod.results.list[[1]]$Level)
    df.output$crude_mod_OR <- c(paste0(specifyDecimal(x = tab_univ$OR, k = sf,format = format,simpleround =simpleround),
                                       " (",
                                       specifyDecimal(tab_univ$Lower,  k = sf,format = format,simpleround =simpleround),
                                       ",",
                                       specifyDecimal(tab_univ$Upper,  k = sf,format = format,simpleround =simpleround),
                                       ")"),
                                rep(NA_character_, nrow(df.output)- nrow(tab_univ)))

    names(mod.results.list.forplot)  <- joint_adjustment_vars
    mod.results.list.forplot$crude <- tab_univ


    for (i in 1:length(joint_adjustment_vars)){
      # print(i)
      mod.results.list[[i]]$OR_concat <- c(paste0(specifyDecimal(mod.results.list[[i]]$OR,  k = sf,format = format,simpleround =simpleround),
                                                  " (",
                                                  specifyDecimal(mod.results.list[[i]]$Lower, k = sf,format = format,simpleround =simpleround),
                                                  ",",
                                                  specifyDecimal(mod.results.list[[i]]$Upper,  k = sf,format = format,simpleround =simpleround),
                                                  ")"))

      df.output <- plyr::join(df.output, mod.results.list[[i]] %>% dplyr::select(Level, OR_concat),
                              by="Level", type = "left", match = "first")
      names(df.output)[i+2]=paste0("plus_", joint_adjustment_vars[[i]])

    }

  # }
  df.output.abbrev <- df.output

  return(list(model_df=df.output,
              model_df_predictorORs_only = df.output.abbrev,
              crude_model_output=tab_univ,
              adj_model_outputs=mod.results.list.forplot))
}



# Helper that decides how to spell each variable in the output ------------


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


#' Multiple-model maker
#' ---------------------------------------------------------------
#' @import dplyr
#' @import stats
#' @import mgcv
#' @import progress
#' @importFrom future.apply future_lapply
#' @importFrom future plan multisession
#' @importFrom progressr with_progress progressor

#' @param dat  Data frame to be modelled
#' @param list_of_variables_of_interest Character vector of predictors
#' @param outcome Name of the outcome column
#' @param sf,format,simpleround  Arguments passed to \code{specifyDecimal()}
#' @param joint_adjustment_vars Vector of sequential adjustment variables
#' @param cov_name_list Optional named vector for pretty-printing predictors
#' @param remove_intercept_from_results Logical; drop “Intercept” rows?
#' @param ncores Integer.  If \code{NULL} or \code{< 2} the function
#'   runs sequentially; otherwise it runs in parallel on the requested
#'   number of CPU cores using \pkg{future.apply}.
#'

ModelMakerMulti <- function(dat               = dfRes,
                            list_of_variables_of_interest,
                            outcome           = "res",
                            family            = NULL,
                            sf                = 2,
                            name_fun          = NULL,
                            auto_pretty       = TRUE,
                            format            = "f",
                            simpleround       = FALSE,
                            joint_adjustment_vars = c("age_group_named",
                                                      "sex",
                                                      "region_named",
                                                      "ethnic_new",
                                                      "imd_quintile_cat"),
                            remove_intercept_from_results = TRUE,
                            ncores            = NULL) {

  ## ------------------------------------------------------------------ ##
  ## 1.  Detect outcome type (if caller did not supply `family`)        ##
  ## ------------------------------------------------------------------ ##
  if (is.null(family)) {
    message("No family supplied")
    family <- if (length(unique(dplyr::pull(dat, outcome))) == 2) {
      message("Assuming binomial model")
      "binomial"
    } else {
      message("Assuming gaussian model")
      "gaussian"
    }
  }

  ## ------------------------------------------------------------------ ##
  ## 2.  Workhorse: fit all sequential models for one predictor         ##
  ## ------------------------------------------------------------------ ##
  run_one <- function(var_name) {
    reflev <- levels(dplyr::pull(dat, var_name))[1]

    mod <- modelMakerSequential(variable_name         = var_name,
                                data                  = dat,
                                outcome               = outcome,
                                sf                    = sf,
                                format                = format,
                                ref_level             = reflev,
                                joint_adjustment_vars = joint_adjustment_vars)

    names(mod$adj_model_outputs) <- joint_adjustment_vars
    mod$adj_model_outputs <- mod$adj_model_outputs[
      c(length(mod$adj_model_outputs),
        seq_len(length(mod$adj_model_outputs) - 1))
    ]

    list(res  = mod$model_df_predictorORs_only,
         plot = dplyr::bind_rows(mod$adj_model_outputs, .id = "adjustment"))
  }

  ## ------------------------------------------------------------------ ##
  ## 3.  Sequential or parallel?                                        ##
  ## ------------------------------------------------------------------ ##
  if (is.null(ncores) || ncores < 2) {

    ## ---- Sequential with progress::progress_bar ----
    pb <- progress::progress_bar$new(
      format = " Running models [:bar] :percent eta: :eta",
      total  = length(list_of_variables_of_interest),
      width  = 100, clear = FALSE)
    pb$tick(0)

    results <- lapply(list_of_variables_of_interest, function(v) {
      pb$tick()
      run_one(v)
    })

  } else {

    ## ---- Parallel with future + progressr ----
    oplan <- future::plan()                             # save user's plan
    on.exit(future::plan(oplan), add = TRUE)
    future::plan(future::multisession, workers = ncores)

    results <- progressr::with_progress({
      p <- progressr::progressor(steps = length(list_of_variables_of_interest))
      future.apply::future_lapply(
        list_of_variables_of_interest,
        function(v) { p(); run_one(v) }
      )
    })
  }

  ## ------------------------------------------------------------------ ##
  ## 4.  Human-friendly variable names                                  ##
  ## ------------------------------------------------------------------ ##
  pretty_names <- vapply(list_of_variables_of_interest,
                         get_pretty_name,
                         FUN.VALUE  = character(1),
                         dat        = dat,
                         name_fun   = name_fun,
                         auto_pretty = auto_pretty)

  names(results) <- pretty_names

  ## ------------------------------------------------------------------ ##
  ## 5.  Assemble final data frames                                     ##
  ## ------------------------------------------------------------------ ##
  res_list      <- lapply(results, `[[`, "res")
  plot_res_list <- lapply(results, `[[`, "plot")

  out_df   <- dplyr::bind_rows(res_list,  .id = "Variable") |>
    dplyr::rename(Category = Level)

  out_plot <- dplyr::bind_rows(plot_res_list, .id = "Variable") |>
    dplyr::rename(Category = Level)

  if (family == "gaussian") {
    out_plot <- dplyr::rename(out_plot,  Beta = OR)
    out_df   <- dplyr::rename(out_df, crude_mod_Beta = crude_mod_OR)
  }

  if (remove_intercept_from_results) {
    out_plot <- dplyr::filter(out_plot, !grepl("Intercept", Category))
    out_df   <- dplyr::filter(out_df,   !grepl("Intercept", Category))
  }

  ## ------------------------------------------------------------------ ##
  ## 6.  Return                                                         ##
  ## ------------------------------------------------------------------ ##
  list(df_output   = out_df,
       plot_output = out_plot)
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


