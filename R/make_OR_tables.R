#' @import dplyr
#' @import stats
#' @import mgcv
#' @import progress
#' @import car
#' @import parallel
#' @import doParallel
#' @import foreach

# --------------------------------------------------------------------------
# ROBUST VERSION: tidy VIF table that survives rank‑deficient models
# --------------------------------------------------------------------------
#' Compute tidy VIFs for a model with enhanced error handling
#'
#' @param fit      A fitted lm()/glm() object
#' @param step_lbl Character label for the model step ("Crude", "+age", …)
#' @param adj_no   Integer: number of covariates added so far (0 = crude)
#'
#' @return data.frame <model, adjustment, term, vif>
get_vif_df <- function(fit, step_lbl, adj_no) {

  # Check if fit is valid
  if (is.null(fit) || inherits(fit, "try-error")) {
    return(data.frame(
      model = step_lbl,
      adjustment = as.integer(adj_no),
      term = NA_character_,
      vif = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Safe extraction of terms
  terms_obj <- tryCatch(
    terms(fit),
    error = function(e) NULL
  )

  if (is.null(terms_obj)) {
    return(data.frame(
      model = step_lbl,
      adjustment = as.integer(adj_no),
      term = NA_character_,
      vif = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  terms_in_model <- attr(terms_obj, "term.labels")

  ## -------------------------------------------------
  ## 1. one‑predictor case → VIF is 1 by definition
  ## -------------------------------------------------
  if (length(terms_in_model) < 2L) {
    vif_vec <- setNames(rep(1, length(terms_in_model)), terms_in_model)

  } else {

    ## -------------------------------------------------
    ## 2. multi‑predictor case
    ##    try car::vif(); if aliased, fall back
    ## -------------------------------------------------
    vif_vec <- tryCatch({

      v <- car::vif(fit)                          # may error if aliased
      if (is.matrix(v)) v <- v[, ncol(v)]         # GVIF^(1/(2*Df))
      v                                            # named numeric vector

    }, error = function(e) {

      if (grepl("aliased coefficients", e$message, fixed = TRUE)) {

        ## --- fallback: manual VIF on full‑rank subset ----
        X <- tryCatch(
          model.matrix(fit),
          error = function(e) NULL
        )

        if (is.null(X)) {
          return(setNames(rep(NA_real_, length(terms_in_model)), terms_in_model))
        }

        X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

        if (ncol(X) == 0) {
          return(setNames(rep(NA_real_, length(terms_in_model)), terms_in_model))
        }

        qr_X <- qr(X)
        keep <- qr_X$pivot[seq_len(qr_X$rank)]   # columns giving full rank
        X_r  <- X[, keep, drop = FALSE]

        vif_manual <- sapply(seq_len(ncol(X_r)), function(j) {
          other <- X_r[, -j, drop = FALSE]
          if (ncol(other) == 0) return(1)

          tryCatch({
            r2 <- summary(lm(X_r[, j] ~ other))$r.squared
            1 / (1 - r2)
          }, error = function(e) NA_real_)
        })
        names(vif_manual) <- colnames(X_r)

        # aliased terms → NA
        if (ncol(X_r) < ncol(X)) {
          aliased <- setdiff(colnames(X), colnames(X_r))
          vif_manual <- c(vif_manual,
                          setNames(rep(NA_real_, length(aliased)), aliased))
        }
        vif_manual

      } else {

        ## any other unexpected error → NA for all terms
        setNames(rep(NA_real_, length(terms_in_model)), terms_in_model)
      }
    })
  }

  data.frame(model      = step_lbl,
             adjustment = as.integer(adj_no),
             term       = names(vif_vec),
             vif        = as.numeric(vif_vec),
             row.names  = NULL,
             stringsAsFactors = FALSE)
}

# ──────────────────────────────────────────────────────────────────────────────
# Helper that decides how to spell each variable in the output ----
# ──────────────────────────────────────────────────────────────────────────────

get_pretty_name <- function(var, dat,
                            name_fun = NULL,
                            auto_pretty = TRUE) {

  # Validate input
  if (is.null(var) || !is.character(var) || length(var) != 1) {
    return(as.character(var))
  }

  ## (i)  Session-wide dictionary in options()
  dict <- getOption("modelmaker.name_map")
  if (!is.null(dict) && var %in% names(dict)) return(dict[[var]])

  ## (ii)  Label attribute carried by haven / Hmisc / readstat, etc.
  if (!is.null(dat) && var %in% names(dat)) {
    lbl <- attr(dat[[var]], "label", exact = TRUE)
    if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1) return(lbl)
  }

  ## (iii) Automatic prettifier
  if (auto_pretty) {
    var <- gsub("_", " ", var)
    var <- tools::toTitleCase(var)
  }

  ## (iv)  Final chance: a user-supplied function
  if (is.function(name_fun)) {
    var <- tryCatch(
      name_fun(var),
      error = function(e) var
    )
  }

  var
}

# ──────────────────────────────────────────────────────────────────────────────
# Safe version of specifyDecimal (placeholder - implement your version)
# ──────────────────────────────────────────────────────────────────────────────
specifyDecimal <- function(x, k = 2, format = "f", simpleround = FALSE) {
  if (is.null(x) || all(is.na(x))) return(as.character(x))

  if (simpleround) {
    round(x, k)
  } else {
    formatC(x, digits = k, format = format)
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# ROBUST VERSION: makeORTable
# ──────────────────────────────────────────────────────────────────────────────
makeORTable <- function(mod, ref_level = NULL, dp = 3) {

  # Check if model is valid
  if (is.null(mod) || inherits(mod, "try-error")) {
    return(data.frame(
      Level = NA_character_,
      OR = NA_real_,
      Lower = NA_real_,
      Upper = NA_real_,
      P_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Determine if we should exponentiate
  mod_exp <- tryCatch(
    mod$family$family == "binomial",
    error = function(e) FALSE
  )

  # Process based on model class
  if (inherits(mod, "gam")) {

    tab <- tryCatch({
      summ <- summary.gam(mod)
      as.data.frame(summ$p.table)
    }, error = function(e) {
      message(paste("Error in GAM summary:", e$message))
      return(NULL)
    })

    if (is.null(tab)) {
      return(data.frame(
        Level = NA_character_,
        OR = NA_real_,
        Lower = NA_real_,
        Upper = NA_real_,
        P_value = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    tab$Lower = tab$Estimate - 1.96 * tab$`Std. Error`
    tab$Upper = tab$Estimate + 1.96 * tab$`Std. Error`
    tab <- tab %>% dplyr::select(Estimate, Lower, Upper, `Pr(>|z|)`)
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")

    if (!is.null(ref_level) && nrow(tab) > 0) {
      tab[1,] <- c(paste0(ref_level, " [reference]"), rep(NA_real_, ncol(tab) - 1))
    }

    if (mod_exp) {
      tab[, 2] <- round(exp(as.numeric(tab[, 2])), dp)
      tab[, 3] <- round(exp(as.numeric(tab[, 3])), dp)
      tab[, 4] <- round(exp(as.numeric(tab[, 4])), dp)
    } else {
      tab[, 2] <- round(as.numeric(tab[, 2]), dp)
      tab[, 3] <- round(as.numeric(tab[, 3]), dp)
      tab[, 4] <- round(as.numeric(tab[, 4]), dp)
    }
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)

  } else {

    # Standard GLM/LM processing
    tab <- tryCatch({
      coef_summ <- summary(mod)$coefficients
      se_col <- grep("Std\\.|Std\\. Error|Standard Error", colnames(coef_summ),
                     ignore.case = TRUE)[1]
      p_col <- grep("Pr\\(", colnames(coef_summ))[1]

      if (is.na(se_col) || is.na(p_col)) {
        stop("Cannot find standard error or p-value columns")
      }

      data.frame(
        Level = rownames(coef_summ),
        OR = coef_summ[, 1],
        SE = coef_summ[, se_col],
        P_value = coef_summ[, p_col],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message(paste("Error extracting coefficients:", e$message))
      return(NULL)
    })

    if (is.null(tab)) {
      return(data.frame(
        Level = NA_character_,
        OR = NA_real_,
        Lower = NA_real_,
        Upper = NA_real_,
        P_value = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    # Calculate CIs
    tab$Lower <- tab$OR - qnorm(0.975) * tab$SE
    tab$Upper <- tab$OR + qnorm(0.975) * tab$SE

    # Exponentiate if needed
    if (mod_exp) {
      tab$OR <- exp(tab$OR)
      tab$Lower <- exp(tab$Lower)
      tab$Upper <- exp(tab$Upper)
    }

    # Format output
    tab <- tab %>%
      dplyr::select(Level, OR, Lower, Upper, P_value) %>%
      dplyr::mutate(
        OR = round(OR, dp),
        Lower = round(Lower, dp),
        Upper = round(Upper, dp),
        P_value = round(P_value, 5)
      )

    # Add reference level if specified
    if (!is.null(ref_level) && nrow(tab) > 0) {
      # Find the variable name from the first non-intercept row
      var_rows <- grep("Intercept", tab$Level, invert = TRUE)
      if (length(var_rows) > 0) {
        # Insert reference row after intercept
        ref_row <- data.frame(
          Level = paste0(ref_level, " [reference]"),
          OR = NA_real_,
          Lower = NA_real_,
          Upper = NA_real_,
          P_value = NA_real_,
          stringsAsFactors = FALSE
        )

        if (any(grepl("Intercept", tab$Level))) {
          tab <- rbind(tab[1, , drop = FALSE], ref_row, tab[-1, , drop = FALSE])
        } else {
          tab <- rbind(ref_row, tab)
        }
      }
    }
  }

  rownames(tab) <- NULL
  return(tab)
}

# ──────────────────────────────────────────────────────────────────────────────
# ROBUST SEQUENTIAL MODELLER
# ──────────────────────────────────────────────────────────────────────────────

modelMakerSequential <- function(variable_name,
                                 data = dfRes,
                                 sf = 2,
                                 format = "f",
                                 simpleround = FALSE,
                                 outcome = "res",
                                 ref_level = NULL,
                                 joint_adjustment_vars = c("age_group_named", "sex",
                                                           "region_named", "ethnic_new",
                                                           "imd_quintile_cat")) {

  # Input validation
  if (!variable_name %in% names(data)) {
    stop(paste("Variable", variable_name, "not found in data"))
  }

  if (!outcome %in% names(data)) {
    stop(paste("Outcome", outcome, "not found in data"))
  }

  # Check for missing adjustment variables
  missing_vars <- setdiff(joint_adjustment_vars, names(data))
  if (length(missing_vars) > 0) {
    warning(paste("Adjustment variables not found in data:",
                  paste(missing_vars, collapse = ", ")))
    joint_adjustment_vars <- setdiff(joint_adjustment_vars, missing_vars)
  }

  # Determine model family
  num_y <- length(unique(na.omit(dplyr::pull(data, outcome))))
  family <- if (num_y == 2L) "binomial" else "gaussian"

  # Create a safe data subset with complete cases for the outcome
  data_subset <- data[!is.na(data[[outcome]]), ]

  #──── 1. crude model ────#
  f0 <- as.formula(paste(outcome, "~", variable_name))

  crude_mod <- tryCatch(
    glm(f0, data = data_subset, family = family),
    error = function(e) {
      message(paste("Error fitting crude model for", variable_name, ":", e$message))
      return(NULL)
    }
  )

  if (is.null(crude_mod)) {
    tab_univ <- data.frame(
      Level = paste0(variable_name, " [Model failed]"),
      OR = NA, Lower = NA, Upper = NA, P_value = NA,
      stringsAsFactors = FALSE
    )
    vif_crude_df <- data.frame(
      model = "Crude",
      adjustment = 1,
      term = variable_name,
      vif = NA,
      stringsAsFactors = FALSE
    )
  } else {
    tab_univ <- makeORTable(crude_mod, ref_level = ref_level)
    vif_crude_df <- get_vif_df(crude_mod, "Crude", 1)
  }

  tab_univ$model <- "Crude"

  # Safe string removal
  if (!is.na(tab_univ$Level[1])) {
    tab_univ$Level <- stringr::str_remove(tab_univ$Level, variable_name)
  }

  # Objects to collect VIFs and OR tables for each step
  vif_list <- list(vif_crude_df)
  mod.results.list <- list()
  mod.results.forplot <- list()

  #──── 2. sequentially add covariates ────#
  for (i in seq_along(joint_adjustment_vars)) {

    # Build formula with unique variables
    vars_to_include <- unique(c(variable_name, joint_adjustment_vars[1:i]))

    # Check if all variables exist and have variation
    valid_vars <- sapply(vars_to_include, function(v) {
      v %in% names(data_subset) && length(unique(na.omit(data_subset[[v]]))) > 1
    })

    if (!all(valid_vars)) {
      warning(paste("Skipping adjustment step", i,
                    "- missing or constant variables:",
                    paste(vars_to_include[!valid_vars], collapse = ", ")))
      next
    }

    step_vars <- paste(vars_to_include, collapse = " + ")
    f_step <- as.formula(paste(outcome, "~", step_vars))

    label <- paste0("+", joint_adjustment_vars[i])

    fit <- tryCatch(
      glm(f_step, data = data_subset, family = family),
      error = function(e) {
        message(paste("Error fitting model at step", i, ":", e$message))
        return(NULL)
      }
    )

    if (is.null(fit)) {
      tab <- data.frame(
        Level = paste0(variable_name, " [Model failed]"),
        OR = NA, Lower = NA, Upper = NA, P_value = NA,
        model = label,
        stringsAsFactors = FALSE
      )
      vif_step_df <- data.frame(
        model = label,
        adjustment = i + 1,
        term = NA,
        vif = NA,
        stringsAsFactors = FALSE
      )
    } else {
      tab <- makeORTable(fit, ref_level = ref_level)
      tab$model <- label
      vif_step_df <- get_vif_df(fit, label, i + 1)
    }

    # Save VIF & OR outputs
    vif_list[[i + 1]] <- vif_step_df

    # Safe selection of relevant rows
    if (nrow(tab) > 0) {
      sel_indx <- grepl(variable_name, tab$Level, ignore.case = TRUE)
      sel_indx[1] <- TRUE  # Always include first row (intercept or reference)

      # Safe string removal
      tab$Level <- sapply(tab$Level, function(x) {
        if (!is.na(x)) stringr::str_remove(x, variable_name) else x
      })

      mod.results.forplot[[i]] <- tab[sel_indx, , drop = FALSE]
      mod.results.list[[i]] <- tab[sel_indx, , drop = FALSE]
    } else {
      mod.results.forplot[[i]] <- tab
      mod.results.list[[i]] <- tab
    }
  }

  #──── 3. assemble the OR summary frame ────#

  # Find the maximum number of levels across all models
  max_levels <- max(c(nrow(tab_univ),
                      sapply(mod.results.list, function(x) if (!is.null(x)) nrow(x) else 0)))

  # Initialize output data frame
  if (length(mod.results.list) > 0 && !is.null(mod.results.list[[1]])) {
    df.output <- data.frame(
      Level = c(mod.results.list[[1]]$Level,
                rep(NA, max(0, max_levels - length(mod.results.list[[1]]$Level)))),
      stringsAsFactors = FALSE
    )
  } else {
    df.output <- data.frame(Level = character(max_levels), stringsAsFactors = FALSE)
  }

  # Add crude model results
  crude_or_values <- character(nrow(df.output))
  for (j in seq_len(min(nrow(tab_univ), nrow(df.output)))) {
    crude_or_values[j] <- paste0(
      specifyDecimal(tab_univ$OR[j], k = sf, format = format, simpleround = simpleround),
      " (",
      specifyDecimal(tab_univ$Lower[j], k = sf, format = format, simpleround = simpleround),
      ",",
      specifyDecimal(tab_univ$Upper[j], k = sf, format = format, simpleround = simpleround),
      ")"
    )
  }
  df.output$crude_mod_OR <- crude_or_values

  # Store results for plotting
  names(mod.results.forplot) <- joint_adjustment_vars[seq_along(mod.results.forplot)]
  mod.results.forplot$crude <- tab_univ

  # Add adjusted model results
  for (i in seq_along(mod.results.list)) {
    if (!is.null(mod.results.list[[i]]) && nrow(mod.results.list[[i]]) > 0) {

      # Create OR concatenated string
      or_concat <- character(nrow(mod.results.list[[i]]))
      for (j in seq_len(nrow(mod.results.list[[i]]))) {
        or_concat[j] <- paste0(
          specifyDecimal(mod.results.list[[i]]$OR[j], k = sf, format = format, simpleround = simpleround),
          " (",
          specifyDecimal(mod.results.list[[i]]$Lower[j], k = sf, format = format, simpleround = simpleround),
          ",",
          specifyDecimal(mod.results.list[[i]]$Upper[j], k = sf, format = format, simpleround = simpleround),
          ")"
        )
      }

      mod.results.list[[i]]$OR_concat <- or_concat

      # Use dplyr::left_join instead of plyr::join for better error handling
      col_name <- paste0("plus_", joint_adjustment_vars[i])

      temp_df <- mod.results.list[[i]][, c("Level", "OR_concat"), drop = FALSE]
      names(temp_df)[2] <- col_name

      df.output <- tryCatch(
        dplyr::left_join(df.output, temp_df, by = "Level"),
        error = function(e) {
          message(paste("Error joining results at step", i, ":", e$message))
          df.output[[col_name]] <- NA_character_
          df.output
        }
      )
    }
  }

  # Combine VIF results
  out_vif <- tryCatch(
    do.call(rbind, vif_list),
    error = function(e) {
      message(paste("Error combining VIF results:", e$message))
      data.frame(
        model = character(),
        adjustment = integer(),
        term = character(),
        vif = numeric(),
        stringsAsFactors = FALSE
      )
    }
  )

  list(
    model_df = df.output,
    model_df_predictorORs_only = df.output,
    crude_model_output = tab_univ,
    adj_model_outputs = mod.results.forplot,
    out_vif = out_vif
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# ROBUST BATCH WRAPPER (ModelMakerMulti)
# ──────────────────────────────────────────────────────────────────────────────
ModelMakerMulti <- function(dat = dfRes,
                            list_of_variables_of_interest,
                            outcome = "res",
                            sf = 2,
                            format = "f",
                            simpleround = FALSE,
                            joint_adjustment_vars = c("age_group_named", "sex",
                                                      "region_named", "ethnic_new",
                                                      "imd_quintile_cat"),
                            cov_name_list = NULL,
                            name_fun = NULL,
                            auto_pretty = TRUE,
                            remove_intercept_from_results = TRUE,
                            ncores = NULL) {

  # Input validation
  if (!is.data.frame(dat)) {
    stop("dat must be a data.frame")
  }

  if (!outcome %in% names(dat)) {
    stop(paste("Outcome variable", outcome, "not found in data"))
  }

  # Check which variables exist
  missing_vars <- setdiff(list_of_variables_of_interest, names(dat))
  if (length(missing_vars) > 0) {
    warning(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
    list_of_variables_of_interest <- setdiff(list_of_variables_of_interest, missing_vars)
  }

  if (length(list_of_variables_of_interest) == 0) {
    stop("No valid variables to analyze")
  }

  # Determine model family
  num_y <- length(unique(na.omit(dplyr::pull(dat, outcome))))
  family <- if (num_y == 2L) "binomial" else "gaussian"

  if (family == "binomial") {
    message("Assuming binomial outcome (logit GLM).")
  } else {
    message("Assuming gaussian outcome (identity GLM).")
  }

  # Inner runner function with error handling
  single_var_runner <- function(pred_name) {

    tryCatch({

      # Check if variable exists and has variation
      if (!pred_name %in% names(dat)) {
        warning(paste("Variable", pred_name, "not found in data"))
        return(NULL)
      }

      var_values <- dat[[pred_name]]
      if (length(unique(na.omit(var_values))) < 2) {
        warning(paste("Variable", pred_name, "has no variation"))
        return(NULL)
      }

      # Determine reference level
      reflev <- NULL
      if (is.factor(var_values)) {
        reflev <- levels(var_values)[1]
      }

      # Run sequential models
      mod <- modelMakerSequential(
        variable_name = pred_name,
        data = dat,
        outcome = outcome,
        sf = sf,
        format = format,
        ref_level = reflev,
        joint_adjustment_vars = joint_adjustment_vars
      )

      # Reorganize adjustment outputs
      if (length(mod$adj_model_outputs) > 0) {
        names(mod$adj_model_outputs) <- joint_adjustment_vars[seq_along(mod$adj_model_outputs)]
        mod$adj_model_outputs <- mod$adj_model_outputs[
          c(length(mod$adj_model_outputs),
            seq_len(length(mod$adj_model_outputs) - 1))
        ]
      }

      # Safely bind rows
      tidy_plot <- tryCatch(
        dplyr::bind_rows(mod$adj_model_outputs, .id = "adjustment"),
        error = function(e) {
          message(paste("Error binding plot results for", pred_name, ":", e$message))
          data.frame(adjustment = character(), stringsAsFactors = FALSE)
        }
      )

      list(
        tidy_res = mod$model_df_predictorORs_only,
        tidy_plot = tidy_plot,
        vif_df = mod$out_vif
      )

    }, error = function(e) {
      message(paste("Error processing variable", pred_name, ":", e$message))
      return(NULL)
    })
  }

  # Run either sequentially or in parallel
  if (is.null(ncores) || ncores < 2L) {
    message("Running sequentially …")
    results <- lapply(list_of_variables_of_interest, single_var_runner)
  } else {
    message(sprintf("Running in parallel on %d cores …", ncores))

    cl <- tryCatch(
      parallel::makeCluster(ncores),
      error = function(e) {
        warning(paste("Failed to create cluster:", e$message, "- running sequentially"))
        return(NULL)
      }
    )

    if (!is.null(cl)) {
      on.exit(parallel::stopCluster(cl), add = TRUE)

      # Export necessary objects
      parallel::clusterExport(
        cl,
        varlist = c("dat", "outcome", "sf", "format", "simpleround",
                    "joint_adjustment_vars", "modelMakerSequential",
                    "specifyDecimal", "makeORTable", "get_vif_df"),
        envir = environment()
      )

      # Load required packages
      parallel::clusterEvalQ(cl, {
        suppressPackageStartupMessages({
          library(dplyr)
          library(stats)
          library(mgcv)
          library(stringr)
          library(car)
        })
      })

      results <- parallel::parLapply(cl, list_of_variables_of_interest, single_var_runner)
    } else {
      results <- lapply(list_of_variables_of_interest, single_var_runner)
    }
  }

  # Remove NULL results
  null_results <- sapply(results, is.null)
  if (any(null_results)) {
    failed_vars <- list_of_variables_of_interest[null_results]
    warning(paste("Failed to process variables:", paste(failed_vars, collapse = ", ")))
    results <- results[!null_results]
    list_of_variables_of_interest <- list_of_variables_of_interest[!null_results]
  }

  if (length(results) == 0) {
    stop("All variables failed to process")
  }

  # Get pretty names
  pretty_names <- vapply(
    list_of_variables_of_interest,
    get_pretty_name,
    FUN.VALUE = character(1),
    dat = dat,
    name_fun = name_fun,
    auto_pretty = auto_pretty
  )

  names(results) <- pretty_names

  # Safely combine results
  out_df <- tryCatch(
    dplyr::bind_rows(lapply(results, `[[`, "tidy_res"), .id = "Variable"),
    error = function(e) {
      message(paste("Error binding tidy results:", e$message))
      data.frame(Variable = character(), stringsAsFactors = FALSE)
    }
  )

  out_plot <- tryCatch(
    dplyr::bind_rows(lapply(results, `[[`, "tidy_plot"), .id = "Variable"),
    error = function(e) {
      message(paste("Error binding plot results:", e$message))
      data.frame(Variable = character(), stringsAsFactors = FALSE)
    }
  )

  out_vif <- tryCatch(
    dplyr::bind_rows(lapply(results, `[[`, "vif_df"), .id = "Variable"),
    error = function(e) {
      message(paste("Error binding VIF results:", e$message))
      data.frame(Variable = character(), stringsAsFactors = FALSE)
    }
  )

  # Rename columns if they exist
  if ("Level" %in% names(out_df)) {
    out_df <- dplyr::rename(out_df, Category = Level)
  }
  if ("Level" %in% names(out_plot)) {
    out_plot <- dplyr::rename(out_plot, Category = Level)
  }

  # Handle gaussian family renaming
  if (family == "gaussian") {
    if ("OR" %in% names(out_plot)) {
      out_plot <- dplyr::rename(out_plot, Beta = OR)
    }
    if ("crude_mod_OR" %in% names(out_df)) {
      out_df <- dplyr::rename(out_df, crude_mod_Beta = crude_mod_OR)
    }
  }

  # Remove intercept rows if requested
  if (remove_intercept_from_results) {
    if (nrow(out_df) > 0 && "Category" %in% names(out_df)) {
      out_df <- dplyr::filter(out_df, !grepl("Intercept", Category, ignore.case = TRUE))
    }
    if (nrow(out_plot) > 0 && "Category" %in% names(out_plot)) {
      out_plot <- dplyr::filter(out_plot, !grepl("Intercept", Category, ignore.case = TRUE))
    }
    if (nrow(out_vif) > 0 && "term" %in% names(out_vif)) {
      out_vif <- dplyr::filter(out_vif, !term %in% c("(Intercept)", NA))
    }
  }

  list(
    df_output = out_df,
    plot_output = out_plot,
    vif_output = out_vif
  )
}
