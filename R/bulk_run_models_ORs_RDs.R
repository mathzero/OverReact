#' @import dplyr
#' @import stats
#' @import mgcv
#' @import progress
#' @import car
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import MASS
#' @import data.table
#' @import stringr

# =============================================================================
#  Model-maker utilities -- VERSION WITH IMPROVED ERROR HANDLING
# =============================================================================
# 2025-07-31
# -----------------------------------------------------------------------------
#  Improvements:
#  - Better missing data handling
#  - Factor level validation
#  - Perfect separation detection
#  - More informative error messages
#  - Model convergence controls
# -----------------------------------------------------------------------------

#
# # =============================================================================
# # 1. Helper: pretty variable names --------------------------------------------
# # =============================================================================
# get_pretty_name <- function(var, dat, name_fun = NULL, auto_pretty = TRUE) {
#   if (is.null(var) || !is.character(var) || length(var) != 1) {
#     return(as.character(var))
#   }
#   dict <- getOption("modelmaker.name_map")
#   if (!is.null(dict) && var %in% names(dict)) return(dict[[var]])
#   if (!is.null(dat) && var %in% names(dat)) {
#     lbl <- attr(dat[[var]], "label", exact = TRUE)
#     if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1) return(lbl)
#   }
#   if (auto_pretty) {
#     var <- gsub("_", " ", var)
#     var <- tools::toTitleCase(var)
#   }
#   if (is.function(name_fun)) {
#     var <- tryCatch(name_fun(var), error = function(e) var)
#   }
#   var
# }


# =============================================================================
# NEW HELPER: Validate factor levels after subsetting -------------------------
# =============================================================================
validate_factor_levels <- function(data, var_names) {
  issues <- list()
  for (var in var_names) {
    if (var %in% names(data) && is.factor(data[[var]])) {
      # Drop unused levels and check if any levels have zero counts
      data[[var]] <- droplevels(data[[var]])
      level_counts <- table(data[[var]])
      if (any(level_counts == 0)) {
        issues[[var]] <- paste("Zero counts in levels:",
                               paste(names(level_counts)[level_counts == 0], collapse = ", "))
      }
      if (length(levels(data[[var]])) < 2) {
        issues[[var]] <- "Factor has less than 2 levels after subsetting"
      }
    }
  }
  return(list(data = data, issues = issues))
}

# =============================================================================
# NEW HELPER: Check for perfect separation ------------------------------------
# =============================================================================
check_perfect_separation <- function(data, outcome, predictor) {
  if (!outcome %in% names(data) || !predictor %in% names(data)) return(NULL)

  # Only check for binary outcomes
  if (length(unique(na.omit(data[[outcome]]))) != 2) return(NULL)

  if (is.factor(data[[predictor]]) || is.character(data[[predictor]])) {
    # Check cross-tabulation for zero cells
    tab <- table(data[[outcome]], data[[predictor]])
    if (any(tab == 0)) {
      return(paste("Perfect separation detected: some", predictor,
                   "levels have all 0s or all 1s in outcome"))
    }
  }
  return(NULL)
}

# =============================================================================
# 3. Odds-ratio / coefficient table builder -----------------------------------
# =============================================================================
makeORTable <- function(mod, ref_level = NULL, dp = 3) {
  tab <- .react_build_coef_table(mod, ref_level = ref_level, dp = dp)
  if (!nrow(tab)) {
    return(data.frame(
      Level = NA_character_,
      term_raw = NA_character_,
      OR = NA_real_,
      Lower = NA_real_,
      Upper = NA_real_,
      P_value = NA_real_,
      is_reference = NA,
      is_intercept = NA,
      model_family = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  rownames(tab) <- NULL
  tab
}

# =============================================================================
# 4. Risk-difference table builder --------------------------------------------
# =============================================================================


makeRDTable <- function(mod, variable_name, ref_level = NULL, dp = 4,
                        data = NULL, n_sim = 100, seed = 1) {
  na_tbl <- data.frame(Level = NA_character_, RD = NA_real_, Lower = NA_real_,
                       Upper = NA_real_, P_value = NA_real_,
                       stringsAsFactors = FALSE)
  if (is.null(mod) || inherits(mod, "try-error")) return(na_tbl)
  fam_is_binom <- tryCatch(mod$family$family == "binomial",
                           error = function(e) FALSE)
  if (!fam_is_binom) return(na_tbl)
  if (is.null(data)) data <- model.frame(mod)
  if (!variable_name %in% names(data)) return(na_tbl)

  # Check for valid vcov matrix before proceeding
  vcov_mat <- tryCatch(vcov(mod), error = function(e) NULL)
  if (is.null(vcov_mat) || any(is.na(vcov_mat))) {
    warning("Invalid variance-covariance matrix for RD calculation")
    return(na_tbl)
  }

  get_X <- function(nd) model.matrix(delete.response(terms(mod)), data = nd)
  # 1) factor exposure ---------------------------------------------------------
  if (is.factor(data[[variable_name]])) {
    lvls <- levels(data[[variable_name]])
    if (is.null(ref_level) || !(ref_level %in% lvls)) ref_level <- lvls[1]
    comp_lvls <- setdiff(lvls, ref_level)
    new_ref <- data; new_ref[[variable_name]] <- factor(ref_level, levels = lvls)
    X_ref <- get_X(new_ref)
    beta_hat <- coef(mod)
    lin_ref <- X_ref %*% beta_hat
    risk_ref_hat <- mean(plogis(lin_ref))
    set.seed(seed)
    beta_sim <- tryCatch(MASS::mvrnorm(n_sim, mu = beta_hat, Sigma = vcov_mat),
                         error = function(e) NULL)
    if (is.null(beta_sim)) return(na_tbl)

    out <- vector("list", length(comp_lvls))
    for (j in seq_along(comp_lvls)) {
      lvl <- comp_lvls[j]
      new_lvl <- data; new_lvl[[variable_name]] <- factor(lvl, levels = lvls)
      X_lvl <- get_X(new_lvl)
      lin_lvl <- X_lvl %*% beta_hat
      risk_lvl_hat <- mean(plogis(lin_lvl))
      rd_hat <- risk_lvl_hat - risk_ref_hat
      lin_ref_sim <- X_ref %*% t(beta_sim)
      lin_lvl_sim <- X_lvl %*% t(beta_sim)
      risk_ref_sim <- colMeans(plogis(lin_ref_sim))
      risk_lvl_sim <- colMeans(plogis(lin_lvl_sim))
      rd_sim <- risk_lvl_sim - risk_ref_sim
      ci <- quantile(rd_sim, c(.025, .975), na.rm = TRUE)
      se_rd <- sd(rd_sim, na.rm = TRUE)
      z_rd <- rd_hat / se_rd
      p_rd <- 2 * (1 - pnorm(abs(z_rd)))
      out[[j]] <- data.frame(Level = lvl, RD = round(rd_hat, dp),
                             Lower = round(ci[1], dp), Upper = round(ci[2], dp),
                             P_value = round(p_rd, 5), stringsAsFactors = FALSE)
    }
    return(do.call(rbind, out))
  }
  # 2) numeric / ordered exposure --------------------------------------------
  if (is.numeric(data[[variable_name]]) || is.ordered(data[[variable_name]])) {
    X <- model.matrix(delete.response(terms(mod)), data = data)
    beta_hat <- coef(mod)
    p_hat <- plogis(X %*% beta_hat)
    col_id <- grep(paste0("^", variable_name), colnames(X))
    if (length(col_id) == 0) return(na_tbl)
    ame_hat <- mean(p_hat * (1 - p_hat) * X[, col_id, drop = FALSE])
    set.seed(seed)
    beta_sim <- tryCatch(MASS::mvrnorm(n_sim, mu = beta_hat, Sigma = vcov_mat),
                         error = function(e) NULL)
    if (is.null(beta_sim)) return(na_tbl)

    lin_sim <- X %*% t(beta_sim)
    p_sim <- plogis(lin_sim)
    ame_sim <- colMeans(p_sim * (1 - p_sim) * X[, col_id])
    ci_ame <- quantile(ame_sim, c(.025, .975), na.rm = TRUE)
    se_ame <- sd(ame_sim, na.rm = TRUE)
    z_ame <- ame_hat / se_ame
    p_ame <- 2 * (1 - pnorm(abs(z_ame)))
    return(data.frame(Level = variable_name, RD = round(ame_hat, dp),
                      Lower = round(ci_ame[1], dp), Upper = round(ci_ame[2], dp),
                      P_value = round(p_ame, 5), stringsAsFactors = FALSE))
  }
  na_tbl
}

## ===========================================================================
##  Revised wrapper functions to separate OR and RD outputs
##  ---------------------------------------------------------------------------
##  - modelMakerSequentialRD(): now returns separate OR and RD tables & plots
##  - ModelMakerMultiRD():      now collates and returns four outputs:
##       * df_output          (OR summary tables)
##       * plot_output        (OR plotting tables)
##       * df_output_RDs      (Risk-difference summary tables)
##       * plot_output_RDs    (Risk-difference plotting tables)
##  All other behaviour and arguments are unchanged.
## ===========================================================================

# ---------------------------------------------------------------------------
# 5. modelMakerSequentialRD() - SEPARATE OR / RD OUTPUTS
# ---------------------------------------------------------------------------
modelMakerSequentialRD <- function(variable_name,
                                   data                    = dfRes,
                                   sf                      = 2,
                                   format                  = "f",
                                   simpleround             = FALSE,
                                   outcome                 = "res",
                                   ref_level               = NULL,
                                   joint_adjustment_vars   = c("age_group_named","sex","region_named",
                                                               "ethnic_new","imd_quintile_cat"),
                                   include_rd              = FALSE,
                                   n_sim                   = 100,
                                   glm_control             = glm.control(maxit = 50),
                                   sample_strategy         = c("common_per_predictor", "per_model")) {
  sample_strategy <- match.arg(sample_strategy)

  found_adj <- intersect(joint_adjustment_vars, names(data))
  missing_adj <- setdiff(joint_adjustment_vars, names(data))
  if (length(missing_adj)) {
    warning("Adjustment variables not found in data: ", paste(missing_adj, collapse = ", "))
  }
  found_adj <- setdiff(found_adj, variable_name)

  adj_sets <- lapply(seq_along(found_adj), function(i) found_adj[seq_len(i)])
  names(adj_sets) <- found_adj

  out <- modelMakerAdjustedSetsRD(
    variable_name = variable_name,
    data = data,
    sf = sf,
    format = format,
    simpleround = simpleround,
    outcome = outcome,
    ref_level = ref_level,
    adj_sets = adj_sets,
    include_crude = TRUE,
    include_rd = include_rd,
    n_sim = n_sim,
    glm_control = glm_control,
    sample_strategy = sample_strategy
  )

  rename_cols <- function(df, suffix) {
    if (is.null(df) || !nrow(df)) return(df)
    for (adj_name in names(adj_sets)) {
      old_name <- paste0("adj_", adj_name, suffix)
      new_name <- paste0("plus_", adj_name, suffix)
      if (old_name %in% names(df)) names(df)[names(df) == old_name] <- new_name
    }
    df
  }

  out$model_df_ORs <- rename_cols(out$model_df_ORs, "_OR")
  out$model_df_RDs <- rename_cols(out$model_df_RDs, "_RD")
  out
}




# =========================================================
# Memory- and compute-efficient adjusted risk-difference
# (and average marginal effect) table builder
# =========================================================
#
# # =========================================================
# # Memory- and compute-efficient adjusted risk-difference
# # (and average marginal effect) table builder
# # =========================================================
# makeRDTable_fast2 <- function(mod, variable_name, ref_level = NULL, dp = 4,
#                               data = NULL, n_sim = 100, seed = 1,
#                               chunk_size = 5e4, sparse = TRUE) {
#
#   na_tbl <- data.frame(Level   = NA_character_,
#                        RD      = NA_real_,
#                        Lower   = NA_real_,
#                        Upper   = NA_real_,
#                        P_value = NA_real_,
#                        stringsAsFactors = FALSE)
#
#   if (is.null(mod) || inherits(mod, "try-error") ||
#       mod$family$family != "binomial") return(na_tbl)
#
#   if (is.null(data)) data <- model.frame(mod)
#   if (!variable_name %in% names(data)) return(na_tbl)
#
#   vc <- tryCatch(vcov(mod), error = function(e) NULL)
#   if (is.null(vc) || anyNA(vc)) return(na_tbl)
#
#   # -- model matrix ----------------------------------------------------------
#   if (sparse) {
#     X_full <- Matrix::sparse.model.matrix(delete.response(terms(mod)), data)
#   } else {
#     X_full <- model.matrix(delete.response(terms(mod)), data)
#   }
#
#   beta_hat <- coef(mod)
#   set.seed(seed)
#   beta_sim <- MASS::mvrnorm(n_sim, mu = beta_hat, Sigma = vc)
#   B        <- cbind(beta_hat, t(beta_sim))          # p  x (1 + n_sim)
#   k        <- ncol(B)                               # number of coefficient draws
#
#   # -- helper: mean risk (vectorised over coefficient draws) -----------------
#   mean_risk_multi <- function(X, B) {
#     n   <- nrow(X)
#     idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
#     out <- numeric(k)
#     for (i in idx) {
#       lp  <- as.matrix(X[i, , drop = FALSE] %*% B)  # chunk x k
#       out <- out + colSums(plogis(lp))
#     }
#     out / n
#   }
#
#   # --------------------------------------------------------------------------
#   # 1) Categorical exposure ---------------------------------------------------
#   # --------------------------------------------------------------------------
#   if (is.factor(data[[variable_name]])) {
#
#     lvls <- levels(data[[variable_name]])
#     if (is.null(ref_level) || !(ref_level %in% lvls)) ref_level <- lvls[1]
#     comp_lvls <- setdiff(lvls, ref_level)
#
#     # locate dummy columns for the factor (excluding intercept, interactions)
#     col_id <- grep(paste0("^", variable_name), colnames(X_full), value = TRUE)
#
#     # baseline (reference): zero all dummies for this factor
#     X_ref <- X_full
#     if (length(col_id)) X_ref[, col_id] <- 0
#
#     risk_ref_all <- mean_risk_multi(X_ref, B)
#     risk_ref_hat <- risk_ref_all[1]
#     risk_ref_sim <- risk_ref_all[-1]
#
#     out <- vector("list", length(comp_lvls))
#
#     for (j in seq_along(comp_lvls)) {
#       lvl     <- comp_lvls[j]
#       # column for this specific level (simple main-effect coding)
#       lvl_col <- grep(paste0(variable_name, lvl),
#                       colnames(X_full),
#                       value = TRUE,
#                       fixed = TRUE)
#
#       # If we don't find a unique column for the level, skip safely
#       if (!length(lvl_col)) {
#         out[[j]] <- data.frame(Level   = lvl,
#                                RD      = NA_real_,
#                                Lower   = NA_real_,
#                                Upper   = NA_real_,
#                                P_value = NA_real_,
#                                stringsAsFactors = FALSE)
#         next
#       }
#
#       X_lvl <- X_ref
#       X_lvl[, lvl_col] <- 1
#
#       risk_lvl_all <- mean_risk_multi(X_lvl, B)
#       rd_hat       <- risk_lvl_all[1]  - risk_ref_hat
#       rd_sim       <- risk_lvl_all[-1] - risk_ref_sim
#
#       ci <- stats::quantile(rd_sim, c(.025, .975), names = FALSE, type = 7)
#       se <- stats::sd(rd_sim)
#       p  <- 2 * (1 - stats::pnorm(abs(rd_hat / se)))
#
#       out[[j]] <- data.frame(Level   = lvl,
#                              RD      = round(rd_hat, dp),
#                              Lower   = round(ci[1], dp),
#                              Upper   = round(ci[2], dp),
#                              P_value = round(p,    5),
#                              stringsAsFactors = FALSE)
#     }
#
#     comp_tab <- do.call(rbind, out)
#
#     # ---- insert an empty [reference] row to mirror makeORTable ------------
#     ref_row <- data.frame(Level   = paste0(ref_level, " [reference]"),
#                           RD      = NA_real_,
#                           Lower   = NA_real_,
#                           Upper   = NA_real_,
#                           P_value = NA_real_,
#                           stringsAsFactors = FALSE)
#
#     tab <- if (is.null(comp_tab) || nrow(comp_tab) == 0) {
#       ref_row
#     } else {
#       rbind(ref_row, comp_tab)
#     }
#
#     rownames(tab) <- NULL
#     return(tab)
#   }
#
#   # --------------------------------------------------------------------------
#   # 2) Numeric / ordered exposure --------------------------------------------
#   # --------------------------------------------------------------------------
#   if (is.numeric(data[[variable_name]]) || is.ordered(data[[variable_name]])) {
#
#     col_id <- grep(paste0("^", variable_name), colnames(X_full))
#     if (!length(col_id)) return(na_tbl)
#
#     n   <- nrow(X_full)
#     idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
#     out <- numeric(k)          # will hold sums -> divide by n later
#
#     for (i in idx) {
#       lp   <- as.matrix(X_full[i, , drop = FALSE] %*% B)       # chunk x k
#       p    <- plogis(lp)
#       w    <- p * (1 - p)
#       xcol <- X_full[i, col_id]
#       out  <- out + colSums(w * matrix(xcol, nrow = length(xcol), ncol = k))
#     }
#     ame_all <- out / n
#     ame_hat <- ame_all[1]
#     ame_sim <- ame_all[-1]
#
#     ci <- stats::quantile(ame_sim, c(.025, .975), names = FALSE, type = 7)
#     se <- stats::sd(ame_sim)
#     p  <- 2 * (1 - stats::pnorm(abs(ame_hat / se)))
#
#     tab <- data.frame(Level   = variable_name,
#                       RD      = round(ame_hat, dp),
#                       Lower   = round(ci[1],  dp),
#                       Upper   = round(ci[2],  dp),
#                       P_value = round(p,      5),
#                       stringsAsFactors = FALSE)
#     rownames(tab) <- NULL
#     return(tab)
#   }
#
#   na_tbl
# }




# Non INcremental workers -------------------------------------------------
# =========================
# Helpers (new/updated)
# =========================

# Always returns a [reference] row for categorical exposures in RD output
safe_makeRDTable <- function(mod, variable_name, ref_level = NULL, dp = 4,
                             data = NULL, n_sim = 100, seed = 1) {
  out <- tryCatch(
    makeRDTable(mod = mod, variable_name = variable_name, ref_level = ref_level,
                dp = dp, data = data, n_sim = n_sim, seed = seed),
    error = function(e) { message("Risk-difference table failed: ", e$message); NULL }
  )
  if (is.null(out)) return(NULL)

  # Inject a [reference] row for categorical exposures if missing
  mf <- tryCatch(model.frame(mod), error = function(e) NULL)
  if (!is.null(mf) && variable_name %in% names(mf) && is.factor(mf[[variable_name]])) {
    lvls <- levels(mf[[variable_name]])
    if (is.null(ref_level) || !(ref_level %in% lvls)) ref_level <- lvls[1]
    ref_lbl <- paste0(ref_level, " [reference]")
    if (!any(grepl("\\[reference\\]$", out$Level))) {
      ref_row <- data.frame(Level = ref_lbl, RD = NA_real_, Lower = NA_real_,
                            Upper = NA_real_, P_value = NA_real_, stringsAsFactors = FALSE)
      out <- rbind(ref_row, out)
    }
  }
  out
}

# Build cumulative adjustment sets from a vector of variables:
# c("age","sex","region") -> list("age", "age+sex", "age+sex+region")
build_cumulative_sets <- function(adj_vars) {
  if (length(adj_vars) == 0L) return(list(`full` = character(0)))
  sets <- lapply(seq_along(adj_vars), function(i) adj_vars[seq_len(i)])
  nm   <- vapply(seq_along(adj_vars),
                 function(i) paste(adj_vars[seq_len(i)], collapse = "+"),
                 FUN.VALUE = character(1))
  names(sets) <- nm
  sets
}

# Compute factor-level order for wide tables (reference first, then remaining levels in factor order)
compute_level_order <- function(x, ref_level = NULL) {
  if (!is.factor(x)) return(NULL)
  lvls <- levels(x)
  ref  <- if (is.null(ref_level) || !(ref_level %in% lvls)) lvls[1] else ref_level
  c(paste0(ref, " [reference]"), setdiff(lvls, ref))
}



# =========================
# File I/O helpers (with diagnostics, RDS-first)
# =========================

# ---------------------------------------------------------------------
# Helpers for robust parallel with restart
# ---------------------------------------------------------------------

.collect_completed <- function(vars, savepath_runs, include_rd) {
  out <- list()
  if (is.null(savepath_runs) || !length(vars)) return(out)
  for (v in vars) {
    fmap <- .var_filemap(savepath_runs, .sanitize_var_id(v), include_rd = include_rd)
    if (.has_complete_outputs(fmap)) out[[v]] <- .load_var_outputs(fmap)
  }
  out
}

.run_todo_with_restarts <- function(todo_vars,
                                    single_var_runner,
                                    savepath_runs,
                                    include_rd,
                                    ncores,
                                    parallel_restarts = 2,
                                    shrink_workers_on_retry = TRUE,
                                    retry_sleep = 1) {
  computed  <- list()
  remaining <- todo_vars
  if (!length(remaining)) return(computed)

  use_parallel <- is.numeric(ncores) && is.finite(ncores) && ncores > 1L
  if (!use_parallel) {
    message("Running sequentially (ncores <= 1).")
    for (v in remaining) computed[[v]] <- single_var_runner(v, save_immediately = TRUE)
    return(computed)
  }

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    message("future/future.apply not available; running sequentially.")
    for (v in remaining) computed[[v]] <- single_var_runner(v, save_immediately = TRUE)
    return(computed)
  }

  # Determine initial worker count
  workers <- tryCatch({
    ac <- future::availableCores()
    if (is.numeric(ac) && length(ac) == 1L) as.integer(min(ncores, ac)) else as.integer(ncores)
  }, error = function(e) as.integer(ncores))
  if (!is.finite(workers) || workers < 1L) workers <- 1L

  # Choose initial backend
  backend_first <- if (isTRUE(tryCatch(future::supportsMulticore(), error = function(e) FALSE))) "multicore" else "multisession"

  old_plan <- future::plan()
  on.exit({ try(future::plan(old_plan), silent = TRUE) }, add = TRUE)

  attempt <- 1L
  while (length(remaining) && attempt <= as.integer(max(1L, parallel_restarts))) {
    backend <- if (attempt == 1L) backend_first else "multisession"
    w <- workers
    if (attempt > 1L && isTRUE(shrink_workers_on_retry)) w <- max(1L, floor(w / 2L))

    # Try to set plan
    plan_ok <- TRUE
    plan_msg <- NULL
    tryCatch({
      if (identical(backend, "multicore")) {
        future::plan(future::multicore, workers = w)
      } else {
        future::plan(future::multisession, workers = w)
      }
    }, error = function(e) { plan_ok <<- FALSE; plan_msg <<- conditionMessage(e) })

    if (!plan_ok) {
      message("Parallel attempt ", attempt, " (", backend, ", workers=", w,
              ") failed to set plan: ", plan_msg)
    } else {
      message("Parallel attempt ", attempt, " using backend=", backend, ", workers=", w,
              " for ", length(remaining), " variables ...")
      err_msg <- NULL
      res_list <- tryCatch(
        future.apply::future_lapply(
          remaining,
          function(v) single_var_runner(v, save_immediately = TRUE),
          future.seed = TRUE
        ),
        error = function(e) { err_msg <<- conditionMessage(e); NULL }
      )

      if (!is.null(res_list)) {
        names(res_list) <- remaining
        for (nm in names(res_list)) computed[[nm]] <- res_list[[nm]]
        remaining <- character(0)
        break
      } else {
        message("Parallel attempt ", attempt, " failed: ", if (is.null(err_msg)) "unknown error" else err_msg)
      }
    }

    # Harvest any completions from disk and shrink the todo set
    newly <- .collect_completed(remaining, savepath_runs, include_rd)
    if (length(newly)) {
      message("Recovered completed variables from disk: ", paste(names(newly), collapse = ", "))
      for (nm in names(newly)) computed[[nm]] <- newly[[nm]]
      remaining <- setdiff(remaining, names(newly))
    }

    if (!length(remaining)) break

    attempt <- attempt + 1L
    if (retry_sleep > 0) Sys.sleep(retry_sleep)
  }

  # Fallback to sequential for whatever is left
  if (length(remaining)) {
    message("Falling back to sequential for ", length(remaining), " variables: ",
            paste(remaining, collapse = ", "))
    for (v in remaining) computed[[v]] <- single_var_runner(v, save_immediately = TRUE)
  }

  computed
}


.sanitize_var_id <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

.safe_dir_create <- function(path) {
  ok <- tryCatch({ dir.create(path, showWarnings = FALSE, recursive = TRUE); TRUE },
                 error = function(e) FALSE, warning = function(w) TRUE)
  isTRUE(ok) && dir.exists(path)
}

.atomic_write_csv <- function(df, path) {
  tmp <- paste0(path, ".tmp-", Sys.getpid(), "-", sprintf("%08x", sample.int(.Machine$integer.max, 1)))
  on.exit({ if (file.exists(tmp)) unlink(tmp) }, add = TRUE)
  if (requireNamespace("readr", quietly = TRUE)) readr::write_csv(df, tmp) else utils::write.csv(df, tmp, row.names = FALSE)
  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)
}

.atomic_write_rds <- function(obj, path) {
  tmp <- paste0(path, ".tmp-", Sys.getpid(), "-", sprintf("%08x", sample.int(.Machine$integer.max, 1)))
  on.exit({ if (file.exists(tmp)) unlink(tmp) }, add = TRUE)
  saveRDS(obj, file = tmp)
  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)
}

.robust_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  out <- tryCatch({
    if (requireNamespace("readr", quietly = TRUE)) suppressWarnings(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
    else utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) data.frame())
  as.data.frame(out, stringsAsFactors = FALSE)
}

.robust_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

.coerce_df_types <- function(df, kind = c("df_or","plot_or","df_rd","plot_rd","diagnostics")) {
  kind <- match.arg(kind)
  if (!nrow(df)) return(df)
  to_char <- intersect(c("Level","Category","term_raw","model","model_family","adjustment_label","stage","adjusted_vars","status","error_msg","warnings","separation_msg"), names(df))
  for (nm in to_char) df[[nm]] <- as.character(df[[nm]])
  if (kind %in% c("plot_or","plot_rd")) {
    if ("adjustment" %in% names(df)) df$adjustment <- suppressWarnings(as.integer(df$adjustment))
    num_cols <- if (kind == "plot_or") c("OR","Lower","Upper","P_value") else c("RD","Lower","Upper","P_value")
    for (nm in intersect(num_cols, names(df))) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  } else if (kind %in% c("df_or","df_rd")) {
    if ("Nobs_in_model" %in% names(df)) df$Nobs_in_model <- suppressWarnings(as.integer(df$Nobs_in_model))
  } else if (kind == "diagnostics") {
    int_cols <- intersect(c("nobs","rd_rows","attempt"), names(df))
    for (nm in int_cols) df[[nm]] <- suppressWarnings(as.integer(df[[nm]]))
    if ("vcov_ok" %in% names(df)) df$vcov_ok <- as.logical(df$vcov_ok)
  }
  df
}

.var_filemap <- function(savepath_runs, var_id, include_rd = TRUE) {
  var_dir  <- file.path(savepath_runs, var_id)
  list(
    dir          = var_dir,
    df_or_csv    = file.path(var_dir, "df_or.csv"),
    plot_or_csv  = file.path(var_dir, "plot_or.csv"),
    df_rd_csv    = if (isTRUE(include_rd)) file.path(var_dir, "df_rd.csv") else NULL,
    plot_rd_csv  = if (isTRUE(include_rd)) file.path(var_dir, "plot_rd.csv") else NULL,
    diag_csv     = file.path(var_dir, "diagnostics.csv"),
    df_or_rds    = file.path(var_dir, "df_or.rds"),
    plot_or_rds  = file.path(var_dir, "plot_or.rds"),
    df_rd_rds    = if (isTRUE(include_rd)) file.path(var_dir, "df_rd.rds") else NULL,
    plot_rd_rds  = if (isTRUE(include_rd)) file.path(var_dir, "plot_rd.rds") else NULL,
    diag_rds     = file.path(var_dir, "diagnostics.rds")
  )
}

.has_complete_outputs <- function(filemap) {
  req <- c(filemap$df_or_rds, filemap$plot_or_rds, filemap$df_rd_rds, filemap$plot_rd_rds)
  req <- req[!sapply(req, is.null)]
  if (length(req) && all(file.exists(req))) {
    sizes <- file.info(req)$size
    return(all(is.finite(sizes) & !is.na(sizes) & sizes > 0))
  }
  req_csv <- c(filemap$df_or_csv, filemap$plot_or_csv, filemap$df_rd_csv, filemap$plot_rd_csv)
  req_csv <- req_csv[!sapply(req_csv, is.null)]
  if (!length(req_csv)) return(FALSE)
  if (!all(file.exists(req_csv))) return(FALSE)
  sizes <- file.info(req_csv)$size
  all(is.finite(sizes) & !is.na(sizes) & sizes > 0)
}

.save_var_outputs <- function(filemap, out, diagnostics = NULL, write_csv = TRUE) {
  .safe_dir_create(filemap$dir)
  .atomic_write_rds(out$tidy_res_or,  filemap$df_or_rds)
  .atomic_write_rds(out$tidy_plot_or, filemap$plot_or_rds)
  if (!is.null(filemap$df_rd_rds))  .atomic_write_rds(out$tidy_res_rd,  filemap$df_rd_rds)
  if (!is.null(filemap$plot_rd_rds)) .atomic_write_rds(out$tidy_plot_rd, filemap$plot_rd_rds)
  if (!is.null(diagnostics)) .atomic_write_rds(diagnostics, filemap$diag_rds)

  if (isTRUE(write_csv)) {
    if (!is.null(out$tidy_res_or)  && nrow(out$tidy_res_or))  .atomic_write_csv(out$tidy_res_or,  filemap$df_or_csv)
    if (!is.null(out$tidy_plot_or) && nrow(out$tidy_plot_or)) .atomic_write_csv(out$tidy_plot_or, filemap$plot_or_csv)
    if (!is.null(filemap$df_rd_csv)   && !is.null(out$tidy_res_rd)  && nrow(out$tidy_res_rd))  .atomic_write_csv(out$tidy_res_rd,  filemap$df_rd_csv)
    if (!is.null(filemap$plot_rd_csv) && !is.null(out$tidy_plot_rd) && nrow(out$tidy_plot_rd)) .atomic_write_csv(out$tidy_plot_rd, filemap$plot_rd_csv)
    if (!is.null(diagnostics) && nrow(diagnostics)) .atomic_write_csv(diagnostics, filemap$diag_csv)
  }
  invisible(TRUE)
}

.load_var_outputs <- function(filemap) {
  df_or   <- .robust_read_rds(filemap$df_or_rds)
  plot_or <- .robust_read_rds(filemap$plot_or_rds)
  df_rd   <- if (!is.null(filemap$df_rd_rds))  .robust_read_rds(filemap$df_rd_rds)  else NULL
  plot_rd <- if (!is.null(filemap$plot_rd_rds)) .robust_read_rds(filemap$plot_rd_rds) else NULL
  diag_df <- .robust_read_rds(filemap$diag_rds)

  using_rds <- !is.null(df_or) && !is.null(plot_or) &&
    (is.null(filemap$df_rd_rds)  || !is.null(df_rd)) &&
    (is.null(filemap$plot_rd_rds) || !is.null(plot_rd))

  if (!using_rds) {
    df_or   <- .coerce_df_types(.robust_read_csv(filemap$df_or_csv),   "df_or")
    plot_or <- .coerce_df_types(.robust_read_csv(filemap$plot_or_csv), "plot_or")
    if (!is.null(filemap$df_rd_csv))   df_rd   <- .coerce_df_types(.robust_read_csv(filemap$df_rd_csv),   "df_rd")   else df_rd <- data.frame()
    if (!is.null(filemap$plot_rd_csv)) plot_rd <- .coerce_df_types(.robust_read_csv(filemap$plot_rd_csv), "plot_rd") else plot_rd <- data.frame()
    diag_df <- .coerce_df_types(.robust_read_csv(filemap$diag_csv), "diagnostics")
    .safe_dir_create(filemap$dir)
    .atomic_write_rds(df_or,   filemap$df_or_rds)
    .atomic_write_rds(plot_or, filemap$plot_or_rds)
    if (!is.null(filemap$df_rd_rds))   .atomic_write_rds(df_rd,   filemap$df_rd_rds)
    if (!is.null(filemap$plot_rd_rds)) .atomic_write_rds(plot_rd, filemap$plot_rd_rds)
    if (!is.null(diag_df) && nrow(diag_df)) .atomic_write_rds(diag_df, filemap$diag_rds)
  }

  list(
    tidy_res_or  = as.data.frame(df_or,   stringsAsFactors = FALSE),
    tidy_plot_or = as.data.frame(plot_or, stringsAsFactors = FALSE),
    tidy_res_rd  = as.data.frame(if (is.null(df_rd))  data.frame() else df_rd,   stringsAsFactors = FALSE),
    tidy_plot_rd = as.data.frame(if (is.null(plot_rd)) data.frame() else plot_rd, stringsAsFactors = FALSE),
    diagnostics  = as.data.frame(if (is.null(diag_df)) data.frame() else diag_df, stringsAsFactors = FALSE)
  )
}

# =========================
# Diagnostics-aware adjusted-sets runner
# =========================

.run_glm_captured <- function(fml, data, family, control) {
  .react_fit_glm_captured(fml, data = data, family = family, control = control)
}


# =============================================================================

# modelMakerAdjustedSetsRD(): ensures level-order & clean plot lis --------

# ======================================================================
# Diagnostics-aware adjusted-sets runner - now keeps reference in plots
# ======================================================================
modelMakerAdjustedSetsRD <- function(variable_name,
                                     data                    = dfRes,
                                     sf                      = 2,
                                     format                  = "f",
                                     simpleround             = FALSE,
                                     outcome                 = "res",
                                     ref_level               = NULL,
                                     adj_sets                = list(),
                                     include_crude           = TRUE,
                                     include_rd              = FALSE,
                                     n_sim                   = 100,
                                     glm_control             = glm.control(maxit = 50),
                                     sample_strategy         = c("common_per_predictor", "per_model")) {
  sample_strategy <- match.arg(sample_strategy)

  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!all(c(variable_name, outcome) %in% names(data))) {
    stop("variable_name or outcome not found in `data`.")
  }

  if (length(adj_sets) == 0L) adj_sets <- list(full = character(0))
  if (is.null(names(adj_sets)) || any(!nzchar(names(adj_sets)))) {
    names(adj_sets) <- if (length(adj_sets) == 1L) "full" else paste0("set", seq_along(adj_sets))
  }

  used_adjusters <- unique(unlist(adj_sets, use.names = FALSE))
  missing_adj <- setdiff(used_adjusters, names(data))
  if (length(missing_adj)) {
    warning("Adjustment variables not found in data: ", paste(missing_adj, collapse = ", "))
  }

  adj_sets <- lapply(adj_sets, function(x) {
    x <- intersect(x, names(data))
    setdiff(x, variable_name)
  })

  stage_specs <- list()
  if (isTRUE(include_crude)) {
    stage_specs[[length(stage_specs) + 1L]] <- list(
      stage_id = "crude",
      adjusted_vars = character(0),
      model = "crude",
      wide_col = "crude_mod_OR",
      adjustment = 0L
    )
  }
  for (i in seq_along(adj_sets)) {
    adj_name <- names(adj_sets)[[i]]
    adj_vars <- adj_sets[[i]]
    stage_specs[[length(stage_specs) + 1L]] <- list(
      stage_id = adj_name,
      adjusted_vars = adj_vars,
      model = if (length(adj_vars)) paste0("+", paste(adj_vars, collapse = "+")) else "full",
      wide_col = paste0("adj_", adj_name, "_OR"),
      adjustment = if (isTRUE(include_crude)) i else i
    )
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

  or_rows <- runner$rows
  df_or <- .react_wide_from_long(
    long_df = or_rows,
    effect_col = "OR",
    sf = sf,
    format = format,
    simpleround = simpleround,
    include_nobs = identical(sample_strategy, "common_per_predictor")
  )

  rd_rows <- data.frame()
  if (isTRUE(include_rd) && identical(runner$family, "binomial")) {
    rd_parts <- lapply(runner$stages, .react_extract_rd_rows, predictor = variable_name, n_sim = n_sim, dp = 3)
    rd_rows <- dplyr::bind_rows(rd_parts)
  }
  df_rd <- if (nrow(rd_rows)) {
    .react_wide_from_long(
      long_df = rd_rows,
      effect_col = "RD",
      sf = sf + 1L,
      format = format,
      simpleround = simpleround,
      include_nobs = identical(sample_strategy, "common_per_predictor")
    )
  } else NULL

  diag_df <- runner$diagnostics
  if (nrow(diag_df)) {
    rd_counts <- integer(nrow(diag_df))
    if (nrow(rd_rows)) {
      rd_count_map <- tapply(rd_rows$Level, rd_rows$stage_id, length)
      rd_counts <- unname(rd_count_map[match(diag_df$stage, names(rd_count_map))])
      rd_counts[is.na(rd_counts)] <- 0L
    }
    diag_df$rd_rows <- rd_counts
  }

  crude_or <- if (isTRUE(include_crude)) or_rows[or_rows$stage_id == "crude", , drop = FALSE] else NULL
  crude_rd <- if (isTRUE(include_rd) && isTRUE(include_crude) && nrow(rd_rows)) {
    rd_rows[rd_rows$stage_id == "crude", , drop = FALSE]
  } else NULL

  adj_or_outputs <- lapply(names(adj_sets), function(adj_name) {
    or_rows[or_rows$stage_id == adj_name, , drop = FALSE]
  })
  names(adj_or_outputs) <- names(adj_sets)

  adj_rd_outputs <- lapply(names(adj_sets), function(adj_name) {
    if (!nrow(rd_rows)) return(data.frame())
    rd_rows[rd_rows$stage_id == adj_name, , drop = FALSE]
  })
  names(adj_rd_outputs) <- names(adj_sets)

  stage_nobs <- vapply(runner$stages, function(stage) {
    if (is.null(stage$data)) 0L else nrow(stage$data)
  }, integer(1))

  list(
    model_df_ORs = df_or,
    model_df_RDs = if (isTRUE(include_rd)) df_rd else NULL,
    crude_model_OR = crude_or,
    crude_model_RD = crude_rd,
    adj_or_outputs = adj_or_outputs,
    adj_rd_outputs = adj_rd_outputs,
    diagnostics = diag_df,
    n_complete_cases = if (length(stage_nobs)) max(stage_nobs) else 0L,
    n_original_cases = sum(!is.na(data[[outcome]]))
  )
}



# ---------------------------------------------------------------------
# ModelMakerMultiRD() - with robust parallel restart & no hard stop
# ---------------------------------------------------------------------
ModelMakerMultiRD <- function(dat = dfRes, list_of_variables_of_interest, outcome = "res",
                              sf = 2, format = "f", simpleround = FALSE,
                              joint_adjustment_vars = c("age_group_named", "sex",
                                                        "region_named", "ethnic_new",
                                                        "imd_quintile_cat"),
                              adj_sets = NULL,
                              incremental = TRUE,
                              include_crude = TRUE,
                              cov_name_list = NULL, name_fun = NULL,
                              auto_pretty = TRUE,
                              remove_intercept_from_results = TRUE,
                              ncores = NULL, include_rd = TRUE, n_sim = 100,
                              savepath = NULL,
                              glm_control = glm.control(maxit = 50),
                              retry_failed = TRUE,
                              max_retries = 1,
                              # NEW: robustness for future backend
                              parallel_restarts = 2,
                              retry_sleep = 1,
                              sample_strategy = c("common_per_predictor", "per_model")) {
  sample_strategy <- match.arg(sample_strategy)

  if (!is.data.frame(dat)) stop("dat must be a data.frame")
  if (!outcome %in% names(dat)) stop(paste("Outcome", outcome, "not found"))

  missing_vars <- setdiff(list_of_variables_of_interest, names(dat))
  if (length(missing_vars) > 0) {
    warning("Variables not found: ", paste(missing_vars, collapse = ", "))
    list_of_variables_of_interest <- setdiff(list_of_variables_of_interest, missing_vars)
  }
  if (length(list_of_variables_of_interest) == 0) stop("No variables to analyse")

  family <- .react_detect_outcome_family(dat[[outcome]], outcome = outcome)
  message(if (family == "binomial") "Assuming binomial outcome" else "Assuming gaussian outcome")

  # --- save paths (RDS-first checkpointing assumed available) --------------
  savepath_runs <- NULL
  if (!is.null(savepath)) {
    savepath_runs <- file.path(savepath, "runs")
    if (!.safe_dir_create(savepath_runs)) {
      warning("Could not create '", savepath_runs, "'. Checkpointing disabled.")
      savepath_runs <- NULL
    }
  }

  # --- pretty-name helper (fallback) ---------------------------------------
  if (!exists("get_pretty_name")) {
    get_pretty_name <- function(var, dat, name_fun = NULL, auto_pretty = TRUE) {
      if (!is.character(var) || length(var) != 1) return(as.character(var))
      if (!is.null(dat) && var %in% names(dat)) {
        lbl <- attr(dat[[var]], "label", exact = TRUE)
        if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1) return(lbl)
      }
      var2 <- if (auto_pretty) tools::toTitleCase(gsub("_", " ", var)) else var
      if (is.function(name_fun)) return(tryCatch(name_fun(var2), error = function(e) var2))
      var2
    }
  }

  # --- set builder ---------------------------------------------------------
  build_cumulative_sets <- function(adj_vars) {
    if (length(adj_vars) == 0L) return(list(`full` = character(0)))
    sets <- lapply(seq_along(adj_vars), function(i) adj_vars[seq_len(i)])
    nm   <- vapply(seq_along(adj_vars),
                   function(i) paste(adj_vars[seq_len(i)], collapse = "+"),
                   FUN.VALUE = character(1))
    names(sets) <- nm
    sets
  }

  # ---------- per-variable worker (unchanged modelling logic) --------------
  single_var_runner <- function(pred_name, save_immediately = TRUE) {
    ref_lv <- if (is.factor(dat[[pred_name]]) && !is.ordered(dat[[pred_name]])) levels(dat[[pred_name]])[1] else NULL
    pred_adjusters <- setdiff(joint_adjustment_vars, pred_name)
    sets <- if (isTRUE(incremental)) {
      build_cumulative_sets(pred_adjusters)
    } else {
      if (is.null(adj_sets)) list(full = pred_adjusters) else {
        sets_local <- adj_sets
        if (is.list(sets_local) && is.null(names(sets_local))) {
          names(sets_local) <- if (length(sets_local) == 1L) "full" else paste0("set", seq_along(sets_local))
        }
        lapply(sets_local, function(x) setdiff(x, pred_name))
      }
    }

    adj_map <- data.frame(
      adjustment_label = names(sets),
      adjustment = vapply(sets, length, integer(1)),
      stringsAsFactors = FALSE
    )

    run_once <- function(ctrl, attempt = 1L) {
      res <- tryCatch(
        modelMakerAdjustedSetsRD(variable_name = pred_name, data = dat, outcome = outcome,
                                 sf = sf, format = format, simpleround = simpleround,
                                 ref_level = ref_lv,
                                 adj_sets = sets, include_crude = include_crude,
                                 include_rd = include_rd, n_sim = n_sim,
                                 glm_control = ctrl,
                                 sample_strategy = sample_strategy),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        return(list(res = NULL,
                    diag = data.frame(stage="__wrapper__", adjusted_vars="", status="error",
                                      error_msg=conditionMessage(res), warnings="", nobs=NA,
                                      vcov_ok=NA, separation_msg="", rd_rows=NA,
                                      attempt=attempt, stringsAsFactors = FALSE)))
      }
      if (!is.null(res$diagnostics) && nrow(res$diagnostics)) res$diagnostics$attempt <- attempt
      list(res = res, diag = res$diagnostics)
    }

    a1 <- run_once(glm_control, attempt = 1L)
    all_diag <- .coerce_df_types(if (is.null(a1$diag)) data.frame() else a1$diag, "diagnostics")

    # retry (model-level) if gaps
    has_gaps <- function(res, sets, need_crude, need_rd) {
      if (is.null(res)) return(TRUE)
      gap <- FALSE
      if (need_crude) {
        if (is.null(res$crude_model_OR) || !nrow(res$crude_model_OR)) gap <- TRUE
        if (need_rd && (is.null(res$crude_model_RD) || !nrow(res$crude_model_RD))) gap <- TRUE
      }
      want <- names(sets)
      got_or <- names(res$adj_or_outputs)[vapply(res$adj_or_outputs, function(x) !is.null(x) && nrow(x) > 0, logical(1))]
      if (!all(want %in% got_or)) gap <- TRUE
      if (need_rd) {
        got_rd <- names(res$adj_rd_outputs)[vapply(res$adj_rd_outputs, function(x) !is.null(x) && nrow(x) > 0, logical(1))]
        if (!all(want %in% got_rd)) gap <- TRUE
      }
      gap
    }

    need_retry <- isTRUE(retry_failed) && has_gaps(a1$res, sets, include_crude, include_rd) && max_retries >= 1L
    if (need_retry) {
      message("Retrying variable '", pred_name, "' with stricter glm.control ...")
      ctrl2 <- glm_control
      if (is.null(ctrl2$maxit) || !is.finite(ctrl2$maxit)) ctrl2$maxit <- 50
      ctrl2$maxit   <- max(100L, as.integer(ctrl2$maxit * 2L))
      ctrl2$epsilon <- if (is.null(ctrl2$epsilon)) 1e-8 else min(1e-8, ctrl2$epsilon / 10)
      a2 <- run_once(ctrl2, attempt = 2L)
      if (!is.null(a2$diag) && nrow(a2$diag)) all_diag <- rbind(all_diag, .coerce_df_types(a2$diag, "diagnostics"))
      if (!is.null(a2$res)) a1$res <- a2$res
    }

    if (is.null(a1$res)) {
      message("Variable ", pred_name, ": failed completely after retries.")
      return(list(tidy_res_or  = data.frame(), tidy_plot_or  = data.frame(),
                  tidy_res_rd  = data.frame(), tidy_plot_rd  = data.frame(),
                  diagnostics  = all_diag))
    }

    res <- a1$res

    if (isTRUE(remove_intercept_from_results) && !is.null(res$model_df_ORs) && nrow(res$model_df_ORs)) {
      or_tables <- c(list(res$crude_model_OR), res$adj_or_outputs)
      or_tables <- Filter(function(x) !is.null(x) && nrow(x), or_tables)
      if (length(or_tables)) {
        or_long_all <- dplyr::bind_rows(or_tables)
        intercept_levels <- unique(or_long_all$Level[ifelse(is.na(or_long_all$is_intercept), FALSE, or_long_all$is_intercept)])
        if (length(intercept_levels)) {
          res$model_df_ORs <- res$model_df_ORs[!res$model_df_ORs$Level %in% intercept_levels, , drop = FALSE]
        }
      }
    }

    # --------- plot tables (keep [reference]) ---------
    tidy_plot_or <- dplyr::bind_rows(res$adj_or_outputs, .id = "adjustment_label")
    if (nrow(tidy_plot_or) && !"adjustment" %in% names(tidy_plot_or)) {
      tidy_plot_or <- dplyr::left_join(tidy_plot_or, adj_map, by = "adjustment_label")
    }

    crude_or_plot <- NULL
    if (isTRUE(include_crude) && !is.null(res$crude_model_OR) && nrow(res$crude_model_OR)) {
      crude_or_plot <- res$crude_model_OR
      if (nrow(crude_or_plot)) {
        crude_or_plot$model            <- "crude"
        crude_or_plot$adjustment_label <- "crude"
        crude_or_plot$adjustment       <- 0L
      }
    }
    if (!is.null(crude_or_plot) && nrow(crude_or_plot)) tidy_plot_or <- dplyr::bind_rows(crude_or_plot, tidy_plot_or)
    if (!"adjustment" %in% names(tidy_plot_or) && nrow(tidy_plot_or)) tidy_plot_or$adjustment <- ifelse(tidy_plot_or$model == "crude", 0L, NA_integer_)
    if (isTRUE(remove_intercept_from_results) && nrow(tidy_plot_or) && "is_intercept" %in% names(tidy_plot_or)) {
      tidy_plot_or <- tidy_plot_or[!tidy_plot_or$is_intercept, , drop = FALSE]
    }

    tidy_plot_rd <- dplyr::bind_rows(res$adj_rd_outputs, .id = "adjustment_label")
    if (nrow(tidy_plot_rd) && !"adjustment" %in% names(tidy_plot_rd)) {
      tidy_plot_rd <- dplyr::left_join(tidy_plot_rd, adj_map, by = "adjustment_label")
    }

    crude_rd_plot <- NULL
    if (isTRUE(include_rd) && isTRUE(include_crude) && !is.null(res$crude_model_RD) && nrow(res$crude_model_RD)) {
      crude_rd_plot <- res$crude_model_RD
      if (nrow(crude_rd_plot)) {
        crude_rd_plot$model            <- "crude"
        crude_rd_plot$adjustment_label <- "crude"
        crude_rd_plot$adjustment       <- 0L
      }
    }
    if (!is.null(crude_rd_plot) && nrow(crude_rd_plot)) tidy_plot_rd <- dplyr::bind_rows(crude_rd_plot, tidy_plot_rd)
    if (!"adjustment" %in% names(tidy_plot_rd) && nrow(tidy_plot_rd)) tidy_plot_rd$adjustment <- ifelse(tidy_plot_rd$model == "crude", 0L, NA_integer_)

    out <- list(tidy_res_or  = res$model_df_ORs,
                tidy_plot_or = tidy_plot_or,
                tidy_res_rd  = res$model_df_RDs,
                tidy_plot_rd = tidy_plot_rd,
                diagnostics  = all_diag)

    if (isTRUE(save_immediately) && !is.null(savepath_runs)) {
      var_id  <- .sanitize_var_id(pred_name)
      fmap    <- .var_filemap(savepath_runs, var_id, include_rd = include_rd)
      .save_var_outputs(fmap, out, diagnostics = all_diag, write_csv = TRUE)
    }
    out
  }

  # ---------- checkpoint scan (skip completed) ------------------------------
  var_ids  <- vapply(list_of_variables_of_interest, .sanitize_var_id, FUN.VALUE = character(1))
  completed <- logical(length(var_ids))
  if (!is.null(savepath_runs)) {
    for (i in seq_along(var_ids)) {
      completed[i] <- .has_complete_outputs(.var_filemap(savepath_runs, var_ids[i], include_rd = include_rd))
    }
  } else {
    completed[] <- FALSE
  }
  done_vars <- list_of_variables_of_interest[completed]
  todo_vars <- list_of_variables_of_interest[!completed]
  if (length(done_vars)) message("Skipping already-completed variables: ", paste(done_vars, collapse = ", "))
  if (length(todo_vars)) message("Running remaining variables: ", paste(todo_vars, collapse = ", "))

  # ---------- load pre-existing --------------------------------------------
  preloaded <- list()
  if (length(done_vars) && !is.null(savepath_runs)) {
    for (v in done_vars) preloaded[[v]] <- .load_var_outputs(.var_filemap(savepath_runs, .sanitize_var_id(v), include_rd = include_rd))
  }

  # ---------- run outstanding with robust parallel + restart ----------------
  computed <- .run_todo_with_restarts(todo_vars,
                                      single_var_runner = single_var_runner,
                                      savepath_runs = savepath_runs,
                                      include_rd = include_rd,
                                      ncores = ncores,
                                      parallel_restarts = parallel_restarts,
                                      shrink_workers_on_retry = TRUE,
                                      retry_sleep = retry_sleep)

  # ---------- assemble final outputs ---------------------------------------
  results_by_raw <- preloaded
  for (v in names(computed)) results_by_raw[[v]] <- computed[[v]]
  for (v in list_of_variables_of_interest) {
    if (is.null(results_by_raw[[v]])) {
      results_by_raw[[v]] <- list(tidy_res_or  = data.frame(),
                                  tidy_plot_or = data.frame(),
                                  tidy_res_rd  = data.frame(),
                                  tidy_plot_rd = data.frame(),
                                  diagnostics  = data.frame())
    }
  }

  pretty_names <- vapply(list_of_variables_of_interest, get_pretty_name,
                         FUN.VALUE = character(1), dat = dat, name_fun = name_fun,
                         auto_pretty = auto_pretty)
  ordered_results <- lapply(list_of_variables_of_interest, function(v) results_by_raw[[v]])
  names(ordered_results) <- pretty_names

  out_df_or   <- dplyr::bind_rows(lapply(ordered_results, `[[`, "tidy_res_or"),   .id = "Variable")
  out_plot_or <- dplyr::bind_rows(lapply(ordered_results, `[[`, "tidy_plot_or"),  .id = "Variable")
  out_df_rd   <- dplyr::bind_rows(lapply(ordered_results, `[[`, "tidy_res_rd"),   .id = "Variable")
  out_plot_rd <- dplyr::bind_rows(lapply(ordered_results, `[[`, "tidy_plot_rd"),  .id = "Variable")
  out_diag    <- dplyr::bind_rows(lapply(ordered_results, `[[`, "diagnostics"),   .id = "Variable")

  if (family == "gaussian") {
    if ("OR" %in% names(out_plot_or))    out_plot_or <- dplyr::rename(out_plot_or,  Beta = OR)
    if ("crude_mod_OR" %in% names(out_df_or)) out_df_or <- dplyr::rename(out_df_or, crude_mod_Beta = crude_mod_OR)
  }

  if (remove_intercept_from_results) {
    if ("is_intercept" %in% names(out_plot_or)) out_plot_or <- out_plot_or[!out_plot_or$is_intercept, , drop = FALSE]
    if ("is_intercept" %in% names(out_plot_rd)) out_plot_rd <- out_plot_rd[!out_plot_rd$is_intercept, , drop = FALSE]
  }

  if ("Level" %in% names(out_df_or))   out_df_or   <- dplyr::rename(out_df_or,   Category = Level)
  if ("Level" %in% names(out_plot_or)) out_plot_or <- dplyr::rename(out_plot_or, Category = Level)
  if (nrow(out_df_rd) && "Level" %in% names(out_df_rd))   out_df_rd   <- dplyr::rename(out_df_rd,   Category = Level)
  if (nrow(out_plot_rd) && "Level" %in% names(out_plot_rd)) out_plot_rd <- dplyr::rename(out_plot_rd, Category = Level)

  .repl_na_str <- function(x) { x[x == "NA (NA,NA)"] <- "-"; x }
  if (nrow(out_df_or)) out_df_or[] <- lapply(out_df_or, function(col) if (is.character(col)) .repl_na_str(col) else col)
  if (nrow(out_df_rd)) out_df_rd[] <- lapply(out_df_rd, function(col) if (is.character(col)) .repl_na_str(col) else col)
  out_df_or[is.na(out_df_or)] <- "-"

  if (!is.null(savepath)) {
    .atomic_write_csv(out_df_or,   file.path(savepath, "df_output.csv"))
    .atomic_write_csv(out_plot_or, file.path(savepath, "plot_output.csv"))
    if (nrow(out_df_rd))   .atomic_write_csv(out_df_rd,   file.path(savepath, "df_output_RDs.csv"))
    if (nrow(out_plot_rd)) .atomic_write_csv(out_plot_rd, file.path(savepath, "plot_output_RDs.csv"))
    .atomic_write_csv(out_diag,    file.path(savepath, "diagnostics.csv"))
    .atomic_write_rds(out_df_or,   file.path(savepath, "df_output.rds"))
    .atomic_write_rds(out_plot_or, file.path(savepath, "plot_output.rds"))
    if (nrow(out_df_rd))   .atomic_write_rds(out_df_rd,   file.path(savepath, "df_output_RDs.rds"))
    if (nrow(out_plot_rd)) .atomic_write_rds(out_plot_rd, file.path(savepath, "plot_output_RDs.rds"))
    .atomic_write_rds(out_diag,    file.path(savepath, "diagnostics.rds"))
  }

  list(df_output       = out_df_or,
       plot_output     = out_plot_or,
       df_output_RDs   = if (nrow(out_df_rd)) out_df_rd else NULL,
       plot_output_RDs = if (nrow(out_plot_rd)) out_plot_rd else NULL,
       diagnostics     = out_diag)
}
