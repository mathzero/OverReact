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

# =============================================================================
# NEW HELPER: Validate factor levels after subsetting -------------------------
# =============================================================================
validate_factor_levels <- function(data, var_names) {
  issues <- list()
  for (var in var_names) {
    if (var %in% names(data) && is.factor(data[[var]])) {
      # Report empty levels before dropping them; afterwards none can remain.
      level_counts <- table(data[[var]])
      if (any(level_counts == 0)) {
        issues[[var]] <- paste("Zero counts in levels:",
                               paste(names(level_counts)[level_counts == 0], collapse = ", "))
      }
      data[[var]] <- droplevels(data[[var]])
      if (length(levels(data[[var]])) < 2) {
        issues[[var]] <- c(issues[[var]], "Factor has less than 2 levels after subsetting")
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
      measure = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  rownames(tab) <- NULL
  tab
}

# =============================================================================
# 4. Marginal risk-difference and risk-ratio table builders --------------------
# =============================================================================
# Marginal (standardised) contrasts by g-computation over the analysis sample, for
# binomial and (modified) Poisson GLMs of a binary outcome. Factor exposures compare the
# mean risk with every row set to each level against every row set to the reference
# level, as a difference (RD) or a ratio (RR). Numeric exposures give the effect of a
# one-unit increase: the average marginal effect for RD, and exp(d log(mean risk) / dx)
# for RR, both by central finite difference so that splines and interactions are handled
# too. Intervals use the delta method (on the log scale for RR) by default;
# ci_method = "simulation" uses n_sim multivariate-normal coefficient draws instead.
# dp = NULL returns unrounded estimates.
.react_marginal_table <- function(mod, variable_name, measure = c("RD", "RR"), ref_level = NULL,
                                  dp = 4, data = NULL, n_sim = 100, seed = 1,
                                  ci_method = c("delta", "simulation")) {
  measure <- match.arg(measure)
  ci_method <- match.arg(ci_method)
  na_tbl <- stats::setNames(
    data.frame(NA_character_, NA_real_, NA_real_, NA_real_, NA_real_, stringsAsFactors = FALSE),
    c("Level", measure, "Lower", "Upper", "P_value")
  )
  if (is.null(mod) || inherits(mod, "try-error")) return(na_tbl)
  family_ok <- tryCatch(mod$family$family %in% c("binomial", "poisson"),
                        error = function(e) FALSE)
  if (!isTRUE(family_ok)) return(na_tbl)
  if (is.null(data)) data <- model.frame(mod)
  if (!variable_name %in% names(data)) return(na_tbl)

  # Check for valid coefficients and vcov matrix (robust, if the fit has one)
  beta_hat <- coef(mod)
  vcov_mat <- tryCatch(.react_vcov(mod), error = function(e) NULL)
  if (is.null(vcov_mat) || anyNA(vcov_mat) || anyNA(beta_hat)) {
    warning("Invalid variance-covariance matrix for ", measure, " calculation")
    return(na_tbl)
  }

  # Standardise over rows that could have entered the fit, so passing the full dataset
  # does not add rows the model never saw.
  model_vars <- intersect(all.vars(formula(mod)), names(data))
  if (length(model_vars)) {
    data <- data[complete.cases(data[, model_vars, drop = FALSE]), , drop = FALSE]
  }
  if (!nrow(data)) return(na_tbl)

  tt <- delete.response(terms(mod))
  get_X <- function(nd) {
    model.matrix(tt, data = nd, xlev = mod$xlevels, contrasts.arg = mod$contrasts)
  }
  linkinv <- mod$family$linkinv
  mu_eta <- mod$family$mu.eta
  beta_sim <- NULL
  if (identical(ci_method, "simulation")) {
    # with_seed() keeps the draws reproducible without resetting the caller's RNG stream.
    beta_sim <- tryCatch(
      withr::with_seed(seed, matrix(MASS::mvrnorm(n_sim, mu = beta_hat, Sigma = vcov_mat),
                                    nrow = n_sim)),
      error = function(e) NULL
    )
    if (is.null(beta_sim)) return(na_tbl)
  }

  # Contrast of mean risk between two counterfactual design matrices, per `scale` units:
  # a difference for RD, a difference of logs (exponentiated at the end) for RR.
  contrast <- function(risk1, risk0) if (measure == "RD") risk1 - risk0 else log(risk1) - log(risk0)
  contrast_row <- function(level, X1, X0, scale = 1) {
    eta1 <- drop(X1 %*% beta_hat)
    eta0 <- drop(X0 %*% beta_hat)
    risk1 <- mean(linkinv(eta1))
    risk0 <- mean(linkinv(eta0))
    est <- contrast(risk1, risk0) / scale
    if (is.null(beta_sim)) {
      grad1 <- colMeans(mu_eta(eta1) * X1)
      grad0 <- colMeans(mu_eta(eta0) * X0)
      grad <- if (measure == "RD") grad1 - grad0 else grad1 / risk1 - grad0 / risk0
      se <- sqrt(drop(crossprod(grad / scale, vcov_mat %*% (grad / scale))))
      ci <- est + c(-1, 1) * qnorm(0.975) * se
    } else {
      sims <- contrast(colMeans(linkinv(X1 %*% t(beta_sim))),
                       colMeans(linkinv(X0 %*% t(beta_sim)))) / scale
      ci <- quantile(sims, c(.025, .975), na.rm = TRUE, names = FALSE)
      se <- sd(sims, na.rm = TRUE)
    }
    p_value <- 2 * pnorm(-abs(est / se))
    if (measure == "RR") {
      est <- exp(est)
      ci <- exp(ci)
    }
    out <- stats::setNames(
      data.frame(level, est, ci[1], ci[2], p_value, stringsAsFactors = FALSE),
      c("Level", measure, "Lower", "Upper", "P_value")
    )
    if (!is.null(dp)) {
      out[c(measure, "Lower", "Upper")] <- lapply(out[c(measure, "Lower", "Upper")], round, dp)
    }
    out
  }

  exposure <- data[[variable_name]]
  # 1) factor exposure ---------------------------------------------------------
  if (is.factor(exposure) || is.character(exposure)) {
    lvls <- mod$xlevels[[variable_name]]
    if (is.null(lvls)) lvls <- levels(factor(exposure))
    if (is.null(ref_level) || !(ref_level %in% lvls)) ref_level <- lvls[1]
    set_level <- function(lvl) {
      nd <- data
      nd[[variable_name]] <- factor(rep(lvl, nrow(nd)), levels = lvls)
      nd
    }
    X_ref <- get_X(set_level(ref_level))
    out <- lapply(setdiff(lvls, ref_level), function(lvl) {
      contrast_row(lvl, get_X(set_level(lvl)), X_ref)
    })
    if (!length(out)) return(na_tbl)
    return(do.call(rbind, out))
  }
  # 2) numeric exposure: effect of a one-unit increase -------------------------
  if (is.numeric(exposure)) {
    h <- 1e-4 * sd(exposure)
    if (!is.finite(h) || h <= 0) h <- 1e-4
    shift_exposure <- function(delta) {
      nd <- data
      nd[[variable_name]] <- nd[[variable_name]] + delta
      nd
    }
    return(contrast_row(variable_name, get_X(shift_exposure(h / 2)),
                        get_X(shift_exposure(-h / 2)), scale = h))
  }
  na_tbl
}

makeRDTable <- function(mod, variable_name, ref_level = NULL, dp = 4,
                        data = NULL, n_sim = 100, seed = 1,
                        ci_method = c("delta", "simulation")) {
  .react_marginal_table(mod, variable_name, measure = "RD", ref_level = ref_level, dp = dp,
                        data = data, n_sim = n_sim, seed = seed, ci_method = match.arg(ci_method))
}

makeRRTable <- function(mod, variable_name, ref_level = NULL, dp = 4,
                        data = NULL, n_sim = 100, seed = 1,
                        ci_method = c("delta", "simulation")) {
  .react_marginal_table(mod, variable_name, measure = "RR", ref_level = ref_level, dp = dp,
                        data = data, n_sim = n_sim, seed = seed, ci_method = match.arg(ci_method))
}

# ---------------------------------------------------------------------------
# 5. modelMakerSequentialRD() - SEPARATE OR / RD / RR OUTPUTS
# ---------------------------------------------------------------------------
modelMakerSequentialRD <- function(variable_name,
                                   data,
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
                                   sample_strategy         = c("common_per_predictor", "per_model"),
                                   rd_ci_method            = c("delta", "simulation"),
                                   include_rr              = FALSE,
                                   family                  = "auto",
                                   offset                  = NULL) {
  sample_strategy <- match.arg(sample_strategy)
  rd_ci_method <- match.arg(rd_ci_method)

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
    sample_strategy = sample_strategy,
    rd_ci_method = rd_ci_method,
    include_rr = include_rr,
    family = family,
    offset = offset
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
  out$model_df_RRs <- rename_cols(out$model_df_RRs, "_RR")
  out
}


# Non INcremental workers -------------------------------------------------
# =========================
# Helpers (new/updated)
# =========================

# Marginal table with a [reference] row for categorical exposures; NULL (with a message)
# if the calculation fails.
.react_safe_marginal_table <- function(mod, variable_name, measure = "RD", ref_level = NULL,
                                       dp = 4, data = NULL, n_sim = 100, seed = 1,
                                       ci_method = "delta") {
  out <- tryCatch(
    .react_marginal_table(mod = mod, variable_name = variable_name, measure = measure,
                          ref_level = ref_level, dp = dp, data = data, n_sim = n_sim,
                          seed = seed, ci_method = ci_method),
    error = function(e) {
      message(if (measure == "RD") "Risk-difference" else "Risk-ratio", " table failed: ", e$message)
      NULL
    }
  )
  if (is.null(out)) return(NULL)

  # Inject a [reference] row for categorical exposures if missing
  lvls <- tryCatch(mod$xlevels[[variable_name]], error = function(e) NULL)
  if (!is.null(lvls)) {
    if (is.null(ref_level) || !(ref_level %in% lvls)) ref_level <- lvls[1]
    if (!any(grepl("\\[reference\\]$", out$Level))) {
      ref_row <- out[1, , drop = FALSE]
      ref_row$Level <- paste0(ref_level, " [reference]")
      ref_row[c(measure, "Lower", "Upper", "P_value")] <- NA_real_
      out <- rbind(ref_row, out)
    }
  }
  out
}

# Always returns a [reference] row for categorical exposures in RD output
safe_makeRDTable <- function(mod, variable_name, ref_level = NULL, dp = 4,
                             data = NULL, n_sim = 100, seed = 1,
                             ci_method = c("delta", "simulation")) {
  .react_safe_marginal_table(mod, variable_name, measure = "RD", ref_level = ref_level, dp = dp,
                             data = data, n_sim = n_sim, seed = seed,
                             ci_method = match.arg(ci_method))
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

.collect_completed <- function(vars, savepath_runs, tables, spec_hashes = NULL) {
  out <- list()
  if (is.null(savepath_runs) || !length(vars)) return(out)
  for (v in vars) {
    fmap <- .var_filemap(savepath_runs, .sanitize_var_id(v), tables = tables)
    if (.has_complete_outputs(fmap, spec_hash = spec_hashes[[v]])) out[[v]] <- .load_var_outputs(fmap)
  }
  out
}

.run_todo_with_restarts <- function(todo_vars,
                                    single_var_runner,
                                    savepath_runs,
                                    tables,
                                    ncores,
                                    parallel_restarts = 2,
                                    shrink_workers_on_retry = TRUE,
                                    retry_sleep = 1,
                                    spec_hashes = NULL) {
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
    newly <- .collect_completed(remaining, savepath_runs, tables, spec_hashes)
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

# Write to a temporary file beside `path`, then move it into place, so readers never see
# a partly written file. tempfile() names do not touch the session's RNG stream.
.atomic_write <- function(path, write) {
  tmp <- tempfile(pattern = paste0(basename(path), ".tmp-"), tmpdir = dirname(path))
  on.exit({ if (file.exists(tmp)) unlink(tmp) }, add = TRUE)
  write(tmp)
  if (file.rename(tmp, path)) return(invisible(TRUE))
  # Some platforms will not rename over an existing file.
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Could not write '", path, "'.")
  invisible(TRUE)
}

.atomic_write_csv <- function(df, path) {
  .atomic_write(path, function(tmp) readr::write_csv(df, tmp))
}

.atomic_write_rds <- function(obj, path) {
  .atomic_write(path, function(tmp) saveRDS(obj, file = tmp))
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

.coerce_df_types <- function(df, kind = c("df_or","plot_or","df_rd","plot_rd","df_rr","plot_rr","diagnostics")) {
  kind <- match.arg(kind)
  if (!nrow(df)) return(df)
  to_char <- intersect(c("Level","Category","term_raw","model","model_family","measure","outcome_level","adjustment_label","stage","adjusted_vars","status","error_msg","warnings","separation_msg"), names(df))
  for (nm in to_char) df[[nm]] <- as.character(df[[nm]])
  if (startsWith(kind, "plot_")) {
    if ("adjustment" %in% names(df)) df$adjustment <- suppressWarnings(as.integer(df$adjustment))
    num_cols <- c(toupper(sub("^plot_", "", kind)), "Lower", "Upper", "P_value", "Q_value")
    for (nm in intersect(num_cols, names(df))) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  } else if (startsWith(kind, "df_")) {
    if ("Nobs_in_model" %in% names(df)) df$Nobs_in_model <- suppressWarnings(as.integer(df$Nobs_in_model))
  } else if (kind == "diagnostics") {
    int_cols <- intersect(c("nobs","rd_rows","rr_rows","attempt"), names(df))
    for (nm in int_cols) df[[nm]] <- suppressWarnings(as.integer(df[[nm]]))
    if ("vcov_ok" %in% names(df)) df$vcov_ok <- as.logical(df$vcov_ok)
  }
  df
}

# Per-predictor checkpoint files: df_<table> and plot_<table> outputs, as RDS and CSV, for
# each table ("or" for coefficients, "rd" and "rr" for marginal effects), plus the
# diagnostics and the specification fingerprint.
.var_filemap <- function(savepath_runs, var_id, tables = c("or", "rd")) {
  var_dir  <- file.path(savepath_runs, var_id)
  files <- list(
    dir      = var_dir,
    tables   = tables,
    diag_csv = file.path(var_dir, "diagnostics.csv"),
    diag_rds = file.path(var_dir, "diagnostics.rds"),
    spec_rds = file.path(var_dir, "spec.rds")
  )
  for (tab in tables) for (kind in c("df", "plot")) for (ext in c("csv", "rds")) {
    files[[paste0(kind, "_", tab, "_", ext)]] <- file.path(var_dir, paste0(kind, "_", tab, ".", ext))
  }
  files
}

.var_table_files <- function(filemap, ext) {
  keys <- paste0(rep(c("df_", "plot_"), times = length(filemap$tables)),
                 rep(filemap$tables, each = 2L), "_", ext)
  unlist(filemap[keys], use.names = FALSE)
}

# With spec_hash, outputs only count as complete if they were saved for the same model
# specification and data; otherwise a changed run would silently reuse stale results.
.has_complete_outputs <- function(filemap, spec_hash = NULL) {
  if (!is.null(spec_hash) && !identical(.robust_read_rds(filemap$spec_rds), spec_hash)) {
    return(FALSE)
  }
  complete <- function(paths) {
    length(paths) > 0L && all(file.exists(paths)) && all(file.info(paths)$size > 0)
  }
  complete(.var_table_files(filemap, "rds")) || complete(.var_table_files(filemap, "csv"))
}

.save_var_outputs <- function(filemap, out, diagnostics = NULL, write_csv = TRUE,
                              spec_hash = NULL) {
  .safe_dir_create(filemap$dir)
  # The spec file is written last, so an interrupted save never looks complete.
  if (file.exists(filemap$spec_rds)) unlink(filemap$spec_rds)
  for (tab in filemap$tables) {
    for (kind in c("df", "plot")) {
      obj <- out[[paste0(if (kind == "df") "tidy_res_" else "tidy_plot_", tab)]]
      .atomic_write_rds(obj, filemap[[paste0(kind, "_", tab, "_rds")]])
      if (isTRUE(write_csv) && !is.null(obj) && nrow(obj)) {
        .atomic_write_csv(obj, filemap[[paste0(kind, "_", tab, "_csv")]])
      }
    }
  }
  if (!is.null(diagnostics)) {
    .atomic_write_rds(diagnostics, filemap$diag_rds)
    if (isTRUE(write_csv) && nrow(diagnostics)) .atomic_write_csv(diagnostics, filemap$diag_csv)
  }
  if (!is.null(spec_hash)) .atomic_write_rds(spec_hash, filemap$spec_rds)
  invisible(TRUE)
}

.load_var_outputs <- function(filemap) {
  keys <- paste0(rep(c("df_", "plot_"), times = length(filemap$tables)),
                 rep(filemap$tables, each = 2L))
  tables <- lapply(keys, function(key) .robust_read_rds(filemap[[paste0(key, "_rds")]]))
  names(tables) <- keys
  diag_df <- .robust_read_rds(filemap$diag_rds)

  # Fall back to the CSV copies, and cache them as RDS.
  if (any(vapply(tables, is.null, logical(1)))) {
    tables <- lapply(keys, function(key) .coerce_df_types(.robust_read_csv(filemap[[paste0(key, "_csv")]]), key))
    names(tables) <- keys
    diag_df <- .coerce_df_types(.robust_read_csv(filemap$diag_csv), "diagnostics")
    .safe_dir_create(filemap$dir)
    for (key in keys) .atomic_write_rds(tables[[key]], filemap[[paste0(key, "_rds")]])
    if (!is.null(diag_df) && nrow(diag_df)) .atomic_write_rds(diag_df, filemap$diag_rds)
  }

  as_df <- function(x) as.data.frame(if (is.null(x)) data.frame() else x, stringsAsFactors = FALSE)
  out <- list()
  for (tab in filemap$tables) {
    out[[paste0("tidy_res_", tab)]] <- as_df(tables[[paste0("df_", tab)]])
    out[[paste0("tidy_plot_", tab)]] <- as_df(tables[[paste0("plot_", tab)]])
  }
  out$diagnostics <- as_df(diag_df)
  out
}

# =============================================================================

# modelMakerAdjustedSetsRD(): ensures level-order & clean plot lis --------

# ======================================================================
# Diagnostics-aware adjusted-sets runner - now keeps reference in plots
# ======================================================================
modelMakerAdjustedSetsRD <- function(variable_name,
                                     data,
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
                                     sample_strategy         = c("common_per_predictor", "per_model"),
                                     rd_ci_method            = c("delta", "simulation"),
                                     include_rr              = FALSE,
                                     family                  = "auto",
                                     offset                  = NULL) {
  sample_strategy <- match.arg(sample_strategy)
  rd_ci_method <- match.arg(rd_ci_method)

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
      adjustment = i
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
    ref_level = ref_level,
    family = family,
    offset = offset
  )

  or_rows <- runner$rows
  stage_cols <- vapply(stage_specs, `[[`, character(1), "wide_col")
  df_or <- .react_wide_from_long(
    long_df = or_rows,
    effect_col = "OR",
    sf = sf,
    format = format,
    simpleround = simpleround,
    include_nobs = identical(sample_strategy, "common_per_predictor"),
    wide_cols = stage_cols
  )

  # Marginal risk differences and risk ratios, which need a binary outcome.
  marginal_rows <- list(RD = data.frame(), RR = data.frame())
  marginal_wide <- list(RD = NULL, RR = NULL)
  for (m in c(if (isTRUE(include_rd)) "RD", if (isTRUE(include_rr)) "RR")) {
    if (!isTRUE(runner$spec$binary)) break
    marginal_rows[[m]] <- dplyr::bind_rows(lapply(
      runner$stages, .react_extract_rd_rows, predictor = variable_name,
      n_sim = n_sim, dp = NULL, ci_method = rd_ci_method, measure = m
    ))
    if (nrow(marginal_rows[[m]])) {
      marginal_wide[[m]] <- .react_wide_from_long(
        long_df = marginal_rows[[m]],
        effect_col = m,
        sf = if (m == "RD") sf + 1L else sf,
        format = format,
        simpleround = simpleround,
        include_nobs = identical(sample_strategy, "common_per_predictor"),
        wide_cols = sub("_OR$", paste0("_", m), stage_cols)
      )
    }
  }

  diag_df <- runner$diagnostics
  if (nrow(diag_df)) {
    count_rows <- function(rows) {
      if (!nrow(rows)) return(integer(nrow(diag_df)))
      counts <- tapply(rows$Level, rows$stage_id, length)
      counts <- unname(counts[match(diag_df$stage, names(counts))])
      counts[is.na(counts)] <- 0L
      as.integer(counts)
    }
    diag_df$rd_rows <- count_rows(marginal_rows$RD)
    if (isTRUE(include_rr)) diag_df$rr_rows <- count_rows(marginal_rows$RR)
  }

  stage_rows <- function(rows, stage_id) {
    if (!nrow(rows) || !"stage_id" %in% names(rows)) return(rows[0, , drop = FALSE])
    rows[rows$stage_id == stage_id, , drop = FALSE]
  }
  crude_rows <- function(rows, wanted = TRUE) {
    if (isTRUE(wanted) && isTRUE(include_crude) && nrow(rows)) stage_rows(rows, "crude") else NULL
  }
  adjusted_rows <- function(rows) {
    stats::setNames(lapply(names(adj_sets), function(adj_name) stage_rows(rows, adj_name)), names(adj_sets))
  }

  stage_nobs <- vapply(runner$stages, function(stage) {
    if (is.null(stage$data)) 0L else nrow(stage$data)
  }, integer(1))

  list(
    model_df_ORs = df_or,
    model_df_RDs = if (isTRUE(include_rd)) marginal_wide$RD else NULL,
    model_df_RRs = if (isTRUE(include_rr)) marginal_wide$RR else NULL,
    crude_model_OR = crude_rows(or_rows),
    crude_model_RD = crude_rows(marginal_rows$RD, include_rd),
    crude_model_RR = crude_rows(marginal_rows$RR, include_rr),
    adj_or_outputs = adjusted_rows(or_rows),
    adj_rd_outputs = adjusted_rows(marginal_rows$RD),
    adj_rr_outputs = adjusted_rows(marginal_rows$RR),
    diagnostics = diag_df,
    family = runner$spec$name,
    measure = runner$spec$measure,
    n_complete_cases = if (length(stage_nobs)) max(stage_nobs) else 0L,
    n_original_cases = sum(!is.na(data[[outcome]]))
  )
}



# ---------------------------------------------------------------------
# ModelMakerMultiRD() - with robust parallel restart & no hard stop
# ---------------------------------------------------------------------
ModelMakerMultiRD <- function(dat, list_of_variables_of_interest, outcome = "res",
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
                              sample_strategy = c("common_per_predictor", "per_model"),
                              rd_ci_method = c("delta", "simulation"),
                              include_rr = FALSE,
                              family = "auto",
                              offset = NULL) {
  sample_strategy <- match.arg(sample_strategy)
  rd_ci_method <- match.arg(rd_ci_method)

  if (!is.data.frame(dat)) stop("dat must be a data.frame")
  if (!outcome %in% names(dat)) stop(paste("Outcome", outcome, "not found"))

  missing_vars <- setdiff(list_of_variables_of_interest, names(dat))
  if (length(missing_vars) > 0) {
    warning("Variables not found: ", paste(missing_vars, collapse = ", "))
    list_of_variables_of_interest <- setdiff(list_of_variables_of_interest, missing_vars)
  }
  list_of_variables_of_interest <- unique(list_of_variables_of_interest)
  if (length(list_of_variables_of_interest) == 0) stop("No variables to analyse")

  spec <- .react_resolve_family(dat[[outcome]], family = family, outcome = outcome)
  .react_check_offset(offset, spec, dat)
  message(if (identical(family, "auto")) "Assuming " else "Fitting ", spec$label, " models.")
  # Marginal effects need a binary outcome; other families skip them.
  marginal <- if (isTRUE(spec$binary)) c(if (isTRUE(include_rd)) "RD", if (isTRUE(include_rr)) "RR") else character(0)
  tables <- c("or", if (isTRUE(include_rd)) "rd", if (isTRUE(include_rr)) "rr")

  # --- save paths (RDS-first checkpointing assumed available) --------------
  savepath_runs <- NULL
  if (!is.null(savepath)) {
    savepath_runs <- file.path(savepath, "runs")
    if (!.safe_dir_create(savepath_runs)) {
      warning("Could not create '", savepath_runs, "'. Checkpointing disabled.")
      savepath_runs <- NULL
    }
  }

  sets_for <- function(pred_name) {
    pred_adjusters <- setdiff(joint_adjustment_vars, pred_name)
    if (isTRUE(incremental)) {
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
  }

  # --- checkpoint fingerprints ---------------------------------------------
  # Everything that determines a predictor's results, including its data columns, so
  # checkpoints are reused only for an identical specification and identical data.
  # Hashes are computed once here, not on parallel workers.
  spec_hash_for <- function(pred_name) {
    sets <- sets_for(pred_name)
    cols <- intersect(unique(c(outcome, pred_name, unlist(sets, use.names = FALSE), offset)), names(dat))
    rlang::hash(list(
      outcome = outcome, predictor = pred_name, sets = sets, family = spec$name, offset = offset,
      include_crude = include_crude, include_rd = include_rd, include_rr = include_rr,
      n_sim = n_sim, rd_ci_method = rd_ci_method, sample_strategy = sample_strategy,
      glm_control = glm_control, retry_failed = retry_failed, max_retries = max_retries,
      sf = sf, format = format, simpleround = simpleround,
      remove_intercept_from_results = remove_intercept_from_results,
      data = stats::setNames(lapply(cols, function(v) dat[[v]]), cols)
    ))
  }
  spec_hashes <- NULL
  if (!is.null(savepath_runs)) {
    spec_hashes <- vapply(list_of_variables_of_interest, spec_hash_for, character(1))
  }

  empty_result <- function(diagnostics = data.frame()) {
    out <- list()
    for (tab in tables) {
      out[[paste0("tidy_res_", tab)]] <- data.frame()
      out[[paste0("tidy_plot_", tab)]] <- data.frame()
    }
    out$diagnostics <- diagnostics
    out
  }

  # ---------- per-variable worker (unchanged modelling logic) --------------
  single_var_runner <- function(pred_name, save_immediately = TRUE) {
    ref_lv <- if (is.factor(dat[[pred_name]]) && !is.ordered(dat[[pred_name]])) levels(dat[[pred_name]])[1] else NULL
    sets <- sets_for(pred_name)

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
                                 sample_strategy = sample_strategy,
                                 rd_ci_method = rd_ci_method,
                                 include_rr = include_rr,
                                 family = spec$name,
                                 offset = offset),
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

    # retry (model-level) if any expected table has gaps
    has_rows <- function(x) !is.null(x) && nrow(x) > 0
    has_gaps <- function(res) {
      if (is.null(res)) return(TRUE)
      for (m in c("OR", marginal)) {
        if (isTRUE(include_crude) && !has_rows(res[[paste0("crude_model_", m)]])) return(TRUE)
        adjusted <- res[[paste0("adj_", tolower(m), "_outputs")]]
        if (!all(names(sets) %in% names(adjusted)[vapply(adjusted, has_rows, logical(1))])) return(TRUE)
      }
      FALSE
    }

    need_retry <- isTRUE(retry_failed) && has_gaps(a1$res) && max_retries >= 1L
    if (need_retry) {
      message("Retrying variable '", pred_name, "' with stricter glm.control ...")
      ctrl2 <- glm_control
      if (is.null(ctrl2$maxit) || !is.finite(ctrl2$maxit)) ctrl2$maxit <- 50
      ctrl2$maxit   <- max(100L, as.integer(ctrl2$maxit * 2L))
      ctrl2$epsilon <- if (is.null(ctrl2$epsilon)) 1e-8 else min(1e-8, ctrl2$epsilon / 10)
      a2 <- run_once(ctrl2, attempt = 2L)
      if (!is.null(a2$diag) && nrow(a2$diag)) all_diag <- dplyr::bind_rows(all_diag, .coerce_df_types(a2$diag, "diagnostics"))
      if (!is.null(a2$res)) a1$res <- a2$res
    }

    if (is.null(a1$res)) {
      message("Variable ", pred_name, ": failed completely after retries.")
      return(empty_result(all_diag))
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
    plot_table <- function(m) {
      plot_df <- dplyr::bind_rows(res[[paste0("adj_", tolower(m), "_outputs")]], .id = "adjustment_label")
      if (nrow(plot_df) && !"adjustment" %in% names(plot_df)) {
        plot_df <- dplyr::left_join(plot_df, adj_map, by = "adjustment_label")
      }
      crude <- res[[paste0("crude_model_", m)]]
      if (isTRUE(include_crude) && has_rows(crude)) {
        crude$model            <- "crude"
        crude$adjustment_label <- "crude"
        crude$adjustment       <- 0L
        plot_df <- dplyr::bind_rows(crude, plot_df)
      }
      if (!"adjustment" %in% names(plot_df) && nrow(plot_df)) {
        plot_df$adjustment <- ifelse(plot_df$model == "crude", 0L, NA_integer_)
      }
      if (isTRUE(remove_intercept_from_results) && nrow(plot_df) && "is_intercept" %in% names(plot_df)) {
        plot_df <- plot_df[!plot_df$is_intercept, , drop = FALSE]
      }
      plot_df
    }

    out <- list(tidy_res_or = res$model_df_ORs, tidy_plot_or = plot_table("OR"))
    if (isTRUE(include_rd)) {
      out$tidy_res_rd <- res$model_df_RDs
      out$tidy_plot_rd <- plot_table("RD")
    }
    if (isTRUE(include_rr)) {
      out$tidy_res_rr <- res$model_df_RRs
      out$tidy_plot_rr <- plot_table("RR")
    }
    out$diagnostics <- all_diag

    if (isTRUE(save_immediately) && !is.null(savepath_runs)) {
      var_id  <- .sanitize_var_id(pred_name)
      fmap    <- .var_filemap(savepath_runs, var_id, tables = tables)
      .save_var_outputs(fmap, out, diagnostics = all_diag, write_csv = TRUE,
                        spec_hash = spec_hashes[[pred_name]])
    }
    out
  }

  # ---------- checkpoint scan (skip completed) ------------------------------
  var_ids  <- vapply(list_of_variables_of_interest, .sanitize_var_id, FUN.VALUE = character(1))
  completed <- logical(length(var_ids))
  if (!is.null(savepath_runs)) {
    for (i in seq_along(var_ids)) {
      fmap <- .var_filemap(savepath_runs, var_ids[i], tables = tables)
      completed[i] <- .has_complete_outputs(fmap, spec_hash = spec_hashes[[i]])
      if (!completed[i] && .has_complete_outputs(fmap)) {
        message("Checkpoint for '", list_of_variables_of_interest[i],
                "' was saved for a different specification or data; recomputing.")
      }
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
    for (v in done_vars) preloaded[[v]] <- .load_var_outputs(.var_filemap(savepath_runs, .sanitize_var_id(v), tables = tables))
  }

  # ---------- run outstanding with robust parallel + restart ----------------
  computed <- .run_todo_with_restarts(todo_vars,
                                      single_var_runner = single_var_runner,
                                      savepath_runs = savepath_runs,
                                      tables = tables,
                                      ncores = ncores,
                                      parallel_restarts = parallel_restarts,
                                      shrink_workers_on_retry = TRUE,
                                      retry_sleep = retry_sleep,
                                      spec_hashes = spec_hashes)

  # ---------- assemble final outputs ---------------------------------------
  results_by_raw <- preloaded
  for (v in names(computed)) results_by_raw[[v]] <- computed[[v]]
  for (v in list_of_variables_of_interest) {
    if (is.null(results_by_raw[[v]])) results_by_raw[[v]] <- empty_result()
  }

  pretty_names <- .react_variable_labels(list_of_variables_of_interest, dat,
                                         name_map = cov_name_list, name_fun = name_fun,
                                         auto_pretty = auto_pretty)
  ordered_results <- lapply(list_of_variables_of_interest, function(v) results_by_raw[[v]])
  names(ordered_results) <- pretty_names

  combine <- function(key) {
    as.data.frame(dplyr::bind_rows(lapply(ordered_results, `[[`, key), .id = "Variable"),
                  stringsAsFactors = FALSE)
  }
  # Formatted (df) and numeric (plot) tables, keyed by measure.
  df_out <- list(OR = combine("tidy_res_or"))
  plot_out <- list(OR = combine("tidy_plot_or"))
  for (m in c(if (isTRUE(include_rd)) "RD", if (isTRUE(include_rr)) "RR")) {
    df_out[[m]] <- combine(paste0("tidy_res_", tolower(m)))
    plot_out[[m]] <- combine(paste0("tidy_plot_", tolower(m)))
  }
  out_diag <- combine("diagnostics")

  # Name coefficient columns after the family's measure (OR, RR, IRR or Beta).
  plot_out$OR <- .react_label_measure(plot_out$OR, spec$measure)
  df_out$OR <- .react_label_measure(df_out$OR, spec$measure, wide = TRUE)

  for (m in names(df_out)) {
    plot_df <- plot_out[[m]]
    if (remove_intercept_from_results && "is_intercept" %in% names(plot_df)) {
      plot_df <- plot_df[!plot_df$is_intercept, , drop = FALSE]
    }
    names(plot_df)[names(plot_df) == "Level"] <- "Category"
    plot_out[[m]] <- plot_df

    df <- df_out[[m]]
    names(df)[names(df) == "Level"] <- "Category"
    names(df)[names(df) == "outcome_level"] <- "Outcome"
    # Stages without an estimate show "-", like reference rows.
    df[] <- lapply(df, function(col) if (is.character(col)) replace(col, is.na(col), "-") else col)
    df_out[[m]] <- df
  }

  output_files <- c(OR = "", RD = "_RDs", RR = "_RRs")
  if (!is.null(savepath)) {
    for (m in names(df_out)) {
      for (kind in c("df", "plot")) {
        tab <- if (kind == "df") df_out[[m]] else plot_out[[m]]
        if (m != "OR" && !nrow(tab)) next
        stem <- file.path(savepath, paste0(kind, "_output", output_files[[m]]))
        .atomic_write_csv(tab, paste0(stem, ".csv"))
        .atomic_write_rds(tab, paste0(stem, ".rds"))
      }
    }
    .atomic_write_csv(out_diag,    file.path(savepath, "diagnostics.csv"))
    .atomic_write_rds(out_diag,    file.path(savepath, "diagnostics.rds"))
  }

  non_empty <- function(x) if (!is.null(x) && nrow(x)) x else NULL
  list(df_output       = df_out$OR,
       plot_output     = plot_out$OR,
       df_output_RDs   = non_empty(df_out$RD),
       plot_output_RDs = non_empty(plot_out$RD),
       df_output_RRs   = non_empty(df_out$RR),
       plot_output_RRs = non_empty(plot_out$RR),
       diagnostics     = out_diag)
}
