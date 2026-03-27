# ======================================================================
# run_auto_tableone_regressions()
# ----------------------------------------------------------------------
# A high-level driver to:
#   1) build an automatic "Table 1" for a set of independent variables
#      (rows) with the outcome as the column if categorical; and
#   2) run your bulk regression engine (ModelMakerMultiRD) with the
#      specified covariates;
#   3) return a short, human-readable report summarising top associations.
#
# Assumes the helper functions you shared (tableOne, ModelMakerMultiRD,
# etc.) are available in the environment.
# ======================================================================

run_auto_tableone_regressions <- function(
    dat,
    outcome,
    covariates,
    independent_variables,
    # ---- Table 1 options -------------------------------------------------
    cov_names = NULL,
    table_summary_stat = c("mean","median"),
    include_percentages = TRUE,
    statistical_test = TRUE,
    includeNAsColvar = F,
    includeNAsRowvar = F,
    formatPvalsForEpiPaper = FALSE,
    addNobsTopRow = TRUE,
    # By default, treat outcome as categorical if factor/character/logical
    # or a numeric with <= 6 unique non-missing values.
    outcome_cat_unique_threshold = 6,
    # ---- Modelling options -----------------------------------------------
    incremental = TRUE,
    adj_sets = NULL,                # ignored if incremental = TRUE
    include_crude = TRUE,
    include_rd = FALSE,              # auto: TRUE for binomial outcome, else FALSE
    ncores = NULL,
    n_sim = 100,
    glm_control = glm.control(maxit = 50),
    # ---- Pretty-printing / naming ----------------------------------------
    name_fun = NULL,                # optional prettifier; passed through to ModelMakerMultiRD
    auto_pretty = TRUE,
    # ---- Output control ---------------------------------------------------
    save_dir = NULL,                # if provided, ModelMakerMultiRD will write CSV/RDS under this folder
    report_path = NULL,             # optional path for a plain-text Markdown report
    top_k = 10                      # number of "top" findings in the short report
) {

  # ---- guards -----------------------------------------------------------
  stopifnot(is.data.frame(dat))
  if (!outcome %in% names(dat)) stop("Outcome not found in 'dat'.")
  if (!length(independent_variables)) stop("'independent_variables' must be a non-empty character vector.")
  missing_IVs <- setdiff(independent_variables, names(dat))
  if (length(missing_IVs)) {
    warning("The following independent variables were not found and will be skipped: ",
            paste(missing_IVs, collapse = ", "))
    independent_variables <- setdiff(independent_variables, missing_IVs)
  }
  if (!length(independent_variables)) stop("No valid independent variables remain to analyse.")

  # ---- small helpers ----------------------------------------------------
  is_outcome_categorical <- function(x) {
    if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
    if (is.numeric(x)) {
      ux <- unique(x[!is.na(x)])
      return(length(ux) <= outcome_cat_unique_threshold)
    }
    FALSE
  }
  fmt_p <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 1e-4) return("<1e-4")
    if (p < 0.001) return(sprintf("<%.3f", 0.001))
    sprintf("%.4f", p)
  }
  fmt_ci <- function(lo, hi, digits = 2) sprintf("(%.2f, %.2f)", round(lo, digits), round(hi, digits))
  fmt_rd <- function(x, digits = 3) ifelse(is.na(x), "NA", sprintf("%.3f", x))
  coalesce_chr <- function(x, y) ifelse(is.na(x) | x == "", y, x)

  # Determine outcome family & RD default
  non_na_y <- dat[[outcome]][!is.na(dat[[outcome]])]
  is_binom <- length(unique(non_na_y)) == 2L
  if (is.null(include_rd)) include_rd <- is_binom

  # ---- 1) Build Table 1 -------------------------------------------------
  table_summary_stat <- match.arg(table_summary_stat)
  use_colvar <- NULL
  if (is_outcome_categorical(dat[[outcome]])) use_colvar <- outcome

  t1 <- tableOne(
    dat                      = dat,
    rowvars                  = independent_variables,
    colvar                   = use_colvar,
    cov_names                = cov_names,
    confint                  = FALSE,
    include_percentages      = include_percentages,
    rowwise_precentages      = TRUE,
    summary_stat             = table_summary_stat,
    comma_thousands          = FALSE,
    statistical_test         = statistical_test,
    includeNAsColvar         = includeNAsColvar,
    includeNAsRowvar         = includeNAsRowvar,
    formatPvalsForEpiPaper   = formatPvalsForEpiPaper,
    addNobsTopRow            = addNobsTopRow
  )

  # ---- 2) Bulk regression via your engine --------------------------------
  mm <- ModelMakerMultiRD(
    dat                         = dat,
    list_of_variables_of_interest = independent_variables,
    outcome                     = outcome,
    sf                          = 2,
    format                      = "f",
    simpleround                 = FALSE,
    joint_adjustment_vars       = covariates,
    adj_sets                    = adj_sets,
    incremental                 = incremental,
    include_crude               = include_crude,
    cov_name_list               = cov_names,
    name_fun                    = name_fun,
    auto_pretty                 = auto_pretty,
    remove_intercept_from_results = TRUE,
    ncores                      = ncores,
    include_rd                  = include_rd,
    n_sim                       = n_sim,
    savepath                    = save_dir,
    glm_control                 = glm_control,
    retry_failed                = TRUE,
    max_retries                 = 1,
    parallel_restarts           = 2,
    retry_sleep                 = 1
  )

  # mm elements we'll use
  df_or_wide   <- mm$df_output            # wide strings for OR/Beta
  df_rd_wide   <- mm$df_output_RDs        # wide strings for RD
  plot_or_long <- mm$plot_output          # numeric OR/Beta, CI, P_value
  plot_rd_long <- mm$plot_output_RDs      # numeric RD, CI, P_value
  diag_df      <- mm$diagnostics

  # ---- 3) Build "short report" ------------------------------------------
  # Strategy:
  #  * Identify the "fully adjusted" rows in plot_* (max adjustment count).
  #  * For each Variable, pick the level with the smallest p-value (excluding reference rows).
  #  * Summarise top_k by p-value (ORs/Beta and RDs if available).

  # Determine which adjustment is "full"
  full_adj <- NA_integer_
  if (!is.null(plot_or_long) && nrow(plot_or_long)) {
    if ("adjustment" %in% names(plot_or_long)) {
      full_adj <- suppressWarnings(max(plot_or_long$adjustment, na.rm = TRUE))
      if (!is.finite(full_adj)) full_adj <- NA_integer_
    }
  }
  if (is.na(full_adj) && !is.null(plot_rd_long) && nrow(plot_rd_long)) {
    if ("adjustment" %in% names(plot_rd_long)) {
      full_adj <- suppressWarnings(max(plot_rd_long$adjustment, na.rm = TRUE))
      if (!is.finite(full_adj)) full_adj <- NA_integer_
    }
  }

  # Filter to full adjustment; include crude if nothing else exists
  filter_full <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    d <- df
    # remove reference lines if present
    if ("is_reference" %in% names(d)) {
      d <- d[!ifelse(is.na(d$is_reference), FALSE, d$is_reference), , drop = FALSE]
    } else if ("Category" %in% names(d)) {
      d <- d[!grepl("\\[reference\\]$", d$Category), , drop = FALSE]
    }
    if ("is_intercept" %in% names(d)) {
      d <- d[!ifelse(is.na(d$is_intercept), FALSE, d$is_intercept), , drop = FALSE]
    }
    if (!is.na(full_adj) && "adjustment" %in% names(d)) {
      d_full <- d[is.finite(d$adjustment) & d$adjustment == full_adj, , drop = FALSE]
      if (nrow(d_full)) return(d_full)
    }
    # fallback: crude only
    if ("model" %in% names(d)) {
      d_crude <- d[d$model == "crude", , drop = FALSE]
      if (nrow(d_crude)) return(d_crude)
    }
    d
  }

  full_or <- filter_full(plot_or_long)
  full_rd <- filter_full(plot_rd_long)

  # Per-variable "best" (smallest p) rows
  best_by_var <- function(df, effect_col = c("OR","Beta","RD")) {
    if (is.null(df) || !nrow(df) || !"Variable" %in% names(df) || !"P_value" %in% names(df)) {
      return(df[0, , drop = FALSE])
    }
    effect_col <- match.arg(effect_col)
    d <- df
    # Choose the right effect column name present in df
    if (!(effect_col %in% names(d))) {
      # Switch OR/Beta if gaussian relabelling happened
      if (effect_col == "OR" && "Beta" %in% names(d)) effect_col <- "Beta"
      if (effect_col == "RD" && !"RD" %in% names(d)) return(d[0, , drop = FALSE])
    }
    # Keep the lowest p row per variable
    o <- do.call(rbind, lapply(split(d, d$Variable), function(z) {
      z <- z[order(z$P_value, na.last = TRUE), , drop = FALSE]
      z[1, , drop = FALSE]
    }))
    rownames(o) <- NULL
    o
  }

  best_or <- best_by_var(full_or, effect_col = "OR")  # will use Beta if gaussian
  best_rd <- best_by_var(full_rd, effect_col = "RD")

  # Order by p-value and limit to top_k
  top_or <- if (!is.null(best_or) && nrow(best_or)) best_or[order(best_or$P_value), , drop = FALSE] else best_or
  top_rd <- if (!is.null(best_rd) && nrow(best_rd)) best_rd[order(best_rd$P_value), , drop = FALSE] else best_rd
  if (!is.null(top_or) && nrow(top_or) > top_k) top_or <- top_or[seq_len(top_k), , drop = FALSE]
  if (!is.null(top_rd) && nrow(top_rd) > top_k) top_rd <- top_rd[seq_len(top_k), , drop = FALSE]

  # Compose markdown report
  n_total <- nrow(dat)
  n_y_obs <- sum(!is.na(dat[[outcome]]))
  fam_lab <- if (is_binom) "Binomial (logit)" else "Gaussian (identity)"

  lines <- c(
    "# Automated analysis summary",
    "",
    sprintf("**Outcome**: %s", outcome),
    sprintf("**Family**: %s", fam_lab),
    sprintf("**N (total)**: %d; **N (non-missing outcome)**: %d", n_total, n_y_obs),
    sprintf("**Independent variables analysed**: %d", length(independent_variables)),
    sprintf("**Covariates (adjusted)**: %s", ifelse(length(covariates), paste(covariates, collapse = ", "), "(none)")),
    sprintf("**Incremental adjustment**: %s", ifelse(incremental, "Yes", "No")),
    sprintf("**Risk difference estimated**: %s", ifelse(include_rd, "Yes", "No")),
    ""
  )

  # Top OR/Beta section
  if (!is.null(top_or) && nrow(top_or)) {
    effect_name <- if ("OR" %in% names(top_or)) "OR" else "Beta"
    lines <- c(lines, sprintf("## Top %d associations by adjusted p-value (%s)", nrow(top_or), effect_name), "")
    # Build bullets
    bullets <- apply(top_or, 1, function(r) {
      var   <- coalesce_chr(r[["Variable"]], "")
      catg  <- coalesce_chr(r[["Category"]], "")
      ptxt  <- fmt_p(suppressWarnings(as.numeric(r[["P_value"]])))
      eff   <- suppressWarnings(as.numeric(r[[effect_name]]))
      lo    <- suppressWarnings(as.numeric(r[["Lower"]]))
      hi    <- suppressWarnings(as.numeric(r[["Upper"]]))
      eff_str <- if (is.na(eff)) "NA" else sprintf("%.2f %s", eff, fmt_ci(lo, hi, 2))
      # Show model/adjustment label if useful
      lab <- if ("adjustment_label" %in% names(top_or)) coalesce_chr(r[["adjustment_label"]], "") else ""
      lab <- if (nzchar(lab)) paste0(" [", lab, "]") else ""
      paste0("- **", var, "** - ", catg, ": ", effect_name, "=", eff_str, ", p=", ptxt, lab)
    })
    lines <- c(lines, bullets, "")
  }

  # Top RD section
  if (include_rd && !is.null(top_rd) && nrow(top_rd)) {
    lines <- c(lines, sprintf("## Top %d associations by adjusted p-value (Risk difference)", nrow(top_rd)), "")
    bullets <- apply(top_rd, 1, function(r) {
      var   <- coalesce_chr(r[["Variable"]], "")
      catg  <- coalesce_chr(r[["Category"]], "")
      ptxt  <- fmt_p(suppressWarnings(as.numeric(r[["P_value"]])))
      rd    <- suppressWarnings(as.numeric(r[["RD"]]))
      lo    <- suppressWarnings(as.numeric(r[["Lower"]]))
      hi    <- suppressWarnings(as.numeric(r[["Upper"]]))
      rd_str <- paste0(fmt_rd(rd, 3), " ", fmt_ci(lo, hi, 3))
      lab <- if ("adjustment_label" %in% names(top_rd)) coalesce_chr(r[["adjustment_label"]], "") else ""
      lab <- if (nzchar(lab)) paste0(" [", lab, "]") else ""
      paste0("- **", var, "** - ", catg, ": RD=", rd_str, ", p=", ptxt, lab)
    })
    lines <- c(lines, bullets, "")
  }

  # Diagnostics snippet (optional)
  if (!is.null(diag_df) && nrow(diag_df)) {
    n_err <- sum(diag_df$status %in% c("error","failed"), na.rm = TRUE)
    n_sep <- sum(nzchar(diag_df$separation_msg %||% ""), na.rm = TRUE)
    if (n_err + n_sep > 0) {
      lines <- c(lines,
                 "## Model diagnostics (summary)",
                 sprintf("- Fits with errors: %d", n_err),
                 sprintf("- Perfect/near separation flags: %d", n_sep),
                 "")
    }
  }

  report_text <- paste(lines, collapse = "\n")

  if (!is.null(report_path)) {
    dir.create(dirname(report_path), showWarnings = FALSE, recursive = TRUE)
    con <- file(report_path, open = "wt", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(report_text, con = con, sep = "\n")
  }

  # ---- return -----------------------------------------------------------
  list(
    table_one         = t1,
    model_outputs     = mm,
    report_markdown   = report_text
  )
}

# Helper for %||% (coalesce to rhs if lhs is NULL)
`%||%` <- function(x, y) if (is.null(x)) y else x
