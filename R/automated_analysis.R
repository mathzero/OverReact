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
    includeNAsColvar = FALSE,
    includeNAsRowvar = FALSE,
    formatPvalsForEpiPaper = FALSE,
    addNobsTopRow = TRUE,
    # By default, treat outcome as categorical if factor/character/logical
    # or a numeric with <= 6 unique non-missing values.
    outcome_cat_unique_threshold = 6,
    # ---- Modelling options -----------------------------------------------
    incremental = TRUE,
    adj_sets = NULL,                # ignored if incremental = TRUE
    include_crude = TRUE,
    include_rd = FALSE,              # NULL: TRUE for binary outcomes, else FALSE
    include_rr = FALSE,              # marginal risk ratios (binary outcomes)
    family = "auto",                 # model family; see ModelMakerMultiRD()
    p_adjust_method = "BH",          # q-values (p.adjust method); NULL or "none" for none
    ncores = NULL,
    n_sim = 100,
    rd_ci_method = c("delta", "simulation"),
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
  fmt_ci <- function(lo, hi, digits = 2) sprintf("(%.*f, %.*f)", digits, lo, digits, hi)
  fmt_rd <- function(x, digits = 3) ifelse(is.na(x), "NA", sprintf("%.*f", digits, x))
  coalesce_chr <- function(x, y) ifelse(is.na(x) | x == "", y, x)

  # Determine the model family; marginal effects need a binary outcome
  spec <- .react_resolve_family(dat[[outcome]], family = family, outcome = outcome)
  if (is.null(include_rd)) include_rd <- isTRUE(spec$binary)
  # What the report describes: requests for other outcomes are skipped by the models.
  rd_estimated <- isTRUE(include_rd) && isTRUE(spec$binary)
  rr_estimated <- isTRUE(include_rr) && isTRUE(spec$binary)

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
    include_rr                  = include_rr,
    family                      = spec$name,
    p_adjust_method             = p_adjust_method,
    n_sim                       = n_sim,
    rd_ci_method                = match.arg(rd_ci_method),
    savepath                    = save_dir,
    glm_control                 = glm_control,
    retry_failed                = TRUE,
    max_retries                 = 1,
    parallel_restarts           = 2,
    retry_sleep                 = 1
  )

  # mm elements we'll use
  marginal_tables <- list(RD = mm$plot_output_RDs, RR = mm$plot_output_RRs)
  diag_df      <- mm$diagnostics

  # ---- 3) Build "short report" ------------------------------------------
  # Strategy:
  #  * Identify each Variable's "fully adjusted" rows in plot_* (its max adjustment count).
  #  * For each Variable, pick the level with the smallest p-value (excluding reference rows).
  #  * Summarise top_k by p-value (model estimates, then marginal RDs and RRs if available).

  # Filter to each Variable's fullest adjustment; include crude if nothing else exists.
  # The maximum is per Variable: a predictor that is also a covariate is never adjusted
  # for itself, so it has fewer stages than the others and a global maximum would drop it.
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
    if (all(c("adjustment", "Variable") %in% names(d)) && nrow(d)) {
      adj <- suppressWarnings(as.numeric(d$adjustment))
      full_adj <- ave(adj, d$Variable, FUN = function(a) {
        if (all(is.na(a))) NA_real_ else max(a, na.rm = TRUE)
      })
      d_full <- d[!is.na(adj) & !is.na(full_adj) & adj == full_adj, , drop = FALSE]
      if (nrow(d_full)) return(d_full)
    }
    # fallback: crude only
    if ("model" %in% names(d)) {
      d_crude <- d[d$model == "crude", , drop = FALSE]
      if (nrow(d_crude)) return(d_crude)
    }
    d
  }

  # The top_k predictors by p-value, each represented by its lowest-p category, with
  # q-values over every test the section ranks (all levels of each predictor's
  # most-adjusted model).
  use_q <- !is.null(p_adjust_method) && !identical(p_adjust_method, "none")
  top_rows <- function(df) {
    df <- filter_full(df)
    if (is.null(df) || !nrow(df) || !all(c("Variable", "P_value") %in% names(df))) return(NULL)
    if (use_q) df$report_q <- stats::p.adjust(df$P_value, method = p_adjust_method)
    best <- do.call(rbind, lapply(split(df, df$Variable), function(z) {
      z[order(z$P_value, na.last = TRUE)[1], , drop = FALSE]
    }))
    best <- best[order(best$P_value), , drop = FALSE]
    rownames(best) <- NULL
    utils::head(best, top_k)
  }

  bullets <- function(top, effect_name, digits) {
    apply(top, 1, function(r) {
      var   <- coalesce_chr(r[["Variable"]], "")
      catg  <- coalesce_chr(r[["Category"]], "")
      if ("outcome_level" %in% names(top) && !is.na(r[["outcome_level"]])) {
        catg <- paste0(r[["outcome_level"]], ": ", catg)
      }
      ptxt  <- fmt_p(suppressWarnings(as.numeric(r[["P_value"]])))
      eff   <- suppressWarnings(as.numeric(r[[effect_name]]))
      lo    <- suppressWarnings(as.numeric(r[["Lower"]]))
      hi    <- suppressWarnings(as.numeric(r[["Upper"]]))
      eff_str <- if (is.na(eff)) "NA" else paste(fmt_rd(eff, digits), fmt_ci(lo, hi, digits))
      # Show model/adjustment label if useful
      lab <- if ("adjustment_label" %in% names(top)) coalesce_chr(r[["adjustment_label"]], "") else ""
      lab <- if (nzchar(lab)) paste0(" [", lab, "]") else ""
      qtxt <- if (use_q) paste0(", q=", fmt_p(suppressWarnings(as.numeric(r[["report_q"]])))) else ""
      paste0("- **", var, "** - ", catg, ": ", effect_name, "=", eff_str, ", p=", ptxt, qtxt, lab)
    })
  }

  # Compose markdown report
  n_total <- nrow(dat)
  n_y_obs <- sum(!is.na(dat[[outcome]]))
  fam_lab <- paste0(toupper(substring(spec$label, 1, 1)), substring(spec$label, 2))

  lines <- c(
    "# Automated analysis summary",
    "",
    sprintf("**Outcome**: %s", outcome),
    sprintf("**Family**: %s", fam_lab),
    sprintf("**N (total)**: %d; **N (non-missing outcome)**: %d", n_total, n_y_obs),
    sprintf("**Independent variables analysed**: %d", length(independent_variables)),
    sprintf("**Covariates (adjusted)**: %s", ifelse(length(covariates), paste(covariates, collapse = ", "), "(none)")),
    sprintf("**Incremental adjustment**: %s", ifelse(incremental, "Yes", "No")),
    sprintf("**Risk difference estimated**: %s", ifelse(rd_estimated, "Yes", "No")),
    sprintf("**Risk ratio estimated**: %s", ifelse(rr_estimated, "Yes", "No")),
    if (use_q) sprintf("**Multiple testing**: q-values from p.adjust(method = \"%s\") over each section's tests", p_adjust_method),
    ""
  )

  # Model-estimate section (OR, RR, IRR or Beta), then marginal RDs and RRs
  top_model <- top_rows(mm$plot_output)
  if (!is.null(top_model) && nrow(top_model)) {
    effect_name <- intersect(c("OR", "RR", "IRR", "Beta"), names(top_model))[1]
    lines <- c(lines, sprintf("## Top %d associations by adjusted p-value (%s)", nrow(top_model), effect_name), "",
               bullets(top_model, effect_name, 2), "")
  }
  section_titles <- c(RD = "Risk difference", RR = "Marginal risk ratio")
  for (m in c(if (rd_estimated) "RD", if (rr_estimated) "RR")) {
    top_m <- top_rows(marginal_tables[[m]])
    if (is.null(top_m) || !nrow(top_m) || !m %in% names(top_m)) next
    lines <- c(lines, sprintf("## Top %d associations by adjusted p-value (%s)", nrow(top_m), section_titles[[m]]), "",
               bullets(top_m, m, if (m == "RD") 3 else 2), "")
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
