#' @import dplyr
#' @import stats
#' @import mgcv

#' @param dat data to be modelled
#' @param rowvar row variable for cross-tab
#' @param colvar column variable for cross-tab
#' @param confint Logical - return confidence intervals in cross tab
#' @param include_percentages Logical - include %s
#' @param rowwise_precentages Logical - calculate %s rowwise (TRUE) or columnwise (FALSE)
#' @param rowvars Supply a list of variables for the rows of a multi-variable cross-tab
#' @param cov_names List of more descriptive names for the row variables in a cross-tab. Supply a list of the format eg list(varname="More descriptive variable name")
#' @param mystring String, or vector of strings, to be amended
#' @param lookbehind The text leading up to the start of the string to be extracted
#' @param lookahead The text after the end of the string to be extracted
#' @param return_numeric Logical - return extracted text as numeric
#' @param myxtab A cross-tab table to be amended
#' @param pivot_for_plotting Logical - if TRUE, the function will return a table that is pivoted long
#' @param statistical_test Logical - if TRUE, the function will conduct an appropriate statistical test (chisq / anova) on your data
#' @param summary_stat "mean" or "median" - summary statistic for continuous variables. Mean will include (SD), median will include (IQR)
#' @param comma_thousands Boolean - insert commas separating thousands in large numbers eg 1,000,000
#' @param includeNAsColvar Boolean - include NA values as a separate group in the table columns
#' @param includeNAsRowvar Boolean - include NA values as a separate group in the table rows
#' @param formatPvalsForEpiPaper Boolean - if TRUE, add asterisks to pvalues and round
################################################################################
# Robust versions of the "table*" helper functions (v3) -----------------------
#  * Removes any **duplicate columns** created when `make.names()` collapses
#    category labels (e.g., "Not sure" → "Not.sure"). When duplicates are
#    found, numeric values are summed; for character/percentage cells we keep
#    the first column.
#  * Ensures that the special observation‑count row produced by
#    `addNobsTopRow = TRUE` is rendered in **exactly one row** – no blank
#    sub‑row underneath.
#  * Keeps previous fixes: graceful skipping of empty tables; robust χ² / ANOVA
#    handling; flexible `cov_names`; NA column handling.
################################################################################


# -----------------------------------------------------------------------------
# Helper utilities -----------------------------------------------------------
# -----------------------------------------------------------------------------

.safe_chisq <- function(tab, rowvar, colvar) {
  tryCatch({
    if (all(tab == 0)) stop("All table cells are zero – χ² not defined")
    suppressWarnings(chisq.test(tab))
  }, error = function(e) {
    message(sprintf("Chi-squared test failed for '%s' × '%s': %s", rowvar, colvar, e$message))
    list(p.value = NA_real_)
  })
}

# Merge duplicate columns generated after `make.names()` collisions.  If the
# cells are numeric we sum across the duplicates; otherwise we keep the first
# (string) column.
.merge_duplicate_cols <- function(df) {
  dup_names <- unique(names(df)[duplicated(names(df))])
  for (nm in dup_names) {
    cols <- which(names(df) == nm)
    if (all(sapply(df[, cols], is.numeric))) {
      df[, cols[1]] <- rowSums(df[, cols], na.rm = TRUE)
    } else {
      df[, cols[1]] <- df[, cols[1]]  # keep first textual column
    }
  }
  df[, !duplicated(names(df)), drop = FALSE]
}

# -----------------------------------------------------------------------------
# Simple cross‑tabulation for categorical variables ---------------------------
# -----------------------------------------------------------------------------

tableCat <- function(dat,
                     rowvar,
                     colvar,
                     confint = TRUE,
                     include_percentages = TRUE,
                     rowwise_precentages = TRUE,
                     weights = NULL,
                     comma_thousands = FALSE,
                     statistical_test = FALSE,
                     includeNAsColvar = TRUE,
                     includeNAsRowvar = TRUE) {

  # --- guards ----------------------------------------------------------------
  if (!rowvar %in% names(dat)) {
    message(sprintf("Variable '%s' not found – skipping cross‑tab.", rowvar))
    return(NULL)
  }
  if (!colvar %in% names(dat)) {
    stop(sprintf("Column variable '%s' not found in data frame.", colvar))
  }
  if (!is.null(weights) && !weights %in% names(dat)) {
    stop(sprintf("Weights variable '%s' not found in data frame.", weights))
  }

  # --- vectors ---------------------------------------------------------------
  colvect <- if (includeNAsColvar && any(is.na(dat[[colvar]]))) addNA(dat[[colvar]]) else dat[[colvar]]
  NAswitch <- includeNAsRowvar && any(is.na(dat[[rowvar]]))
  if (NAswitch) {
    rowvect <- addNA(dat[[rowvar]])
    useNA   <- "ifany"; na.show <- TRUE;  na.rm <- FALSE
  } else {
    rowvect <- dat[[rowvar]]
    useNA   <- "no";    na.show <- FALSE; na.rm <- TRUE
  }

  # --- contingency table -----------------------------------------------------
  if (is.null(weights)) {
    tab_mat <- table(rowvect, colvect, useNA = useNA)
  } else {
    tab_mat <- round(questionr::wtd.table(x = rowvect, y = colvect,
                                          weights = dat[[weights]], normwt = FALSE,
                                          na.rm = na.rm, na.show = na.show), 0)
  }

  if (statistical_test) pval <- .safe_chisq(tab_mat, rowvar, colvar)

  # --- add margins & convert to data frame -----------------------------------
  tab <- addmargins(tab_mat, margin = 2) %>%
    as.data.frame.matrix(check.names = FALSE, stringsAsFactors = FALSE)

  # Normalise & deduplicate column names (handle NA first, then general dups)
  names(tab)[is.na(names(tab)) | names(tab) == "NA."] <- "NA"
  tab <- .merge_duplicate_cols(tab)

  # Early‑exit guard – skip empty tables
  if (nrow(tab) == 0 || ncol(tab) == 0) {
    message(sprintf("Cross‑tab for '%s' × '%s' is empty – skipping.", rowvar, colvar))
    return(NULL)
  }

  # --- percentage table (for later display) ----------------------------------
  prop_fun <- if (rowwise_precentages) function(m) prop.table(m, 1) else function(m) prop.table(m, 2)
  tab.prop <- round(100 * prop_fun(tab_mat), 1) %>% as.data.frame.matrix(check.names = FALSE)
  names(tab.prop)[is.na(names(tab.prop)) | names(tab.prop) == "NA."] <- "NA"
  tab.prop <- .merge_duplicate_cols(tab.prop)
  if (rowwise_precentages) tab.prop$sum <- "" else tab.prop$sum <- round(100 * prop.table(table(rowvect)), 1)

  tab_bu <- tab  # back‑up counts for CI calcs later

  # --- optional comma separation --------------------------------------------
  if (comma_thousands) {
    tab <- lapply(tab, comma_sep) %>% as.data.frame(check.names = FALSE)
    colnames(tab) <- colnames(tab_bu)
  }

  # --- attach percentages / CIs ---------------------------------------------
  if (include_percentages) {
    for (i in seq_len(nrow(tab))) {
      for (j in seq_len(ncol(tab))) {
        if (j == ncol(tab)) {   # total column
          if (confint) {
            ci <- calculate_prop_ci(tab_bu[i, j], tab_bu[i, ncol(tab_bu)], method = "wilson")
            tab[i, j] <- sprintf("%s (%.1f%%)", tab[i, j], 100 * ci$proportion)
          } else {
            tab[i, j] <- as.character(tab[i, j])
          }
        } else {
          if (confint) {
            denom <- if (rowwise_precentages) tab_bu[i, ncol(tab_bu)] else tab_bu[nrow(tab_bu), j]
            ci <- calculate_prop_ci(tab_bu[i, j], denom, method = "wilson")
            tab[i, j] <- sprintf("%s (%.1f%%, [%.1f–%.1f])", tab[i, j], 100 * ci$proportion,
                                 100 * ci$lower, 100 * ci$upper)
          } else {
            tab[i, j] <- sprintf("%s (%.1f%%)", tab[i, j], tab.prop[i, j])
          }
        }
      }
    }
  } else {
    # BUG FIX: If not adding percentages, ensure count columns are character
    # to maintain type consistency for bind_rows in tableOne.
    for (j in seq_len(ncol(tab))) {
      tab[, j] <- as.character(tab[, j])
    }
  }

  # --- build final display frame --------------------------------------------
  rv_na <- sum(is.na(dat[[rowvar]]))

  if (rowvar == "Observations") {
    # Single summary row, no header
    tab$Missing <- as.character(rv_na)
    tab$Level   <- ""
    tab$units   <- ", n" # Changed from ", n (%)"
  } else {
    # Insert header row
    levs <- rownames(tab_mat); levs[is.na(levs)] <- "NA"
    header_row <- tab[1, , drop = FALSE]; header_row[1, ] <- ""
    tab <- rbind(header_row, tab)
    tab$Missing <- c(rv_na, rep("", nrow(tab) - 1))
    tab$Level   <- c("", levs)
    # Adjust units based on whether percentages are included
    unit_str <- if(include_percentages) ", n (%)" else ", n"
    tab$units   <- c(unit_str, rep(" ", nrow(tab) - 1))
  }

  if (statistical_test) {
    if (!"P-value" %in% names(tab)) tab$`P-value` <- NA_real_
    tab$`P-value`[1] <- pval$p.value
  }

  tab <- tab %>%
    dplyr::select(units, Level, Missing, Sum, everything()) %>%
    dplyr::rename(Overall = Sum) %>%
    dplyr::mutate(Level = dplyr::case_when(Level == "NA." ~ "[NA]", TRUE ~ Level))

  tab
}

# -----------------------------------------------------------------------------
# Cross‑tab for continuous variables ------------------------------------------
# -----------------------------------------------------------------------------

tableCont <- function(dat,
                      rowvar,
                      colvar = NULL,
                      weights = NULL,
                      summary_stat = "mean",
                      statistical_test = FALSE,
                      includeNAsColvar = TRUE,
                      includeNAsRowvar = TRUE) {

  # --- guards ----------------------------------------------------------------
  if (!rowvar %in% names(dat)) {
    message(sprintf("Variable '%s' not found – skipping.", rowvar))
    return(NULL)
  }
  if (!is.null(colvar) && !colvar %in% names(dat)) {
    stop(sprintf("Column variable '%s' not found in data frame.", colvar))
  }
  if (!is.null(weights) && !weights %in% names(dat)) {
    stop(sprintf("Weights variable '%s' not found in data frame.", weights))
  }

  if (is.null(colvar)) {
    colvar <- "dummy"; dat$dummy <- "Dummy"
  }

  colvect <- if (includeNAsColvar && any(is.na(dat[[colvar]]))) addNA(dat[[colvar]]) else dat[[colvar]]

  summary_stat <- tolower(summary_stat)
  if (!summary_stat %in% c("mean", "median")) stop("'summary_stat' must be 'mean' or 'median'.")

  calc_fun   <- if (summary_stat == "mean") mean else median
  spread_fun <- if (summary_stat == "mean") sd   else IQR

  means   <- round(by(dat[[rowvar]], colvect, calc_fun, na.rm = TRUE), 2)
  spreads <- round(by(dat[[rowvar]], colvect, spread_fun, na.rm = TRUE), 2)

  means   <- as.table(means,   exclude = "none") %>% as.data.frame(stringsAsFactors = FALSE)
  spreads <- as.table(spreads, exclude = "none") %>% as.data.frame(stringsAsFactors = FALSE)
  names(means)   <- c("Category", "centre")
  names(spreads) <- c("Category", "spread")

  means$Category   <- as.character(addNA(means$Category))
  spreads$Category <- as.character(addNA(spreads$Category))
  means$Category[is.na(means$Category)]     <- "NA"
  spreads$Category[is.na(spreads$Category)] <- "NA"

  uniques <- unique(as.character(colvect))
  uniques <- if (includeNAsColvar) {
    replace(uniques, is.na(uniques), "NA")
  } else {
    uniques[!is.na(uniques)]
  }

  tab <- data.frame(matrix(nrow = 1, ncol = length(uniques), dimnames = list(NULL, uniques)),
                    stringsAsFactors = FALSE)
  for (u in uniques) {
    tab[[u]] <- sprintf("%s (%s)", means$centre[means$Category == u], spreads$spread[spreads$Category == u])
  }

  overall_centre <- calc_fun(dat[[rowvar]], na.rm = TRUE)
  overall_spread <- spread_fun(dat[[rowvar]], na.rm = TRUE)
  tab$Sum <- sprintf("%s (%s)", round(overall_centre, 2), round(overall_spread, 2))

  rv_na <- sum(is.na(dat[[rowvar]]))
  tab$Missing <- as.character(rv_na)

  # normalise & deduplicate column names again
  names(tab)[is.na(names(tab)) | names(tab) == "NA."] <- "NA"
  tab <- .merge_duplicate_cols(tab)

  tab$Level <- ""
  tab$units <- sprintf(", %s (%s)", summary_stat, ifelse(summary_stat == "mean", "SD", "IQR"))

  tab <- tab %>% dplyr::select(units, Level, Missing, Sum, everything()) %>% dplyr::rename(Overall = Sum)

  if (statistical_test) {
    pval <- tryCatch({
      mod <- lm(as.formula(sprintf("%s ~ `%s`", rowvar, colvar)), data = dat)
      anova(mod)$`Pr(>F)`[1]
    }, error = function(e) {
      message(sprintf("ANOVA failed for '%s' ~ '%s': %s", rowvar, colvar, e$message)); NA_real_
    })
    tab$`P-value` <- pval
  }

  tab
}

# -----------------------------------------------------------------------------
# tableOne wrapper (unchanged – earlier v2 already robust) --------------------
# -----------------------------------------------------------------------------

tableOne <- function(dat,
                     rowvars,
                     colvar = NULL,
                     cov_names = NULL,
                     confint = FALSE,
                     include_percentages = TRUE,
                     rowwise_precentages = TRUE,
                     weights = NULL,
                     summary_stat = "mean",
                     comma_thousands = FALSE,
                     statistical_test = FALSE,
                     includeNAsColvar = TRUE,
                     includeNAsRowvar = TRUE,
                     formatPvalsForEpiPaper = FALSE,
                     addNobsTopRow = TRUE) {

  if (!statistical_test) formatPvalsForEpiPaper <- FALSE

  summary_stat <- tolower(summary_stat)
  if (!summary_stat %in% c("mean", "median")) {
    stop("Please choose 'mean' or 'median' for summary_stat.")
  }

  if (is.null(colvar)) {
    colvar <- "dummy"
    dat$dummy <- "All data"
  }

  # ---- tidy / validate cov_names -------------------------------------------
  if (!is.null(cov_names)) {
    if (is.list(cov_names)) {
      cov_names <- unlist(cov_names, use.names = TRUE, recursive = FALSE)
    }
    if (is.null(names(cov_names))) {
      stop("'cov_names' must be a *named* list or vector.")
    }
  }

  # ---- optionally prepend observation count row ----------------------------
  if (addNobsTopRow) {
    dat$Observations <- " "
    rowvars <- c("Observations", setdiff(rowvars, "Observations"))
    if (is.null(cov_names)) cov_names <- character()
    cov_names["Observations"] <- "Observations"
  }

  # ---- iterate over variables ---------------------------------------------
  res_list <- list()
  for (rv in rowvars) {
    if (!rv %in% names(dat)) {
      message(sprintf("Variable '%s' missing – omitted from table.", rv))
      next
    }

    message(sprintf("Processing '%s'", rv))

    if (is.numeric(pull(dat, rv)) &&
        !all(names(table(pull(dat, rv))) %in% c("0", "1"))) {

      res <- tableCont(dat = dat, rowvar = rv, colvar = colvar,
                       summary_stat = summary_stat, statistical_test = statistical_test,
                       includeNAsColvar = includeNAsColvar, includeNAsRowvar = includeNAsRowvar,
                       weights = weights)
    } else {
      res <- tableCat(dat = dat, rowvar = rv, colvar = colvar, confint = confint,
                      include_percentages = include_percentages,
                      rowwise_precentages = rowwise_precentages, weights = weights,
                      comma_thousands = comma_thousands,
                      statistical_test = statistical_test,
                      includeNAsColvar = includeNAsColvar, includeNAsRowvar = includeNAsRowvar)
    }
    if (!is.null(res)) res_list[[rv]] <- res
  }

  if (!length(res_list)) {
    stop("No valid row variables to tabulate – nothing to do.")
  }

  # ---- name each block ------------------------------------------------------
  name_vec <- if (!is.null(cov_names)) {
    nm <- cov_names[names(res_list)]
    nm[is.na(nm)] <- names(res_list)[is.na(nm)]
    nm
  } else {
    names(res_list)
  }
  names(res_list) <- name_vec

  # ---- bind into a single data‑frame ---------------------------------------
  out <- dplyr::bind_rows(res_list, .id = "V") %>%
    dplyr::mutate(Variable = if_else(units == " ", " ", paste0(V, units))) %>%
    dplyr::select(-units, -V) %>%
    dplyr::select(Variable, everything())

  # ---- p‑value formatting for epi papers -----------------------------------
  if (formatPvalsForEpiPaper && "P-value" %in% names(out)) {
    out$`P-value` <- as.character(pvalAsterisker(p_values = out$`P-value`,
                                                 return_p = TRUE, return_ns = FALSE, round_to = 4))
    dupePvals <- out$Variable == " "
    if (any(dupePvals)) out$`P-value`[dupePvals] <- " "
  }

  # ---- tidy -----------------------------------------------------------------
  rownames(out) <- NULL
  out$Variable[out$Variable == "Observations, n (%)"] <- "N (%)"

  out
}



calculate_prop_ci <- function(x, n, method = "wilson") {
  # Validate inputs
  if (!is.numeric(x) || !is.numeric(n)) {
    stop("Both 'x' and 'n' must be numeric values.")
  }
  if (n <= 0 || round(n) != n) {
    stop("The number of n must be a positive integer.")
  }
  if (x < 0 || x > n) {
    stop("The number of x must be between 0 and the number of n.")
  }

  # Calculate point estimate
  p_hat <- x / n

  # 95% confidence corresponds to an alpha of 0.05 (two-tailed)
  z <- qnorm(0.975)  # 97.5th percentile of the normal distribution

  if (method == "normal") {
    # Standard normal approximation (Wald interval)
    se <- sqrt(p_hat * (1 - p_hat) / n)
    lower <- p_hat - z * se
    upper <- p_hat + z * se
    # Ensure that the computed limits lie within [0, 1]
    lower <- max(lower, 0)
    upper <- min(upper, 1)
  } else if (method == "wilson") {
    # Wilson score interval
    n <- n
    z2 <- z^2
    # Adjusted center
    center <- (p_hat + z2 / (2 * n)) / (1 + z2 / n)
    # Margin of error using the Wilson adjustment
    margin <- z * sqrt((p_hat * (1 - p_hat) / n) + (z2 / (4 * n^2))) / (1 + z2 / n)
    lower <- center - margin
    upper <- center + margin
  } else {
    stop("Method must be either 'normal' or 'wilson'.")
  }

  # Return the point estimate and interval
  result <- list(
    proportion = p_hat,
    lower = lower,
    upper = upper
  )

  return(result)
}
