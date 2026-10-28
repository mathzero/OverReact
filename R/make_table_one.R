# Shared helpers keep group identities and display names consistent across blocks.
.table_one_check_columns <- function(dat, rowvar, colvar, weights) {
  if (!is.null(colvar) && !colvar %in% names(dat)) {
    stop(sprintf("Column variable '%s' not found in data frame.", colvar))
  }
  if (!is.null(weights)) {
    if (!weights %in% names(dat)) {
      stop(sprintf("Weights variable '%s' not found in data frame.", weights))
    }
    w <- dat[[weights]]
    if (!is.numeric(w) || any(!is.finite(w[!is.na(w)])) || any(w < 0, na.rm = TRUE)) {
      stop("Weights must be numeric, finite, and non-negative (or NA).")
    }
  }
  if (!rowvar %in% names(dat)) {
    message(sprintf("Variable '%s' not found - skipping.", rowvar))
    return(FALSE)
  }
  TRUE
}

.table_one_factor <- function(x, include_na, reserved = character()) {
  x <- if (is.factor(x)) x else factor(x)
  labels <- levels(x)
  labels <- labels[!is.na(labels)]
  codes <- match(as.character(x), labels)
  if (include_na && anyNA(codes)) {
    codes[is.na(codes)] <- length(labels) + 1L
    labels <- c(labels, "NA")
  }
  # Preserve spaces/punctuation. Only actual duplicate or reserved names need a suffix.
  labels <- make.unique(c(reserved, labels))[seq_along(labels) + length(reserved)]
  factor(codes, levels = seq_along(labels), labels = labels)
}

.table_one_groups <- function(dat, colvar, include_na) {
  x <- if (is.null(colvar)) rep("All data", nrow(dat)) else dat[[colvar]]
  .table_one_factor(x, include_na,
                    reserved = c("Variable", "Level", "Missing", "Overall", "P-value", "units", "V"))
}

.table_one_counts <- function(rows, groups, weights = NULL, digits = 0) {
  tab <- table(rows, groups)
  if (!is.null(weights) && length(tab)) {
    # Explicit missing categories are ordinary factor levels; other NAs are excluded.
    valid <- !is.na(rows) & !is.na(groups) & !is.na(weights)
    sums <- tapply(weights[valid], list(rows[valid], groups[valid]), sum, default = 0)
    tab[] <- if (is.null(digits)) sums else round(sums, digits)
  }
  tab
}

# Kish effective sample size, sum(w)^2 / sum(w^2): n for constant weights, smaller as
# weights vary. Weighted inference uses it so results do not depend on the weights' scale.
.kish_ess <- function(w) {
  w <- w[!is.na(w)]
  if (!length(w) || sum(w^2) <= 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

.wilson_interval <- function(p, n, z = qnorm(0.975)) {
  if (is.na(n) || n <= 0) return(list(proportion = p, lower = NA_real_, upper = NA_real_))
  z2 <- z^2
  center <- (p + z2 / (2 * n)) / (1 + z2 / n)
  margin <- z * sqrt((p * (1 - p) / n) + (z2 / (4 * n^2))) / (1 + z2 / n)
  list(proportion = p, lower = center - margin, upper = center + margin)
}

.safe_chisq <- function(tab, rowvar, colvar) {
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2L || ncol(tab) < 2L) return(list(p.value = NA_real_))
  tryCatch(suppressWarnings(chisq.test(tab)), error = function(e) {
    message(sprintf("Chi-squared test failed for '%s' x '%s': %s", rowvar, colvar, e$message))
    list(p.value = NA_real_)
  })
}

.table_one_cells <- function(counts, denominators, include_percentages,
                             confint, comma_thousands, ess = NULL) {
  vapply(seq_along(counts), function(i) {
    count <- if (comma_thousands) comma_sep(counts[i]) else as.character(counts[i])
    if (!include_percentages) return(count)
    n <- denominators[i]
    if (n <= 0) return(paste0(count, " (NA%)"))
    if (confint) {
      # Weighted cells: weighted proportion, with the denominator's effective sample size.
      ci <- if (is.null(ess)) {
        calculate_prop_ci(as.numeric(counts[i]), as.numeric(n), method = "wilson")
      } else {
        .wilson_interval(as.numeric(counts[i]) / as.numeric(n), ess[i])
      }
      sprintf("%s (%.1f%%, [%.1f-%.1f])", count, 100 * ci$proportion,
              100 * ci$lower, 100 * ci$upper)
    } else {
      sprintf("%s (%.1f%%)", count, 100 * counts[i] / n)
    }
  }, character(1))
}

# Categorical cross-tabulation. The misspelled argument is retained for compatibility.
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
                     includeNAsRowvar = TRUE,
                     rowwise_percentages = rowwise_precentages) {
  if (!.table_one_check_columns(dat, rowvar, colvar, weights)) return(NULL)

  rows <- .table_one_factor(dat[[rowvar]], includeNAsRowvar)
  groups <- .table_one_groups(dat, colvar, includeNAsColvar)
  w <- if (is.null(weights)) NULL else dat[[weights]]
  counts <- .table_one_counts(rows, groups, w)
  if (!nrow(counts)) {
    message(sprintf("Cross-tab for '%s' is empty - skipping.", rowvar))
    return(NULL)
  }

  totals <- rowSums(counts)
  # Overall describes the supplied population, even when missing groups are hidden.
  overall_counts <- if (includeNAsColvar) totals else {
    rowSums(.table_one_counts(rows, .table_one_groups(dat, colvar, TRUE), w))
  }
  # Effective sample sizes of each percentage denominator: row levels, groups, overall.
  ess_rows <- ess_groups <- ess_overall <- NULL
  if (!is.null(w)) {
    in_table <- !is.na(rows) & !is.na(groups)
    ess_rows <- vapply(levels(rows), function(l) .kish_ess(w[in_table & rows == l]), numeric(1))
    ess_groups <- vapply(levels(groups), function(g) .kish_ess(w[in_table & groups == g]), numeric(1))
    ess_overall <- rep(.kish_ess(w[!is.na(rows)]), length(overall_counts))
  }
  overall <- .table_one_cells(overall_counts, rep(sum(overall_counts), length(overall_counts)),
                              include_percentages && !rowwise_percentages,
                              confint, comma_thousands, ess_overall)
  tab <- data.frame(units = c(if (include_percentages) ", n (%)" else ", n",
                              rep(" ", nrow(counts))),
                    Level = c("", levels(rows)),
                    Missing = c(as.character(sum(is.na(as.character(dat[[rowvar]])))), rep("", nrow(counts))),
                    Overall = c("", overall), check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_len(ncol(counts))) {
    denominators <- if (rowwise_percentages) totals else rep(sum(counts[, j]), nrow(counts))
    ess <- if (is.null(w)) NULL else if (rowwise_percentages) ess_rows else rep(ess_groups[[j]], nrow(counts))
    tab[[levels(groups)[j]]] <- c("", .table_one_cells(counts[, j], denominators,
                                                     include_percentages, confint, comma_thousands,
                                                     ess))
  }

  if (statistical_test) {
    # Tests compare observed categories; displaying missing categories does not change inference.
    valid <- !is.na(as.character(dat[[rowvar]]))
    if (!is.null(colvar)) valid <- valid & !is.na(as.character(dat[[colvar]]))
    test_counts <- .table_one_counts(rows[valid], groups[valid], if (is.null(w)) NULL else w[valid],
                                     digits = NULL)
    if (!is.null(w)) {
      # Rescale weighted counts to the effective sample size: weights describe the
      # population, not extra observations, so their scale must not shrink the p-value.
      ess <- .kish_ess(w[valid])
      if (is.finite(ess) && sum(test_counts) > 0) test_counts <- test_counts * (ess / sum(test_counts))
    }
    p <- .safe_chisq(test_counts, rowvar, colvar)$p.value
    tab$`P-value` <- c(p, rep(NA_real_, nrow(counts)))
  }
  tab
}

# Continuous summaries use group indices directly, including the explicit missing group.
tableCont <- function(dat,
                      rowvar,
                      colvar = NULL,
                      weights = NULL,
                      summary_stat = "mean",
                      statistical_test = FALSE,
                      includeNAsColvar = TRUE,
                      includeNAsRowvar = TRUE) {
  if (!.table_one_check_columns(dat, rowvar, colvar, weights)) return(NULL)
  if (!is.numeric(dat[[rowvar]])) stop("Continuous row variables must be numeric.")
  if (!is.null(weights)) {
    stop("Weighted continuous summaries are not supported; weights were previously ignored.")
  }
  summary_stat <- match.arg(tolower(summary_stat), c("mean", "median"))
  centre <- if (summary_stat == "mean") mean else median
  spread <- if (summary_stat == "mean") sd else IQR
  summarize <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_character_)
    sprintf("%s (%s)", round(centre(x), 2), round(spread(x), 2))
  }

  groups <- .table_one_groups(dat, colvar, includeNAsColvar)
  values <- dat[[rowvar]]
  tab <- data.frame(units = sprintf(", %s (%s)", summary_stat,
                                    if (summary_stat == "mean") "SD" else "IQR"),
                    Level = "", Missing = as.character(sum(is.na(values))),
                    Overall = summarize(values), check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_along(levels(groups))) {
    tab[[levels(groups)[j]]] <- summarize(values[which(as.integer(groups) == j)])
  }

  if (statistical_test) {
    # Local column names avoid parsing user-supplied names as R code.
    test_dat <- data.frame(value = values, group = if (is.null(colvar)) rep("All data", nrow(dat)) else as.character(dat[[colvar]]))
    test_dat <- test_dat[complete.cases(test_dat), , drop = FALSE]
    test_dat$group <- factor(test_dat$group)
    p <- if (nlevels(test_dat$group) < 2L) NA_real_ else tryCatch({
      anova(lm(value ~ group, data = test_dat))$`Pr(>F)`[1]
    }, error = function(e) {
      message(sprintf("ANOVA failed for '%s': %s", rowvar, e$message))
      NA_real_
    })
    tab$`P-value` <- if (is.finite(p)) p else NA_real_
  }
  tab
}

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
                     addNobsTopRow = TRUE,
                     rowwise_percentages = rowwise_precentages) {
  if (!is.data.frame(dat) || !nrow(dat)) stop("'dat' must be a data frame with at least one row.")
  if (anyDuplicated(names(dat))) stop("'dat' must have unique column names.")
  if (!is.character(rowvars) || !length(rowvars) || anyNA(rowvars)) stop("'rowvars' must contain variable names.")
  if (!is.null(colvar) && (length(colvar) != 1L || !colvar %in% names(dat))) {
    stop("'colvar' must name one column in 'dat'.")
  }
  summary_stat <- match.arg(tolower(summary_stat), c("mean", "median"))
  if (!is.null(cov_names)) {
    if (is.list(cov_names)) cov_names <- unlist(cov_names, use.names = TRUE, recursive = FALSE)
    if (!is.atomic(cov_names) || is.null(names(cov_names)) || anyNA(names(cov_names)) || any(names(cov_names) == "") || anyDuplicated(names(cov_names))) {
      stop("'cov_names' must be a named list or vector with unique, non-empty names.")
    }
  }

  res_list <- list()
  block_names <- character()
  if (addNobsTopRow) {
    # Build the count row in a separate frame so user columns are never overwritten.
    obs_dat <- data.frame(observation = rep(" ", nrow(dat)), group = if (is.null(colvar)) rep("All data", nrow(dat)) else dat[[colvar]])
    if (!is.null(weights)) {
      .table_one_check_columns(dat, rowvars[1], colvar, weights)
      obs_dat$weight <- dat[[weights]]
    }
    obs <- tableCat(obs_dat, "observation", "group", confint = confint,
                    include_percentages = include_percentages, rowwise_percentages = TRUE,
                    weights = if (is.null(weights)) NULL else "weight",
                    comma_thousands = comma_thousands, includeNAsColvar = includeNAsColvar,
                    includeNAsRowvar = FALSE)
    if (!is.null(obs)) {
      obs <- obs[2, , drop = FALSE]
      obs$units <- ", n"
      obs$Level <- ""
      obs$Missing <- "0"
      if (statistical_test) obs$`P-value` <- NA_real_
      res_list[[1]] <- obs
      block_names <- "Observations"
    }
  }

  for (rv in unique(rowvars)) {
    if (!rv %in% names(dat)) {
      message(sprintf("Variable '%s' missing - omitted from table.", rv))
      next
    }
    message(sprintf("Processing '%s'", rv))
    values <- dat[[rv]]
    observed <- values[!is.na(values)]
    continuous <- is.numeric(values) && (!length(observed) || !all(observed %in% c(0, 1)))
    if (continuous) {
      res <- tableCont(dat, rv, colvar, weights = weights, summary_stat = summary_stat,
                       statistical_test = statistical_test, includeNAsColvar = includeNAsColvar,
                       includeNAsRowvar = includeNAsRowvar)
    } else {
      res <- tableCat(dat, rv, colvar, confint = confint,
                      include_percentages = include_percentages, rowwise_percentages = rowwise_percentages,
                      weights = weights, comma_thousands = comma_thousands,
                      statistical_test = statistical_test, includeNAsColvar = includeNAsColvar,
                      includeNAsRowvar = includeNAsRowvar)
    }
    if (is.null(res)) next
    res_list[[length(res_list) + 1L]] <- res
    label <- if (is.null(cov_names)) NA_character_ else cov_names[rv]
    block_names <- c(block_names, if (is.na(label)) rv else as.character(label))
  }
  if (!length(res_list)) stop("No valid row variables to tabulate - nothing to do.")

  for (i in seq_along(res_list)) {
    res <- res_list[[i]]
    res$Variable <- ifelse(res$units == " ", " ", paste0(block_names[i], res$units))
    res$units <- NULL
    res_list[[i]] <- res[c("Variable", setdiff(names(res), "Variable"))]
  }
  out <- dplyr::bind_rows(res_list)
  if (statistical_test && formatPvalsForEpiPaper && "P-value" %in% names(out)) {
    missing_p <- is.na(out$`P-value`)
    out$`P-value` <- as.character(pvalAsterisker(out$`P-value`, return_p = TRUE,
                                                return_ns = FALSE, round_to = 4))
    out$`P-value`[missing_p] <- NA_character_
    out$`P-value`[out$Variable == " "] <- " "
  }
  rownames(out) <- NULL
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
    ci <- .wilson_interval(p_hat, n, z)
    lower <- ci$lower
    upper <- ci$upper
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
