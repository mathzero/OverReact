table_one_example <- function() {
  data.frame(
    group = factor(c("Asymptomatic", "Non-persistent symptoms", "Long COVID", NA,
                     "Asymptomatic", "Non-persistent symptoms", "Long COVID", NA),
                   levels = c("Asymptomatic", "Non-persistent symptoms", "Long COVID")),
    height = c(160, 170, 180, 150, 164, 174, 184, 154),
    doses = factor(c(0, 1, 2, 0, 1, 2, 0, 1))
  )
}

test_that("tableOne retains missing-group measurements without extra columns", {
  dat <- table_one_example()
  out <- suppressMessages(tableOne(
    dat, c("height", "doses"), "group",
    cov_names = list(height = "Height (cm)"), include_percentages = TRUE,
    rowwise_percentages = TRUE, confint = FALSE, includeNAsColvar = TRUE,
    includeNAsRowvar = FALSE, statistical_test = TRUE, formatPvalsForEpiPaper = TRUE
  ))

  expect_identical(names(out), c("Variable", "Level", "Missing", "Overall",
                                "Asymptomatic", "Non-persistent symptoms", "Long COVID", "NA", "P-value"))
  expect_identical(out$Variable[1:3], c("Observations, n", "Height (cm), mean (SD)", "doses, n (%)"))
  expect_identical(out$`NA`[1:2], c("2 (25.0%)", "152 (2.83)"))
  expect_identical(out$`Non-persistent symptoms`[2], "172 (2.83)")
  expect_identical(out$`Long COVID`[2], "182 (2.83)")
  expect_identical(out$Overall[1:2], c("8", "167 (12.14)"))
  expect_true(is.na(out$`P-value`[1]))
  expect_equal(sum(out$Variable == "Observations, n"), 1)
})

test_that("literal names, missing categories, and reserved names stay distinct", {
  dat <- data.frame(group = factor(rep(c("A B", "A.B", "NA", "Overall", "P-value"), each = 2),
                                  levels = c("A B", "A.B", "NA", "Overall", "P-value", "unused")),
                    value = rep(c(10, 20, 30, 40, 50), each = 2), category = "yes")
  dat <- rbind(dat, data.frame(group = NA, value = 60, category = "yes"))
  out <- suppressMessages(tableOne(dat, c("value", "category"), "group", include_percentages = FALSE))

  expect_identical(names(out), c("Variable", "Level", "Missing", "Overall", "A B", "A.B", "NA",
                                "Overall.1", "P-value.1", "unused", "NA.1"))
  expect_identical(out$`A B`[2], "10 (0)")
  expect_identical(out$A.B[2], "20 (0)")
  expect_identical(out$`NA`[2], "30 (0)")
  expect_identical(out$`NA.1`[2], "60 (NA)")
  expect_identical(out$Overall.1[2], "40 (0)")
  expect_identical(out$`P-value.1`[2], "50 (0)")
  expect_true(is.na(out$unused[2]))
  expect_identical(out$unused[1], "0")
})

test_that("row and column missing-value switches operate independently", {
  dat <- data.frame(group = c("A", NA, "B", "B"), category = c("yes", "no", NA, "yes"))
  for (include_rows in c(FALSE, TRUE)) {
    for (include_groups in c(FALSE, TRUE)) {
      out <- tableCat(dat, "category", "group", confint = FALSE,
                      includeNAsColvar = include_groups, includeNAsRowvar = include_rows)
      expect_identical("NA" %in% names(out), include_groups)
      expect_identical("NA" %in% out$Level, include_rows)
      expect_identical(out$Missing[1], "1")
      expect_identical(out$Overall[out$Level == "no"], "1")
      if (include_groups) expect_identical(out$`NA`[out$Level == "no"], "1 (100.0%)")
    }
  }
})

test_that("column percentages and confidence intervals use column totals", {
  dat <- data.frame(group = c(rep("A", 4), rep("B", 8)),
                    category = c("yes", rep("no", 3), rep("yes", 6), rep("no", 2)))
  out <- tableCat(dat, "category", "group", rowwise_percentages = FALSE, confint = TRUE)
  yes <- out$Level == "yes"
  expect_identical(out$A[yes], "1 (25.0%, [4.6-69.9])")
  expect_identical(out$B[yes], "6 (75.0%, [40.9-92.9])")
  expect_identical(out$Overall[yes], "7 (58.3%, [32.0-80.7])")
  without_ci <- tableCat(dat, "category", "group", rowwise_percentages = FALSE, confint = FALSE)
  expect_identical(without_ci$A[yes], "1 (25.0%)")
  expect_identical(without_ci$Overall[yes], "7 (58.3%)")
})

test_that("zero denominators and all-missing measurements are handled explicitly", {
  dat <- data.frame(group = factor(c("A", "A", "B", "B"), levels = c("A", "B", "unused")),
                    category = factor(c("yes", "yes", "no", "no"), levels = c("no", "yes", "unused")),
                    value = c(10, 12, NA_real_, NA_real_), absent = rep(NA_real_, 4))
  expect_no_warning(out <- tableCat(dat, "category", "group", rowwise_percentages = FALSE))
  expect_identical(out$unused[-1], rep("0 (NA%)", 3))
  expect_no_warning(cont <- tableCont(dat, "value", "group"))
  expect_identical(cont$A, "11 (1.41)")
  expect_true(is.na(cont$B))
  expect_true(is.na(cont$unused))
  absent <- suppressMessages(tableOne(dat, "absent", "group", addNobsTopRow = FALSE))
  expect_identical(absent$Variable, "absent, mean (SD)")
  expect_identical(absent$Missing, "4")
  expect_true(is.na(absent$Overall))
})

test_that("median summaries include the missing group and preserve factor order", {
  dat <- table_one_example()
  dat$group <- factor(dat$group, levels = c("Long COVID", "Asymptomatic", "Non-persistent symptoms"))
  out <- tableCont(dat, "height", "group", summary_stat = "median")
  expect_identical(names(out), c("units", "Level", "Missing", "Overall", "Long COVID", "Asymptomatic", "Non-persistent symptoms", "NA"))
  expect_identical(out$`NA`, "152 (2)")
  expect_identical(out$`Long COVID`, "182 (2)")
})

test_that("tests use complete cases and tolerate empty factor levels", {
  dat <- table_one_example()
  dat$group <- factor(dat$group, levels = c(levels(dat$group), "unused"))
  dat[["height (cm)"]] <- dat$height
  expected_anova <- anova(lm(height ~ group, data = dat))$`Pr(>F)`[1]
  for (show_missing in c(FALSE, TRUE)) {
    cont <- tableCont(dat, "height (cm)", "group", statistical_test = TRUE, includeNAsColvar = show_missing)
    cat <- tableCat(dat, "doses", "group", statistical_test = TRUE, includeNAsColvar = show_missing,
                    cat_test = "chisq")
    expected_chisq <- suppressWarnings(chisq.test(table(dat$doses, droplevels(dat$group))))$p.value
    expect_equal(cont$`P-value`, expected_anova)
    expect_equal(cat$`P-value`[1], expected_chisq)
  }
  one_group <- transform(dat, group = "A")
  expect_true(is.na(tableCont(one_group, "height", "group", statistical_test = TRUE)$`P-value`))
  expect_true(is.na(tableCat(one_group, "doses", "group", statistical_test = TRUE)$`P-value`[1]))
})

test_that("unstratified tables and count rows preserve user columns", {
  dat <- table_one_example()
  dat$dummy <- dat$height
  dat$Observations <- dat$height + 10
  original <- dat
  out <- suppressMessages(tableOne(dat, c("dummy", "Observations"), statistical_test = TRUE))
  expect_identical(out$Variable, c("Observations, n", "dummy, mean (SD)", "Observations, mean (SD)"))
  expect_identical(out$Overall, c("8", "167 (12.14)", "177 (12.14)"))
  expect_true(all(is.na(out$`P-value`)))
  expect_identical(dat, original)
  hidden <- suppressMessages(tableOne(dat, "doses", "group", includeNAsColvar = FALSE))
  expect_false("NA" %in% names(hidden))
  expect_identical(hidden$Overall[1], "8")
  expect_identical(hidden$Asymptomatic[1], "2 (33.3%)")
})

test_that("old percentage argument remains compatible and counts bind correctly", {
  dat <- table_one_example()
  old <- suppressMessages(tableOne(dat, c("height", "doses"), "group", rowwise_precentages = FALSE))
  new <- suppressMessages(tableOne(dat, c("height", "doses"), "group", rowwise_percentages = FALSE))
  expect_identical(old, new)
  counts <- suppressMessages(tableOne(dat, c("height", "doses"), "group", include_percentages = FALSE))
  expect_identical(counts$Asymptomatic[c(1, 4, 5, 6)], c("2", "1", "1", "0"))
  expect_identical(counts$Variable[3], "doses, n")
  large <- data.frame(group = rep("A", 1000), category = "yes")
  expect_identical(tableCat(large, "category", "group", include_percentages = FALSE,
                            comma_thousands = TRUE)$A[2], "1,000")
})

test_that("weights are respected for counts and never silently ignored", {
  dat <- data.frame(group = c("A", NA, "B", "B"), category = c("yes", "no", NA, "yes"),
                    value = c(10, 20, 30, 40), w = c(2, 3, 4, 5))
  out <- tableCat(dat, "category", "group", weights = "w", include_percentages = FALSE,
                  includeNAsColvar = TRUE, includeNAsRowvar = FALSE)
  expect_identical(out$`NA`[out$Level == "no"], "3")
  expect_identical(out$B[out$Level == "yes"], "5")
  expect_identical(out$Overall[out$Level == "yes"], "7")
  expect_error(tableCont(dat, "value", "group", weights = "w"), "Weighted continuous summaries are not supported")
  dat$w[1] <- -1
  expect_error(tableCat(dat, "category", "group", weights = "w"), "non-negative")
})

test_that("invalid inputs fail clearly and missing variables are skipped", {
  dat <- table_one_example()
  expect_error(tableOne(dat[FALSE, ], "height", "group"), "at least one row")
  expect_error(tableOne(dat, "height", "absent"), "colvar")
  expect_error(tableOne(dat, "height", "group", cov_names = "Height"), "named list or vector")
  expect_error(tableCont(dat, "doses", "group"), "must be numeric")
  expect_error(suppressMessages(tableOne(dat, "absent", "group", addNobsTopRow = FALSE)), "No valid row variables")
  expect_message(out <- tableOne(dat, c("absent", "height"), "group"), "missing - omitted")
  expect_identical(out$Variable, c("Observations, n", "height, mean (SD)"))
})

test_that("weighted intervals and tests do not depend on the scale of the weights", {
  set.seed(7)
  dat <- data.frame(g = sample(c("A", "B"), 200, TRUE), c = sample(c("y", "n"), 200, TRUE))
  without_counts <- function(x) sub("^[0-9]+ ", "", x)

  # Constant weights summing to a population total carry no extra information.
  unweighted <- tableCat(dat, "c", "g", statistical_test = TRUE, rowwise_percentages = FALSE)
  dat$w <- 3e5
  population <- tableCat(dat, "c", "g", weights = "w", statistical_test = TRUE,
                         rowwise_percentages = FALSE)
  expect_equal(population$`P-value`, unweighted$`P-value`)
  for (col in c("Overall", "A", "B")) {
    expect_identical(without_counts(population[[col]]), without_counts(unweighted[[col]]))
  }

  dat$w <- sample(1:3, nrow(dat), TRUE)
  small <- tableCat(dat, "c", "g", weights = "w", statistical_test = TRUE)
  dat$w <- dat$w * 1e4
  large <- tableCat(dat, "c", "g", weights = "w", statistical_test = TRUE)
  expect_equal(large$`P-value`, small$`P-value`)
  expect_identical(without_counts(large$A), without_counts(small$A))
})

make_balance_data <- function(seed = 3, n = 300) {
  set.seed(seed)
  dat <- data.frame(g = factor(sample(c("A", "B"), n, TRUE)), age = rnorm(n, 50, 10),
                    sex = factor(sample(c("F", "M"), n, TRUE)),
                    rare = factor(sample(c("x", "y", "z"), n, TRUE, prob = c(0.9, 0.08, 0.02))))
  dat$age[dat$g == "B"] <- dat$age[dat$g == "B"] + 3
  dat$g3 <- factor(sample(c("A", "B", "C"), n, TRUE))
  dat
}

test_that("automatic tests match the data and are recorded", {
  dat <- make_balance_data()
  out <- suppressMessages(tableOne(dat, c("age", "sex", "rare"), "g", statistical_test = TRUE))
  p <- out$`P-value`[out$Variable != " "]
  expect_equal(p[3], chisq.test(table(dat$sex, dat$g))$p.value)
  expect_equal(p[4], fisher.test(table(dat$rare, dat$g))$p.value)
  expect_identical(attr(out, "tests"),
                   c(age = "One-way ANOVA", sex = "Pearson's chi-squared test",
                     rare = "Fisher's exact test"))

  medians <- suppressMessages(tableOne(dat, "age", "g3", summary_stat = "median",
                                       statistical_test = TRUE))
  expect_equal(medians$`P-value`[2], kruskal.test(age ~ g3, data = dat)$p.value)
  forced <- suppressMessages(tableOne(dat, c("age", "rare"), "g3", summary_stat = "median",
                                      statistical_test = TRUE, cont_test = "anova",
                                      cat_test = "chisq"))
  expect_identical(unname(attr(forced, "tests")), c("One-way ANOVA", "Pearson's chi-squared test"))
  expect_error(tableCat(transform(dat, w = 1), "rare", "g", weights = "w", statistical_test = TRUE,
                        cat_test = "fisher"), "unweighted")
})

test_that("standardised mean differences follow the pooled-SD and Yang-Dalton formulas", {
  dat <- make_balance_data()
  out <- suppressMessages(tableOne(dat, c("age", "sex", "rare"), "g", smd = TRUE))
  smd <- out$SMD[!is.na(out$SMD)]

  a <- dat$age[dat$g == "A"]
  b <- dat$age[dat$g == "B"]
  expect_equal(smd[1], abs(mean(a) - mean(b)) / sqrt((var(a) + var(b)) / 2))
  p1 <- mean(dat$sex[dat$g == "A"] == "M")
  p2 <- mean(dat$sex[dat$g == "B"] == "M")
  expect_equal(smd[2], abs(p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2))
  # Three categories: sqrt(d' S^-1 d) with the pooled multinomial covariance.
  shares <- prop.table(table(dat$rare, dat$g), 2)
  d <- (shares[, "A"] - shares[, "B"])[-1]
  cov_of <- function(p) { p <- p[-1]; m <- -outer(p, p); diag(m) <- p * (1 - p); m }
  pooled <- (cov_of(shares[, "A"]) + cov_of(shares[, "B"])) / 2
  expect_equal(smd[3], sqrt(drop(t(d) %*% solve(pooled) %*% d)))

  three <- suppressMessages(tableOne(dat, "age", "g3", smd = TRUE, addNobsTopRow = FALSE))
  ages <- split(dat$age, dat$g3)
  pairwise <- combn(3, 2, function(ix) {
    x <- ages[[ix[1]]]; y <- ages[[ix[2]]]
    abs(mean(x) - mean(y)) / sqrt((var(x) + var(y)) / 2)
  })
  expect_equal(three$SMD, mean(pairwise))
})

test_that("tableOneDisplay renders flextable and gt tables", {
  dat <- make_balance_data()
  out <- suppressMessages(tableOne(dat, c("age", "sex"), "g", statistical_test = TRUE, smd = TRUE))
  skip_if_not_installed("flextable")
  ft <- tableOneDisplay(out, "flextable", title = "Table 1")
  expect_s3_class(ft, "flextable")
  expect_identical(ft$body$dataset$Characteristic[c(1, 3, 4)], c("Observations, n", "sex, n (%)", "F"))
  expect_match(ft$footer$dataset[[1]][1], "One-way ANOVA \\(age\\)")
  skip_if_not_installed("gt")
  expect_s3_class(tableOneDisplay(out, "gt"), "gt_tbl")
  expect_error(tableOneDisplay(data.frame(x = 1)), "tableOne")
})

test_that("tests and SMDs are reported only where they are defined", {
  dat <- data.frame(g = rep(c("a", "b"), each = 5), const = 3, one_cat = "x", v = c(1:5, 3:7),
                    smoke = c("y", "n", NA, "y", "n", "n", "y", NA, "n", "y"))
  out <- suppressMessages(tableOne(dat, c("const", "one_cat", "v", "smoke"), "g",
                                   statistical_test = TRUE, smd = TRUE))
  const <- out$Variable == "const, mean (SD)"
  expect_true(is.na(out$`P-value`[const]))
  expect_false(any(is.nan(out$SMD)))
  expect_true(is.na(out$SMD[const]))
  expect_identical(names(attr(out, "tests")), c("v", "smoke"))
  # Without groups nothing is tested, so no test is named.
  expect_length(attr(suppressMessages(tableOne(dat, c("v", "smoke"), statistical_test = TRUE)), "tests"), 0)

  skip_if_not_installed("gt")
  shown <- tableOneDisplay(out, "gt")[["_data"]]
  expect_true("NA" %in% shown$Characteristic)  # the missing category keeps its label
  expect_false(any(trimws(shown$SMD) %in% c("NA", "NaN")))
  expect_identical(shown$SMD[shown$Characteristic == "const, mean (SD)"], "")
})
