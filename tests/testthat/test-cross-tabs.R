test_that("weighted crossTabContinuous summarises each observed group", {
  dat <- data.frame(v = c(1, 2, 3, 4, 5), g = c("a", "a", "b", "b", NA), w = c(1, 1, 2, 2, 1))
  out <- crossTabContinuous(dat, "v", "g", weights = "w")
  expect_identical(out$a, "1.5 (0.71)")
  expect_identical(out$b, "3.5 (0.58)")
})

test_that("weighted crossTab counts use the weights", {
  dat <- data.frame(y = c("n", "y", "y", "n"), g = c("a", "a", "b", "b"), w = c(1, 2, 3, 4))
  out <- crossTab(dat, "y", "g", weights = "w", include_percentages = FALSE)
  expect_equal(as.numeric(out$a), c(1, 2, 3))
  expect_equal(as.numeric(out$b), c(4, 3, 7))
})

test_that("weighted crossTab percentages, intervals and test follow the weights", {
  dat <- data.frame(y = c("n", "y", "y", "n", "y", "n"), g = c("a", "a", "b", "b", "a", "b"),
                    w = c(1, 3, 2, 4, 2, 1))
  out <- crossTab(dat, "y", "g", weights = "w", confint = FALSE)
  # Weighted rows: n = (1, 5) and y = (5, 2); unweighted percentages would be 33.3% and 66.7%.
  expect_identical(out$a, c("1 (16.7%)", "5 (71.4%)", "6 (46.2%)"))

  set.seed(3)
  big <- data.frame(y = sample(c("n", "y"), 300, TRUE), g = sample(c("a", "b", "c"), 300, TRUE),
                    w = sample(1:3, 300, TRUE))
  big$w_scaled <- big$w * 1000
  big$w_constant <- 2
  run <- function(weights) suppressWarnings(crossTab(big, "y", "g", weights = weights,
                                                     statistical_test = TRUE))
  # The text after the count: percentage and interval.
  shares <- function(out) lapply(out[c("a", "b", "c")], function(col) sub("^[0-9]+ ", "", col))

  # Kish effective sample size of the row's weights.
  in_row <- big$y == "y"
  x <- sum(big$w[in_row & big$g == "a"])
  n <- sum(big$w[in_row])
  ess <- sum(big$w[in_row])^2 / sum(big$w[in_row]^2)
  ci <- .prop_ci(x / n * ess, ess)
  expect_identical(run("w")$a[2], paste0(x, " (", round(100 * ci$p, 1), "%, [", round(100 * ci$lower, 1),
                                         "-", round(100 * ci$upper, 1), "])"))

  expect_identical(shares(run("w_scaled")), shares(run("w")))
  expect_equal(run("w_scaled")$pval[1], run("w")$pval[1])
  expect_identical(shares(run("w_constant")), shares(run(NULL)))
  expect_equal(run("w_constant")$pval[1], run(NULL)$pval[1])
})

test_that("crossTabMulti summarises classed numeric columns as continuous", {
  dat <- data.frame(g = rep(c("a", "b"), each = 4), v = c(1, 2, 3, 4, 2, 4, 6, 8))
  dat$v <- structure(dat$v, class = c("labelled_score", "numeric"))
  out <- suppressMessages(crossTabMulti(dat, "v", "g"))
  expect_identical(out$Category, "Mean (SD)")
  expect_identical(out$a, "2.5 (1.29)")
  expect_error(crossTabMulti(dat, character(), "g"), "at least one variable")
})
