make_rd_test_data <- function(seed = 1, n = 3000) {
  set.seed(seed)
  dat <- data.frame(x = rnorm(n, mean = 3), z = rnorm(n))
  dat$grp <- factor(sample(c("a", "b", "c"), n, TRUE))
  dat$y <- rbinom(n, 1, plogis(-2 + 0.5 * dat$x + 0.3 * dat$z + 0.4 * (dat$grp == "c")))
  dat
}

logistic_ame <- function(fit, term) {
  p <- fitted(fit)
  mean(p * (1 - p)) * coef(fit)[[term]]
}

test_that("numeric RDs are average marginal effects and do not depend on location", {
  dat <- make_rd_test_data()
  fit <- glm(y ~ x + z, family = binomial, data = dat)

  rd <- makeRDTable(fit, "x", data = dat, dp = NULL)
  expect_equal(rd$RD, logistic_ame(fit, "x"), tolerance = 1e-6)
  expect_true(rd$Lower < rd$RD && rd$RD < rd$Upper)

  dat$x_shifted <- dat$x + 10
  shifted <- glm(y ~ x_shifted + z, family = binomial, data = dat)
  expect_equal(makeRDTable(shifted, "x_shifted", data = dat, dp = NULL)$RD,
               logistic_ame(fit, "x"), tolerance = 1e-6)
})

test_that("numeric RDs are not confused by predictors sharing a name prefix", {
  dat <- make_rd_test_data()
  dat$age <- rnorm(nrow(dat), 50, 10)
  dat$age_group <- cut(dat$age, c(-Inf, 40, 60, Inf))
  fit <- glm(y ~ age + age_group, family = binomial, data = dat)

  rd <- makeRDTable(fit, "age", data = dat, dp = NULL)
  expect_equal(rd$RD, logistic_ame(fit, "age"), tolerance = 1e-6)
  expect_true(is.finite(rd$Lower) && is.finite(rd$Upper))
})

test_that("delta-method intervals agree with a large simulation", {
  dat <- make_rd_test_data()
  fit <- glm(y ~ grp + z, family = binomial, data = dat)

  delta <- makeRDTable(fit, "grp", dp = NULL)
  sim <- makeRDTable(fit, "grp", dp = NULL, ci_method = "simulation", n_sim = 4000)
  expect_equal(delta$Level, c("b", "c"))
  expect_equal(delta$RD, sim$RD)
  expect_equal(delta$Upper - delta$Lower, sim$Upper - sim$Lower, tolerance = 0.1)
})

test_that("simulation intervals are reproducible and leave the caller's RNG stream alone", {
  dat <- make_rd_test_data()
  fit <- glm(y ~ grp + z, family = binomial, data = dat)

  set.seed(42)
  expected_draw <- runif(1)
  set.seed(42)
  first <- makeRDTable(fit, "grp", ci_method = "simulation", n_sim = 50)
  expect_identical(runif(1), expected_draw)
  expect_identical(makeRDTable(fit, "grp", ci_method = "simulation", n_sim = 50), first)
})

test_that("RDs standardise over the fitted rows when given the full data", {
  dat <- make_rd_test_data()
  dat$z[1:50] <- NA
  fit <- glm(y ~ grp + z, family = binomial, data = dat)

  expect_equal(makeRDTable(fit, "grp", data = dat, dp = NULL), makeRDTable(fit, "grp", dp = NULL))
})

test_that("ModelMakerMultiRD reports correct numeric RDs", {
  dat <- make_rd_test_data()
  res <- suppressMessages(
    ModelMakerMultiRD(dat, "x", outcome = "y", joint_adjustment_vars = "z",
                      include_rd = TRUE, ncores = 1)
  )
  fit <- glm(y ~ x + z, family = binomial, data = dat)

  adjusted <- res$plot_output_RDs[res$plot_output_RDs$adjustment == 1, , drop = FALSE]
  expect_equal(adjusted$RD, logistic_ame(fit, "x"), tolerance = 1e-6)
})
