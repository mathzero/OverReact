make_family_test_data <- function(seed = 6, n = 1500) {
  set.seed(seed)
  dat <- data.frame(x = rnorm(n), z = rnorm(n))
  dat$grp <- factor(sample(c("a", "b", "c"), n, TRUE))
  dat$y <- rbinom(n, 1, plogis(-1 + 0.5 * dat$x + 0.4 * (dat$grp == "c")))
  dat$time <- runif(n, 0.5, 3)
  dat$count <- rpois(n, dat$time * exp(-0.5 + 0.3 * dat$x))
  dat$score <- cut(dat$x + rnorm(n), c(-Inf, -0.5, 0.5, Inf), labels = c("low", "mid", "high"),
                   ordered_result = TRUE)
  dat$type <- factor(sample(c("none", "mild", "severe"), n, TRUE), levels = c("none", "mild", "severe"))
  dat
}

fit_family <- function(dat, predictor, outcome, family, adjusters = "z", ...) {
  suppressMessages(
    ModelMakerMultiRD(dat, predictor, outcome = outcome, joint_adjustment_vars = adjusters,
                      family = family, include_rd = FALSE, ncores = 1, ...)
  )
}

adjusted_row <- function(res, category) {
  plot <- res$plot_output
  plot[plot$adjustment == max(plot$adjustment) & plot$Category == category, , drop = FALSE]
}

test_that("Poisson models report incidence rate ratios with an offset", {
  dat <- make_family_test_data()
  res <- fit_family(dat, "x", "count", "poisson", offset = "time")
  fit <- glm(count ~ x + z + offset(log(time)), family = poisson, data = dat)

  row <- adjusted_row(res, "x")
  expect_equal(row$IRR, exp(coef(fit)[["x"]]))
  expect_true("crude_mod_IRR" %in% names(res$df_output))
  expect_true(all(res$plot_output$model_family == "poisson"))
  expect_error(fit_family(dat, "x", "y", "binomial", offset = "time"), "only used with Poisson")
})

test_that("modified Poisson models report risk ratios with sandwich standard errors", {
  skip_if_not_installed("sandwich")
  dat <- make_family_test_data()
  res <- fit_family(dat, "grp", "y", "modified_poisson")
  fit <- glm(y ~ grp + z, family = poisson, data = dat)
  se <- sqrt(diag(sandwich::sandwich(fit)))[["grpc"]]

  row <- adjusted_row(res, "c")
  expect_equal(row$RR, exp(coef(fit)[["grpc"]]))
  expect_equal(row$Upper, exp(coef(fit)[["grpc"]] + qnorm(0.975) * se))
  expect_equal(row$P_value, 2 * pnorm(-abs(coef(fit)[["grpc"]] / se)))
})

test_that("log-binomial models converge and agree with modified Poisson", {
  dat <- make_family_test_data()
  log_binomial <- fit_family(dat, "grp", "y", "log_binomial")
  modified <- fit_family(dat, "grp", "y", "modified_poisson")
  expect_true(all(log_binomial$diagnostics$status == "ok"))
  expect_equal(adjusted_row(log_binomial, "c")$RR, adjusted_row(modified, "c")$RR, tolerance = 0.02)
})

test_that("ordered outcomes get proportional-odds models", {
  dat <- make_family_test_data()
  res <- fit_family(dat, "x", "score", "auto")
  fit <- MASS::polr(score ~ x + z, data = dat, Hess = TRUE)

  expect_true(all(res$plot_output$model_family == "ordinal"))
  expect_equal(adjusted_row(res, "x")$OR, exp(coef(fit)[["x"]]), tolerance = 1e-6)
})

test_that("unordered categorical outcomes get multinomial models with one block per level", {
  dat <- make_family_test_data()
  res <- fit_family(dat, "grp", "type", "auto")
  fit <- nnet::multinom(type ~ grp + z, data = dat, trace = FALSE)

  expect_true(all(res$plot_output$model_family == "multinomial"))
  expect_identical(unique(res$df_output$Outcome), c("mild", "severe"))
  expect_equal(res$df_output$Category, rep(c("a [reference]", "b", "c"), 2))
  severe_c <- res$plot_output[res$plot_output$outcome_level == "severe" &
                                res$plot_output$Category == "c" & res$plot_output$adjustment == 1, ]
  expect_equal(severe_c$OR, exp(coef(fit)["severe", "grpc"]), tolerance = 1e-4)
  expect_s3_class(plotReactForest(res), "ggplot")
})

test_that("marginal risk ratios are ratios of standardised risks", {
  dat <- make_family_test_data()
  res <- suppressMessages(
    ModelMakerMultiRD(dat, c("grp", "x"), outcome = "y", joint_adjustment_vars = "z",
                      include_rd = TRUE, include_rr = TRUE, ncores = 1)
  )
  fit_grp <- glm(y ~ grp + z, family = binomial, data = dat)
  risk <- function(level) mean(predict(fit_grp, transform(dat, grp = factor(level, levels(dat$grp))),
                                       type = "response"))
  rr <- res$plot_output_RRs
  expect_equal(rr$RR[rr$Variable == "Grp" & rr$Category == "c" & rr$adjustment == 1],
               risk("c") / risk("a"), tolerance = 1e-8)

  # Numeric predictors: exp of the change in log mean risk per unit, AME / mean risk.
  fit_x <- glm(y ~ x + z, family = binomial, data = dat)
  p <- fitted(fit_x)
  expect_equal(rr$RR[rr$Variable == "x" & rr$adjustment == 1],
               exp(mean(p * (1 - p)) * coef(fit_x)[["x"]] / mean(p)), tolerance = 1e-6)
  expect_true(all(c("crude_mod_RR", "adj_z_RR") %in% names(res$df_output_RRs)))
  expect_s3_class(plotReactForest(res, effect = "RR"), "ggplot")
})

test_that("makeRRTable delta-method intervals agree with simulation", {
  dat <- make_family_test_data()
  fit <- glm(y ~ grp + z, family = binomial, data = dat)
  delta <- makeRRTable(fit, "grp", dp = NULL)
  sim <- makeRRTable(fit, "grp", dp = NULL, ci_method = "simulation", n_sim = 4000)
  expect_equal(delta$RR, sim$RR)
  expect_equal(log(delta$Upper / delta$Lower), log(sim$Upper / sim$Lower), tolerance = 0.1)
})

test_that("coefficient tables name their measure and exponentiate only ratios", {
  dat <- make_family_test_data()
  probit <- makeORTable(glm(y ~ x, family = binomial(link = "probit"), data = dat), dp = NULL)
  expect_identical(unique(probit$measure), "Coef")
  expect_equal(probit$OR[probit$Level == "x"],
               unname(coef(glm(y ~ x, family = binomial(link = "probit"), data = dat))["x"]))

  gaussian <- fit_family(dat, "grp", "x", "auto")
  expect_true(all(c("crude_mod_Beta", "adj_z_Beta") %in% names(gaussian$df_output)))
  expect_true("Beta" %in% names(gaussian$plot_output))
  # No refits: Gaussian outcomes have no risk differences to wait for.
  expect_true(all(gaussian$diagnostics$attempt == 1L))
})

test_that("families are validated against the outcome", {
  dat <- make_family_test_data()
  expect_error(fit_family(dat, "x", "count", "modified_poisson"), "needs a binary outcome")
  expect_error(fit_family(dat, "x", "type", "gaussian"), "needs a numeric outcome")
  expect_error(fit_family(dat, "x", "y", "ordinal"), "at least three outcome categories")
})
