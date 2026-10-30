make_cox_test_data <- function(seed = 3, n = 800) {
  set.seed(seed)
  dat <- data.frame(age = factor(sample(c("18-44", "45-64", "65+"), n, TRUE)),
                    sex = factor(sample(c("F", "M"), n, TRUE)),
                    bmi = rnorm(n, 27, 4))
  lp <- 0.4 * (dat$age == "45-64") + 0.9 * (dat$age == "65+") + 0.3 * (dat$sex == "M")
  event_time <- rexp(n, 0.05 * exp(lp))
  censor_time <- runif(n, 0, 25)
  dat$event <- as.integer(event_time <= censor_time)
  dat$fu <- pmin(event_time, censor_time)
  dat
}

fit_cox <- function(dat, predictors, outcome = "event", time = "fu", ...) {
  suppressMessages(
    ModelMakerMultiRD(dat, predictors, outcome = outcome, time = time,
                      joint_adjustment_vars = "sex", ncores = 1, ...)
  )
}

test_that("Cox models give the hazard ratios, intervals and p-values coxph() gives", {
  skip_if_not_installed("survival")
  dat <- make_cox_test_data()
  res <- fit_cox(dat, c("age", "bmi"))
  plot <- res$plot_output
  expect_true(all(plot$model_family == "cox"))
  expect_true(all(plot$measure == "HR"))
  expect_named(res$df_output, c("Variable", "Category", "Nobs_in_model", "crude_mod_HR", "adj_sex_HR"))

  ref <- survival::coxph(survival::Surv(fu, event) ~ age + sex, data = dat)
  rows <- plot[plot$Variable == "Age" & plot$adjustment == 1 & !plot$is_reference, ]
  expect_equal(rows$Category, c("45-64", "65+"))
  expect_equal(rows$HR, unname(exp(coef(ref))[1:2]))
  expect_equal(rows$Lower, unname(exp(confint(ref))[1:2, 1]))
  expect_equal(rows$Upper, unname(exp(confint(ref))[1:2, 2]))
  expect_equal(rows$P_value, unname(summary(ref)$coefficients[1:2, "Pr(>|z|)"]))

  crude <- survival::coxph(survival::Surv(fu, event) ~ bmi, data = dat)
  expect_equal(plot$HR[plot$Variable == "Bmi" & plot$adjustment == 0], unname(exp(coef(crude))))
})

test_that("family = 'auto' picks Cox when time is given, for any event coding", {
  skip_if_not_installed("survival")
  dat <- make_cox_test_data()
  messages <- capture_messages(ModelMakerMulti(dat, "age", outcome = "event", time = "fu",
                                               joint_adjustment_vars = "sex"))
  expect_true(any(grepl("Assuming Cox proportional hazards models", messages)))
  base <- fit_cox(dat, "age")$plot_output$HR
  dat$died <- ifelse(dat$event == 1, "died", "alive")
  dat$event_lgl <- dat$event == 1
  expect_equal(fit_cox(dat, "age", outcome = "died")$plot_output$HR, base)
  expect_equal(fit_cox(dat, "age", outcome = "event_lgl")$plot_output$HR, base)

  # Every row may be an event, and names need not be syntactic.
  dat$all_events <- 1L
  expect_true(nrow(fit_cox(dat, "age", outcome = "all_events")$plot_output) > 0)
  dat$`follow up` <- dat$fu
  expect_equal(fit_cox(dat, "age", time = "follow up")$plot_output$HR, base)
})

test_that("start and stop times fit counting-process Cox models", {
  skip_if_not_installed("survival")
  dat <- make_cox_test_data()
  dat$start <- runif(nrow(dat), 0, 1)
  dat$stop <- dat$start + dat$fu + 0.01
  res <- suppressMessages(ModelMakerMulti(dat, "age", outcome = "event", time = c("start", "stop"),
                                          joint_adjustment_vars = "sex"))
  ref <- survival::coxph(survival::Surv(start, stop, event) ~ age + sex, data = dat)
  rows <- res$plot_output[res$plot_output$adjustment == 1 & !res$plot_output$is_reference, ]
  expect_equal(rows$HR, unname(exp(coef(ref))[1:2]))
})

test_that("Cox time and event variables are validated", {
  dat <- make_cox_test_data(n = 200)
  run <- function(d = dat, ...) {
    ModelMakerMulti(d, "age", outcome = "event", joint_adjustment_vars = "sex", ...)
  }
  expect_error(run(family = "cox"), "Cox models need `time`")
  expect_error(run(family = "binomial", time = "fu"), "only used with Cox models")
  expect_error(run(time = "missing_col"), "not found in data: missing_col")
  expect_error(run(time = c("fu", "fu", "fu")), "one column, or two")
  dat$fu_chr <- as.character(dat$fu)
  expect_error(run(time = "fu_chr"), "must be numeric")
  negative <- dat
  negative$fu[1] <- -1
  expect_error(run(negative, time = "fu"), "negative values")
  dat$start <- dat$fu
  expect_error(run(time = c("start", "fu")), "must be before stop times")
  coded_12 <- dat
  coded_12$event <- coded_12$event + 1
  expect_error(run(coded_12, time = "fu"), "event indicator coded 0/1")
  none <- dat
  none$event <- 0
  expect_error(run(none, time = "fu"), "has no events")
})

test_that("Cox diagnostics report events, proportional-hazards tests and empty levels", {
  skip_if_not_installed("survival")
  dat <- make_cox_test_data()
  res <- fit_cox(dat, "age")
  diag <- res$diagnostics
  expect_equal(diag$events, rep(sum(dat$event), 2))
  zph <- survival::cox.zph(survival::coxph(survival::Surv(fu, event) ~ age + sex, data = dat))$table
  expect_equal(diag$ph_p_value[diag$stage == "sex"], unname(zph["age", "p"]))
  expect_equal(diag$ph_global_p_value[diag$stage == "sex"], unname(zph["GLOBAL", "p"]))

  # A level with no events has an infinite hazard ratio: flagged, with coxph()'s warning.
  dat$grp <- factor(ifelse(dat$event == 0 & dat$bmi > 33, "none", as.character(dat$sex)))
  flagged <- fit_cox(dat, "grp")$diagnostics
  expect_match(flagged$separation_msg[1], "No events in grp level\\(s\\) none")
  expect_match(flagged$warnings[1], "coefficient may be infinite")
})

test_that("the report names hazard ratios and flags non-proportional hazards", {
  skip_if_not_installed("survival")
  set.seed(11)
  n <- 1000
  dat <- data.frame(arm = factor(sample(c("a", "b"), n, TRUE)), sex = factor(sample(c("F", "M"), n, TRUE)))
  # Arm b's hazard rises over time while arm a's is constant, so hazards are not proportional.
  event_time <- ifelse(dat$arm == "a", rexp(n, 0.1), rweibull(n, shape = 3, scale = 8))
  censor_time <- runif(n, 5, 15)
  dat$event <- as.integer(event_time <= censor_time)
  dat$fu <- pmin(event_time, censor_time)
  out <- suppressMessages(run_auto_tableone_regressions(dat, outcome = "event", covariates = "sex",
                                                        independent_variables = "arm", time = "fu",
                                                        include_rd = TRUE))
  report <- out$report_markdown
  expect_match(report, "**Family**: Cox proportional hazards", fixed = TRUE)
  expect_match(report, "**Follow-up time**: fu", fixed = TRUE)
  expect_match(report, "**Risk difference estimated**: No", fixed = TRUE)
  expect_match(report, "(HR)", fixed = TRUE)
  expect_match(report, "Proportional-hazards tests with p < 0.05: 2 of 2 models", fixed = TRUE)
})

test_that("Cox results skip marginal effects, plot hazard ratios and restore from checkpoints", {
  skip_if_not_installed("survival")
  dat <- make_cox_test_data()
  savepath <- withr::local_tempdir()
  messages <- capture_messages(
    res <- ModelMakerMultiRD(dat, c("age", "bmi"), outcome = "event", time = "fu",
                             joint_adjustment_vars = "sex", include_rd = TRUE, ncores = 1,
                             savepath = savepath)
  )
  expect_true(any(grepl("not estimated for Cox proportional hazards models", messages)))
  expect_null(res$plot_output_RDs)
  expect_identical(plotReactForest(res)$labels$y, "Hazard ratio")

  unlink(file.path(savepath, "runs", "age", c("plot_or.rds", "diagnostics.rds")))
  restored <- fit_cox(dat, c("age", "bmi"), include_rd = TRUE, savepath = savepath)
  expect_equal(restored$plot_output, res$plot_output)
  expect_equal(restored$diagnostics, res$diagnostics)

  # A different follow-up time is a different specification, so it is not reused.
  dat$fu <- dat$fu + runif(nrow(dat), 0, 0.1)
  changed <- fit_cox(dat, c("age", "bmi"), include_rd = TRUE, savepath = savepath)
  expect_false(isTRUE(all.equal(changed$plot_output$HR, res$plot_output$HR)))
})

test_that("single Cox fits work with makeORTable() and plotReactForest()", {
  skip_if_not_installed("survival")
  dat <- make_cox_test_data()
  fit <- survival::coxph(survival::Surv(fu, event) ~ age + bmi, data = dat)
  tab <- makeORTable(fit, dp = NULL)
  expect_equal(tab$OR, unname(exp(coef(fit))))
  expect_true(all(tab$measure == "HR" & tab$model_family == "cox" & !tab$is_intercept))
  expect_identical(plotReactForest(fit)$labels$y, "Hazard ratio")
})
