make_q_test_data <- function(seed = 6, n = 1200) {
  set.seed(seed)
  dat <- data.frame(x = rnorm(n), z = rnorm(n), w = rnorm(n))
  dat$grp <- factor(sample(c("a", "b", "c"), n, TRUE))
  dat$y <- rbinom(n, 1, plogis(-1 + 0.5 * dat$x + 0.4 * (dat$grp == "c")))
  dat
}

test_that("q-values adjust each stage's p-values across predictors and levels", {
  dat <- make_q_test_data()
  res <- suppressMessages(
    ModelMakerMultiRD(dat, c("x", "grp", "w"), outcome = "y", joint_adjustment_vars = "z",
                      include_rd = TRUE, ncores = 1)
  )
  plot <- res$plot_output
  expect_identical(match("Q_value", names(plot)), match("P_value", names(plot)) + 1L)
  for (stage in unique(plot$adjustment)) {
    tests <- plot$adjustment == stage & !plot$is_reference
    expect_equal(plot$Q_value[tests], p.adjust(plot$P_value[tests], "BH"))
  }
  expect_true(all(is.na(plot$Q_value[plot$is_reference])))
  expect_true("Q_value" %in% names(res$plot_output_RDs))

  holm <- suppressMessages(
    ModelMakerMulti(dat, c("x", "grp"), outcome = "y", joint_adjustment_vars = "z",
                    ncores = 1, p_adjust_method = "holm")
  )$plot_output
  crude <- holm$adjustment == 0 & !holm$is_reference
  expect_equal(holm$Q_value[crude], p.adjust(holm$P_value[crude], "holm"))

  none <- suppressMessages(
    ModelMakerMultiRD(dat, "x", outcome = "y", joint_adjustment_vars = "z", include_rd = FALSE,
                      ncores = 1, p_adjust_method = "none")
  )
  expect_false("Q_value" %in% names(none$plot_output))
})

test_that("the automated report shows q-values for the tests it ranks", {
  dat <- make_q_test_data()
  res <- suppressMessages(
    run_auto_tableone_regressions(dat, "y", covariates = "z",
                                  independent_variables = c("x", "grp", "w"), ncores = 1)
  )
  lines <- strsplit(res$report_markdown, "\n")[[1]]
  expect_true(any(grepl("^\\*\\*Multiple testing\\*\\*", lines)))
  bullets <- grep("^- \\*\\*", lines, value = TRUE)
  expect_true(all(grepl(", q=", bullets, fixed = TRUE)))

  plain <- suppressMessages(
    run_auto_tableone_regressions(dat, "y", covariates = "z", independent_variables = "x",
                                  ncores = 1, p_adjust_method = NULL)
  )
  expect_false(grepl("q=", plain$report_markdown, fixed = TRUE))
})
