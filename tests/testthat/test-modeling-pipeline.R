make_model_test_data <- function(seed = 123, n = 1000) {
  set.seed(seed)
  dat <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    stringsAsFactors = FALSE
  )
  dat$cat <- factor(sample(c("Case", "Control"), n, TRUE))
  dat$abcat <- factor(sample(c("A", "B", "C"), n, TRUE), levels = c("A", "B", "C"))
  dat$char_pred <- ifelse(dat$cat == "Case", "Yes", "No")
  dat$char_adj <- sample(c("North", "South"), n, TRUE)
  lp <- -0.4 + 0.8 * dat$x1 + 0.5 * (dat$cat == "Case") -
    0.3 * (dat$abcat == "B") + 0.2 * (dat$abcat == "C")
  dat$y_bin <- rbinom(n, 1, plogis(lp))
  dat
}

test_that("ModelMakerMulti keeps predictor rows and categorical levels", {
  dat <- make_model_test_data()

  res_keep <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat", "abcat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      ncores = 1,
      remove_intercept_from_results = FALSE
    )
  )
  res_drop <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat", "abcat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )

  x1_keep <- res_keep$df_output[res_keep$df_output$Variable == "X1", , drop = FALSE]
  x1_drop <- res_drop$df_output[res_drop$df_output$Variable == "X1", , drop = FALSE]
  cat_drop <- res_drop$df_output[res_drop$df_output$Variable == "Cat", , drop = FALSE]
  abcat_drop <- res_drop$df_output[res_drop$df_output$Variable == "Abcat", , drop = FALSE]

  expect_true("(Intercept)" %in% x1_keep$Category)
  expect_true("x1" %in% x1_keep$Category)
  expect_false("(Intercept)" %in% x1_drop$Category)
  expect_equal(x1_drop$Category, "x1")
  expect_equal(cat_drop$Category, c("Case [reference]", "Control"))
  expect_equal(abcat_drop$Category, c("A [reference]", "B", "C"))
  expect_true(any(res_keep$plot_output$is_intercept, na.rm = TRUE))
  expect_false(any(res_drop$plot_output$is_intercept, na.rm = TRUE))
})

test_that("ModelMakerMulti preserves self-adjustment stages and numbering", {
  dat <- make_model_test_data(n = 500)

  res <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = "x1",
      outcome = "y_bin",
      joint_adjustment_vars = c("x1", "x2"),
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )

  x1_row <- res$df_output[res$df_output$Variable == "X1", , drop = FALSE]
  x1_plot <- res$plot_output[res$plot_output$Variable == "X1", , drop = FALSE]

  expect_equal(x1_row$Category, "x1")
  expect_false(is.na(x1_row$plus_x1))
  expect_equal(x1_row$plus_x1, x1_row$crude_mod_OR)
  expect_equal(x1_plot$stage_id, c("crude", "plus_x1", "plus_x2"))
  expect_equal(x1_plot$adjustment, c(0L, 1L, 2L))
  expect_equal(
    x1_plot$OR[x1_plot$stage_id == "plus_x1"],
    x1_plot$OR[x1_plot$stage_id == "crude"]
  )
})

test_that("binary outcomes with missing values stay binomial", {
  dat <- make_model_test_data()
  dat$y_bin[sample(seq_len(nrow(dat)), 25)] <- NA

  res <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2"),
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )

  expect_true("OR" %in% names(res$plot_output))
  expect_false("Beta" %in% names(res$plot_output))
  expect_true(all(res$plot_output$model_family == "binomial"))
})

test_that("ModelMakerMultiRD standardises character handling and common samples", {
  dat <- make_model_test_data()
  dat$x2[sample(seq_len(nrow(dat)), 100)] <- NA
  dat$char_adj[sample(seq_len(nrow(dat)), 50)] <- NA

  res <- suppressMessages(
    ModelMakerMultiRD(
      dat = dat,
      list_of_variables_of_interest = c("char_pred"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "char_adj"),
      include_rd = FALSE,
      ncores = 1,
      sample_strategy = "common_per_predictor",
      remove_intercept_from_results = TRUE
    )
  )

  plot_rows <- res$plot_output[res$plot_output$Variable == "Char Pred", , drop = FALSE]
  diag_rows <- res$diagnostics[res$diagnostics$Variable == "Char Pred", , drop = FALSE]

  expect_equal(plot_rows$Category, c("No [reference]", "Yes", "No [reference]", "Yes", "No [reference]", "Yes"))
  expect_length(unique(plot_rows$Nobs_in_model), 1)
  expect_length(unique(diag_rows$nobs), 1)
})

test_that("legacy and RD wrappers agree on OR outputs when RD is disabled", {
  dat <- make_model_test_data()

  legacy <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat", "abcat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )$plot_output

  rd <- suppressMessages(
    ModelMakerMultiRD(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat", "abcat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      include_rd = FALSE,
      incremental = TRUE,
      include_crude = TRUE,
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )$plot_output

  legacy$adj_index <- legacy$adjustment
  rd$adj_index <- rd$adjustment

  cols <- c("Variable", "Category", "adj_index", "OR", "Lower", "Upper", "P_value")
  legacy_cmp <- legacy[, cols, drop = FALSE]
  rd_cmp <- rd[, cols, drop = FALSE]

  legacy_cmp <- legacy_cmp[order(legacy_cmp$Variable, legacy_cmp$Category, legacy_cmp$adj_index), , drop = FALSE]
  rd_cmp <- rd_cmp[order(rd_cmp$Variable, rd_cmp$Category, rd_cmp$adj_index), , drop = FALSE]

  expect_equal(legacy_cmp, rd_cmp, tolerance = 1e-8)
})

test_that("ModelMakerMulti handles non-syntactic variable names", {
  dat <- data.frame(
    check.names = FALSE,
    "predictor with space" = rnorm(300),
    "adj var" = rnorm(300),
    y_bin = rbinom(300, 1, 0.5)
  )

  res <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = "predictor with space",
      outcome = "y_bin",
      joint_adjustment_vars = "adj var",
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )

  expect_equal(res$df_output$Category, "predictor with space")
  expect_true(any(grepl("predictor with space", res$plot_output$term_raw, fixed = TRUE)))
})

test_that("plotReactForest accepts metadata-enriched batch outputs", {
  dat <- make_model_test_data(n = 400)
  res <- suppressMessages(
    ModelMakerMultiRD(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      include_rd = FALSE,
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )

  p <- plotReactForest(res, variables = c("X1", "Cat"))
  expect_s3_class(p, "ggplot")
})

test_that("run_auto_tableone_regressions works with metadata-enriched outputs", {
  dat <- make_model_test_data(n = 300)
  res <- suppressMessages(
    run_auto_tableone_regressions(
      dat = dat,
      outcome = "y_bin",
      covariates = c("x2", "x3"),
      independent_variables = c("x1", "cat"),
      include_rd = FALSE,
      ncores = 1,
      top_k = 2
    )
  )

  expect_named(res, c("table_one", "model_outputs", "report_markdown"))
  expect_true(nrow(res$model_outputs$plot_output) > 0)
})

test_that("ModelMakerMulti parallel and sequential outputs match", {
  skip_on_cran()

  dat <- make_model_test_data(n = 400)
  seq_res <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      ncores = 1,
      remove_intercept_from_results = TRUE
    )
  )
  par_res <- suppressMessages(
    ModelMakerMulti(
      dat = dat,
      list_of_variables_of_interest = c("x1", "cat"),
      outcome = "y_bin",
      joint_adjustment_vars = c("x2", "x3"),
      ncores = 2,
      remove_intercept_from_results = TRUE
    )
  )

  expect_equal(seq_res$df_output, par_res$df_output)
  expect_equal(seq_res$plot_output, par_res$plot_output, tolerance = 1e-8)
})

test_that("the automated report keeps predictors that are also covariates", {
  dat <- make_model_test_data(n = 600)
  res <- suppressMessages(
    run_auto_tableone_regressions(
      dat = dat,
      outcome = "y_bin",
      covariates = c("x1", "x2"),
      independent_variables = c("x1", "x2", "x3", "cat"),
      include_rd = FALSE,
      ncores = 1
    )
  )

  bullets <- grep("^- \\*\\*", strsplit(res$report_markdown, "\n")[[1]], value = TRUE)
  expect_length(bullets, 4)
  # x1 is never adjusted for itself, so its most-adjusted model is the one with x2 only.
  expect_match(grep("^- \\*\\*X1\\*\\*", bullets, value = TRUE), "\\[x2\\]$")
})

test_that("numeric outputs keep full precision", {
  set.seed(3)
  dat <- data.frame(income = runif(2000, 10000, 90000), z = rnorm(2000))
  dat$y <- rbinom(2000, 1, plogis(-1 + 2e-5 * (dat$income - 50000)))

  res <- suppressMessages(
    ModelMakerMultiRD(dat, "income", outcome = "y", joint_adjustment_vars = "z",
                      include_rd = FALSE, ncores = 1)
  )
  fit <- glm(y ~ income + z, family = binomial, data = dat)
  adjusted <- res$plot_output[res$plot_output$adjustment == 1, , drop = FALSE]

  expect_equal(adjusted$OR, exp(coef(fit)[["income"]]))
  expect_equal(adjusted$P_value, summary(fit)$coefficients["income", "Pr(>|z|)"])
  expect_true(adjusted$Lower < adjusted$OR && adjusted$OR < adjusted$Upper)
})

test_that("checkpoints are reused only for the same specification and data", {
  dat <- make_model_test_data(n = 400)
  savepath <- withr::local_tempdir()
  run <- function(adjusters, data = dat) {
    ModelMakerMultiRD(dat = data, list_of_variables_of_interest = "cat", outcome = "y_bin",
                      joint_adjustment_vars = adjusters, include_rd = FALSE, ncores = 1,
                      savepath = savepath)
  }

  suppressMessages(run("x2"))
  messages <- capture_messages(changed_spec <- run(c("x2", "x3")))
  expect_true(any(grepl("different specification or data", messages)))
  expect_true("adj_x2+x3_OR" %in% names(changed_spec$df_output))

  messages <- capture_messages(reused <- run(c("x2", "x3")))
  expect_true(any(grepl("Skipping already-completed variables: cat", messages)))
  expect_equal(reused$df_output, changed_spec$df_output)

  changed_data <- dat
  changed_data$y_bin[1:20] <- 1 - changed_data$y_bin[1:20]
  messages <- capture_messages(run(c("x2", "x3"), data = changed_data))
  expect_true(any(grepl("different specification or data", messages)))
})

test_that("per-model samples keep one reference level and one row per level", {
  set.seed(11)
  n <- 800
  dat <- data.frame(y = rbinom(n, 1, 0.4), z = rnorm(n))
  dat$g <- factor(sample(c("a", "b", "c"), n, TRUE))
  run <- function(adjuster) {
    suppressMessages(
      ModelMakerMultiRD(dat, "g", outcome = "y", joint_adjustment_vars = adjuster,
                        include_rd = FALSE, ncores = 1, sample_strategy = "per_model")
    )
  }

  # The adjusted sample has no reference-level rows: report a failed stage, not a new reference.
  dat$w <- dat$z
  dat$w[dat$g == "a"] <- NA
  lost_reference <- run("w")
  expect_equal(lost_reference$df_output$Category, c("a [reference]", "b", "c"))
  expect_false(any(lost_reference$plot_output$Category == "b [reference]"))
  stage_w <- lost_reference$diagnostics[lost_reference$diagnostics$stage == "w", , drop = FALSE]
  expect_true(all(stage_w$status == "error"))
  expect_match(stage_w$error_msg[1], "Reference level 'a'")

  # A non-reference level drops out: its row stays in place with no adjusted estimate.
  dat$w2 <- dat$z
  dat$w2[dat$g == "b"] <- NA
  lost_level <- run("w2")
  expect_equal(lost_level$df_output$Category, c("a [reference]", "b", "c"))
  expect_equal(lost_level$df_output$adj_w2_OR[2], "-")
})

test_that("a supplied reference level is used for estimation, not just labelling", {
  dat <- make_model_test_data(n = 600)
  default <- modelMakerAdjustedSetsRD("cat", data = dat, outcome = "y_bin",
                                      adj_sets = list(a = "x2"))$crude_model_OR
  control <- modelMakerAdjustedSetsRD("cat", data = dat, outcome = "y_bin", ref_level = "Control",
                                      adj_sets = list(a = "x2"))$crude_model_OR

  expect_equal(control$Level[!control$is_intercept], c("Control [reference]", "Case"))
  expect_equal(control$OR[control$Level == "Case"],
               1 / default$OR[default$Level == "Control"], tolerance = 1e-6)
})

test_that("checkpoints never return an earlier run's table, and CSV copies restore tables exactly", {
  dat <- make_model_test_data(n = 400)
  dat$y_cont <- rnorm(nrow(dat))
  savepath <- withr::local_tempdir()
  run <- function(outcome) {
    suppressMessages(
      ModelMakerMultiRD(dat, "cat", outcome = outcome, joint_adjustment_vars = "x2",
                        include_rd = TRUE, include_rr = TRUE, ncores = 1, savepath = savepath)
    )
  }

  binary <- run("y_bin")
  # Gaussian outcomes have no RD or RR tables, on the first run or when reusing it.
  expect_null(run("y_cont")$df_output_RDs)
  reused <- run("y_cont")
  expect_null(reused$df_output_RDs)
  expect_null(reused$df_output_RRs)

  binary <- run("y_bin")
  unlink(file.path(savepath, "runs", "cat", "plot_rr.rds"))
  restored <- run("y_bin")
  expect_equal(restored$plot_output_RRs, binary$plot_output_RRs)
  expect_equal(restored$plot_output, binary$plot_output)
})

test_that("a predictor that is also an adjuster keeps the shared stage numbers", {
  dat <- make_model_test_data(n = 800)
  res <- suppressMessages(
    ModelMakerMultiRD(dat, c("x1", "x2"), outcome = "y_bin", joint_adjustment_vars = c("x2", "x3"),
                      include_rd = TRUE, ncores = 1)
  )
  stages <- function(plot, var) sort(unique(plot$adjustment[plot$Variable == var]))
  expect_equal(stages(res$plot_output, "X1"), 0:2)
  # x2 is never adjusted for itself: its stages are crude and x3 (stage 2, fully adjusted).
  expect_equal(stages(res$plot_output, "X2"), c(0, 2))
  expect_equal(stages(res$plot_output_RDs, "X2"), c(0, 2))

  # Each predictor's fully adjusted model shares a stage, and so a q-value family.
  full <- res$plot_output[res$plot_output$adjustment == 2 & !res$plot_output$is_reference, ]
  expect_setequal(full$Variable, c("X1", "X2"))
  expect_equal(full$Q_value, p.adjust(full$P_value, "BH"))
  forest <- plotReactForest(res, adjustment_numbers = c(0, 2))
  expect_setequal(as.character(unique(forest$data$Variable[forest$data$adjustment == "2"])),
                  c("X1", "X2"))
})
