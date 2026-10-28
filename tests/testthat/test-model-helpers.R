make_helper_test_data <- function(seed = 4, n = 600) {
  set.seed(seed)
  dat <- data.frame(x = rnorm(n), z = rnorm(n))
  dat$grp <- factor(sample(c("a", "b", "c"), n, TRUE))
  dat$y <- rbinom(n, 1, plogis(-0.3 + 0.6 * dat$x + 0.4 * (dat$grp == "c")))
  dat
}

run_rd <- function(dat, predictor, outcome = "y", adjusters = "z", ...) {
  suppressMessages(
    ModelMakerMultiRD(dat, predictor, outcome = outcome, joint_adjustment_vars = adjusters,
                      include_rd = FALSE, ncores = 1, ...)
  )
}

test_that("unsupported outcome codings fail before any model is fitted", {
  dat <- make_helper_test_data()
  dat$y12 <- dat$y + 1
  expect_error(run_rd(dat, "x", outcome = "y12"), "coded 1/2; recode it as 0/1")

  dat$y3 <- factor(sample(c("a", "b", "c"), nrow(dat), TRUE))
  expect_error(run_rd(dat, "x", outcome = "y3"), "has 3 categories")

  dat$y_lgl <- dat$y == 1
  expect_true(all(run_rd(dat, "x", outcome = "y_lgl")$plot_output$model_family == "binomial"))
})

test_that("ModelMakerMulti numbers the crude stage 0, like ModelMakerMultiRD", {
  dat <- make_helper_test_data()
  multi <- suppressMessages(ModelMakerMulti(dat, "grp", outcome = "y", joint_adjustment_vars = "z",
                                            ncores = 1))
  expect_equal(sort(unique(multi$plot_output$adjustment)), c(0L, 1L))
  expect_equal(unique(multi$plot_output$adjustment[multi$plot_output$stage_id == "crude"]), 0L)
})

test_that("ordered factors get one estimate per level against the reference", {
  dat <- make_helper_test_data()
  dat$ord <- factor(dat$grp, levels = c("a", "b", "c"), ordered = TRUE)
  res <- suppressMessages(
    ModelMakerMultiRD(dat, "ord", outcome = "y", joint_adjustment_vars = "z",
                      include_rd = TRUE, ncores = 1)
  )
  expect_equal(res$df_output$Category, c("a [reference]", "b", "c"))
  expect_equal(res$df_output_RDs$Category, c("a [reference]", "b", "c"))
})

test_that("cov_name_list labels variables and colliding labels stay separate", {
  dat <- make_helper_test_data()
  dat$age_grp <- dat$x
  dat$`age grp` <- dat$z
  labelled <- run_rd(dat, c("x", "grp"), adjusters = character(0),
                     cov_name_list = list(x = "Exposure score"))
  expect_setequal(unique(labelled$df_output$Variable), c("Exposure score", "Grp"))

  clashing <- run_rd(dat, c("age_grp", "age grp"), adjusters = character(0))
  expect_setequal(unique(clashing$df_output$Variable), c("Age Grp (age_grp)", "Age Grp (age grp)"))
})

test_that("Gaussian coefficient tables use t intervals and support GAMs", {
  set.seed(5)
  small <- data.frame(x = rnorm(8))
  small$y <- 0.8 * small$x + rnorm(8)
  tab <- makeORTable(glm(y ~ x, data = small), dp = NULL)
  expect_equal(unlist(tab[tab$Level == "x", c("Lower", "Upper")], use.names = FALSE),
               unname(confint(lm(y ~ x, data = small))["x", ]))

  dat <- make_helper_test_data()
  gam_tab <- makeORTable(mgcv::gam(z ~ grp + s(x), data = dat))
  expect_equal(gam_tab$Level, c("(Intercept)", "grpb", "grpc"))
  expect_true(all(is.finite(gam_tab$P_value)))
})

test_that("reference rows and failed stages are shown as '-'", {
  dat <- make_helper_test_data()
  multi <- suppressMessages(ModelMakerMulti(dat, "grp", outcome = "y", joint_adjustment_vars = "z",
                                            ncores = 1))
  expect_equal(multi$df_output$crude_mod_OR[1], "-")

  # The adjusted stage cannot be estimated (no reference-level rows), but keeps its column.
  dat$w <- dat$z
  dat$w[dat$grp == "a"] <- NA
  res <- run_rd(dat, "grp", adjusters = "w", sample_strategy = "per_model")
  expect_true("adj_w_OR" %in% names(res$df_output))
  expect_true(all(res$df_output$adj_w_OR == "-"))
})

test_that("checkpoint writes replace files without touching the RNG stream", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "out.rds")
  set.seed(1)
  expected_draw <- runif(1)
  set.seed(1)
  OverReact:::.atomic_write_rds(1, path)
  OverReact:::.atomic_write_rds(2, path)
  expect_identical(runif(1), expected_draw)
  expect_identical(readRDS(path), 2)
  expect_identical(list.files(dir), "out.rds")
})

test_that("GAMModelMaker fits sequential models with the requested reference", {
  dat <- make_helper_test_data(n = 800)
  res <- GAMModelMaker("grp", data = dat, outcome = "y", ref_level = "b",
                       joint_adjustment_vars = c("z", "x"), spline_vars = 2)
  expect_named(res$model_df, c("Variable", "Category", "crude_mod_OR", "plus_z", "plus_spline:x"))
  expect_equal(res$model_df_predictorORs_only$Category, c("(Intercept)", "b [reference]", "grpa", "grpc"))
  expect_equal(unique(res$crude_model_output$Variable), "grp")
})

test_that("modelling functions need the data argument", {
  expect_error(ModelMakerMultiRD(list_of_variables_of_interest = "x"), "dat")
  expect_error(makeTablesNew(covariates = "x", cov_name_list = NULL), "dat")
})
