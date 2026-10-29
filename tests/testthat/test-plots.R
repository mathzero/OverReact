test_that("plotReactForest clips out-of-range intervals instead of dropping them", {
  plot_df <- data.frame(
    Variable = "V", Category = c("a", "b", "c"),
    OR = c(2, 20, 3), Lower = c(1.5, 12, 0.1), Upper = c(2.5, 35, 90),
    P_value = c(0.01, 0.001, 0.4), adjustment = 0L, model = "crude"
  )

  expect_warning(p <- plotReactForest(plot_df), "outside xrange")
  built <- ggplot2::ggplot_build(p)
  layer_data <- function(geom) {
    built$data[[which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))]]
  }
  expect_equal(sum(!is.na(layer_data("GeomPoint")$y)), 3)
  errorbars <- layer_data("GeomErrorbar")
  expect_equal(sum(!is.na(errorbars$ymin) & !is.na(errorbars$ymax)), 3)

  expect_no_warning(plotReactForest(plot_df, xrange = c(1 / 16, 128)), message = "outside xrange")
})

test_that("plotReactForest can print each row's estimate, interval, p and N", {
  set.seed(6)
  n <- 800
  dat <- data.frame(x = rnorm(n), z = rnorm(n), grp = factor(sample(c("a", "b", "c"), n, TRUE)))
  dat$y <- rbinom(n, 1, plogis(-1 + 0.5 * dat$x + 0.4 * (dat$grp == "c")))
  res <- suppressMessages(
    ModelMakerMultiRD(dat, c("x", "grp"), outcome = "y", joint_adjustment_vars = "z", ncores = 1)
  )

  p <- plotReactForest(res, show_estimates = TRUE)
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(text_layers, 2)
  labels <- text_layers[[1]]$data$estimate_text
  adjusted <- res$plot_output[res$plot_output$adjustment == 1 & res$plot_output$Category == "c", ]
  expected <- sprintf("%.2f (%.2f, %.2f), p=%.3f, N=800", adjusted$OR, adjusted$Lower,
                      adjusted$Upper, adjusted$P_value)
  expect_true(expected %in% labels)
  expect_true("" %in% labels)  # the reference row has no estimate
  expect_identical(text_layers[[2]]$data$header, "OR (95% CI), p, N")

  crude <- plotReactForest(res, show_estimates = TRUE, estimate_adjustment = 0)
  expect_false(identical(Filter(function(l) inherits(l$geom, "GeomText"), crude$layers)[[1]]$data$estimate_text,
                         labels))
  expect_error(plotReactForest(res, adjustment_numbers = 1, show_estimates = TRUE,
                               estimate_adjustment = 0), "not among the plotted")
  expect_s3_class(plotReactForest(glm(y ~ x + grp, binomial, dat), show_estimates = TRUE), "ggplot")
})
