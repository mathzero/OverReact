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
