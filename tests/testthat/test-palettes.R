contrast_with_white <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  1.05 / (drop(c(0.2126, 0.7152, 0.0722) %*% linear) + 0.05)
}

test_that("every palette colour stays visible on the white theme background", {
  for (palette in names(imperial_palettes)) {
    contrast <- vapply(imperial_palettes[[palette]], contrast_with_white, numeric(1))
    expect_true(all(contrast >= 1.15), info = palette)
  }
  expect_false(any(c("White_Smoke", "Yellow") %in% unlist(lapply(imperial_palettes, names))))
  expect_identical(unname(imperial_cols("Yellow")), "#ffff00")
})

test_that("discrete palettes keep their colours and extend with distinct ones", {
  expect_identical(imperial_pal_discrete("soft")(4), unname(imperial_palettes$soft))
  extended <- imperial_pal_discrete("warm")(6)
  expect_length(unique(normalise_hex(extended)), 6)
  expect_identical(extended[1:4], unname(imperial_palettes$warm))
  expect_error(imperial_pal_discrete("warm", fallback = FALSE)(6), "has 4 colours")
})
