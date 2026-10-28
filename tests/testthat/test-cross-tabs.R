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
