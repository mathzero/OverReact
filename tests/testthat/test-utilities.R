test_that("p-value formatters keep missing values missing and avoid scientific notation", {
  expect_identical(pvalAsterisker(c(NA, 0.2, 1e-5, 1e-4), return_p = TRUE, return_ns = TRUE),
                   c(NA, "0.2ns", "<0.0001****", "0.0001***"))
  expect_identical(pValEpiConverter(c(NA, 0.2, 0.02)), c(NA, "ns", "p<0.05*"))
  expect_error(pValEpiConverter(0.1, num_asterisks = 5), "3 or 4")

  set.seed(8)
  dat <- data.frame(g = rep(c("A", "B"), each = 200))
  dat$v <- rnorm(400) + (dat$g == "B")
  out <- suppressMessages(tableOne(dat, "v", "g", statistical_test = TRUE,
                                   formatPvalsForEpiPaper = TRUE))
  expect_identical(out$`P-value`[2], "<0.0001****")
})

test_that("calculateEpiStats counts only complete prediction-outcome pairs", {
  stats <- calculateEpiStats(c(1, 0, 1, NA), c(1, 0, NA, 1), returnAllStats = TRUE)
  expect_equal(c(stats$n, stats$n_pos, stats$tp, stats$tn), c(2, 1, 1, 1))
})

test_that("themes honour their arguments without warnings", {
  expect_no_warning(th <- theme_react(base_size = 20, legend.position = "top"))
  expect_equal(th$text$size, 20)
  expect_identical(th$legend.position, "top")
  expect_equal(as.numeric(th$strip.text$margin), rep(2, 4))
  expect_equal(theme_react()$text$size, 10)

  mw <- theme_mw(legend.position = c(1, 1))
  expect_identical(mw$legend.position, "inside")
  expect_equal(mw$legend.position.inside, c(1, 1))
})

test_that("export helpers write matrices, tables and workbooks", {
  dir <- withr::local_tempdir()
  saveREACT(matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y"))), figpath = dir,
            outpath = dir, filename = "mat")
  expect_equal(read.csv(file.path(dir, "mat.csv")), data.frame(row = c("a", "b"), x = 1:2, y = 3:4))

  saveREACTtable(table(c("u", "v", "v")), outpath = paste0(dir, "/prefix_"), filename = "tab")
  expect_true(file.exists(file.path(dir, "prefix_tab.csv")))

  expect_error(savePrettyExcelWorkbook(NULL, outpath = dir), "named list of tables")
  long <- paste0(strrep("a", 30), c("_one", "_two"))
  savePrettyExcelWorkbook(setNames(list(data.frame(v = 1), data.frame(v = 2)), long),
                          workbookName = "wb", outpath = dir)
  expect_identical(openxlsx::getSheetNames(file.path(dir, "wb.xlsx")),
                   c(strrep("a", 30), paste0(strrep("a", 28), "_1")))
  suppressMessages(savePrettyExcelWorkbook(data.frame(Variable = c("x", "x"), v = 1:2),
                                           workbookName = "single", outpath = dir))
  expect_identical(openxlsx::getSheetNames(file.path(dir, "single.xlsx")), "unnamed_table")
})

test_that("cleaners handle factors, dates and invalid options", {
  dates <- factor(c("2021-01-05", "-99", "2021-02-01"))
  expect_equal(dateCleaner(dates, "%Y-%m-%d"), as.Date(c("2021-01-05", NA, "2021-02-01")))
  expect_identical(dateCleaner(as.Date("2021-03-01"), "%d/%m/%Y"), as.Date("2021-03-01"))
  expect_identical(reactCleaner(dates, datatype = "date", dateFormat = "%Y-%m-%d",
                                dateFormatOut = "%d/%m/%Y"),
                   c("05/01/2021", NA, "01/02/2021"))
  expect_error(reactCleaner(1:3, datatype = "numeric"), "Please select datatype")

  df <- data.frame(`my var` = c("1", "2"), codes = factor(c("01", "1")), num = c("3", NA),
                   check.names = FALSE)
  out <- checkConvertNumeric(df)
  expect_named(out, c("my var", "codes", "num"))
  expect_equal(out$`my var`, c(1, 2))
  expect_identical(out$codes, df$codes)
  expect_equal(out$num, c(3, NA))
})

test_that("reporting helpers order, validate and count correctly", {
  bmi <- data.frame(Variable = "BMI category",
                    Category = c("Obese", "Normal weight", "Underweight", "Overweight"))
  expect_identical(orderMyTable(bmi)$Category, c("Underweight", "Normal weight", "Overweight", "Obese"))
  empty <- data.frame(Variable = character(), Category = character())
  expect_identical(orderMyTable(empty), empty)
  covid <- data.frame(Variable = "Covid",
                      Category = c("No COVID", "COVID suspected by respondent",
                                   "COVID confirmed by test", "COVID suspected by doctor"))
  expect_identical(orderMyTable(covid)$Category,
                   c("COVID confirmed by test", "COVID suspected by doctor",
                     "COVID suspected by respondent", "No COVID"))

  f <- data.frame(g = factor(c("a", "a"), levels = c("a", "b")))
  issues <- validate_factor_levels(f, "g")$issues$g
  expect_match(issues[1], "Zero counts in levels: b")
  expect_match(issues[2], "less than 2 levels")

  codes <- data.frame(q1 = c(1, -91, -92, -77, -99, NA), q2 = c("a", "b", "-99", "c", "d", "e"))
  report <- naReporter(codes)
  expect_identical(report$variable, c("q1", "q2"))
  expect_equal(report$non_response, c(2, 0))
  expect_equal(report$prefer_not_to_say, c(1, 1))
  expect_equal(report$responses, c(1, 5))
})

test_that("makeXtabPlottable extracts percentages from cross-tabs", {
  dat <- data.frame(y = c("n", "y", "y", "n", "y"), g = c("a", "a", "b", "b", "b"))
  xt <- suppressMessages(crossTabMulti(dat, "y", "g", confint = FALSE))
  long <- makeXtabPlottable(xt)
  expect_named(long, c("Variable", "Category", "name", "value"))
  expect_true(is.numeric(long$value))
  expect_true(all(long$value >= 0 & long$value <= 100))
})
