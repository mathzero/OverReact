make_prevalence_test_data <- function(seed = 9, n = 2000) {
  set.seed(seed)
  data.frame(res = rbinom(n, 1, 0.05), sex = sample(c("F", "M"), n, TRUE))
}

# makeTablesNew() reports progress with print(), so capture it to keep test output clean.
quietly <- function(code) {
  utils::capture.output(out <- code)
  out
}

test_that("makeTablesNew reports observed prevalence unless assay values are supplied", {
  dat <- make_prevalence_test_data()
  observed <- quietly(makeTablesNew(dat, covariates = "sex", cov_name_list = NULL,
                                    output_list = FALSE))
  expect_equal(observed$Prevalence, round(observed$Positive / observed$Total, 4) * 100)

  corrected <- quietly(makeTablesNew(dat, covariates = "sex", cov_name_list = NULL,
                                     output_list = FALSE, sens = 0.844, spec = 0.986))
  expect_true(all(corrected$Prevalence < observed$Prevalence))
})

test_that("makeTablesNew uses pos_val and neg_val and excludes other outcome codes", {
  dat <- make_prevalence_test_data()
  labelled <- transform(dat, res = ifelse(res == 1, "pos", "neg"))
  labelled$res[1:5] <- "-99"

  expect_equal(
    quietly(makeTablesNew(labelled, pos_val = "pos", neg_val = "neg", covariates = "sex",
                          cov_name_list = NULL, output_list = FALSE)),
    quietly(makeTablesNew(dat[-(1:5), ], covariates = "sex", cov_name_list = NULL,
                          output_list = FALSE))
  )
  expect_error(quietly(makeTablesNew(dat, pos_val = "pos", neg_val = "neg", covariates = "sex",
                                     cov_name_list = NULL)), "No values of 'res' match")
})

test_that("tables with a single observed outcome value keep positives labelled", {
  dat <- data.frame(res = c(1, 1, 1, 0), sex = c("F", "F", "M", "M"))
  all_positive <- quietly(makeTablesNew(dat[dat$res == 1, ], covariates = "sex",
                                        cov_name_list = NULL, output_list = FALSE))
  expect_equal(all_positive$Positive, c(2, 1))
  expect_equal(all_positive$Prevalence, c(100, 100))
})

test_that("stratifiedTables forwards outcome coding", {
  dat <- make_prevalence_test_data()
  dat$res <- ifelse(dat$res == 1, "pos", "neg")
  dat$region <- rep(c("North", "South"), length.out = nrow(dat))

  out <- quietly(stratifiedTables(dat, pos_val = "pos", neg_val = "neg", covariates = "sex",
                                  cov_name_list = NULL, strat_var = "region"))
  expect_named(out, c("Variable", "Category", "North", "South"))
})
