# ==============================================================================
# OverReact: End-to-End Package Demo
# ==============================================================================
# Showcases data cleaning, missingness reporting, Table 1 & tableOneDisplay(),
# prevalence & stratified prevalence tables, single-model & sequential batch
# regressions (logistic OR/RD/RR, modified Poisson, Poisson rates, ordinal, and
# Cox proportional-hazards models), publication-ready regression tables via
# modelOutputDisplay(), forest & VIF plots, automated analysis & reporting,
# epidemiological utilities, Imperial themes/palettes, and export helpers.

library(OverReact)
library(ggplot2)

set.seed(123)

# 1. Simulate study dataset ----------------------------------------------------
n <- 800
dat <- data.frame(
  age_years = round(rnorm(n, mean = 50, sd = 15)),
  sex = factor(sample(c("Female", "Male"), n, replace = TRUE)),
  region = factor(
    sample(c("North", "Midlands", "South"), n, replace = TRUE, prob = c(0.30, 0.30, 0.40)),
    levels = c("South", "Midlands", "North")
  ),
  deprivation = factor(
    sample(paste0("Q", 1:5), n, replace = TRUE),
    levels = paste0("Q", 1:5)
  ),
  smoking_status = factor(
    sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.55, 0.25, 0.20)),
    levels = c("Never", "Former", "Current")
  ),
  exposure_primary = rnorm(n),
  bmi = rnorm(n, mean = 27, sd = 4),
  survey_weight = runif(n, min = 0.7, max = 1.4),
  follow_up_years = runif(n, 0.5, 3),
  sample_date_chr = format(
    as.Date("2022-01-01") + sample(0:120, n, replace = TRUE),
    "%d/%m/%Y"
  ),
  binary_1_2 = sample(c(1, 2, -99), n, replace = TRUE, prob = c(0.24, 0.72, 0.04)),
  binary_1_0 = sample(c(0, 1, -77), n, replace = TRUE, prob = c(0.68, 0.28, 0.04)),
  continuous_dirty = ifelse(runif(n) < 0.04, -1, rnorm(n, mean = 10, sd = 3))
)

# Binary, continuous, count, and ordinal outcomes
linpred <- -1.1 + 0.55 * dat$exposure_primary + 0.03 * (dat$age_years - 50) +
  0.35 * (dat$sex == "Male") + 0.45 * (dat$smoking_status == "Current") +
  0.25 * (dat$region == "North")
dat$outcome_bin <- rbinom(n, size = 1, prob = plogis(linpred))
dat$outcome_group <- factor(
  dat$outcome_bin,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)
dat$outcome_cont <- 2 + 0.65 * dat$exposure_primary - 0.2 * (dat$sex == "Male") + rnorm(n)
dat$infections <- rpois(n, dat$follow_up_years * exp(-0.9 + 0.35 * dat$exposure_primary))
dat$severity <- factor(
  sample(c("None", "Mild", "Severe"), n, replace = TRUE, prob = c(0.60, 0.25, 0.15)),
  levels = c("None", "Mild", "Severe")
)
dat$predicted_flag <- as.integer(dat$exposure_primary + rnorm(n, sd = 0.8) > 0.5)

# Right-censored survival outcome with proportional hazards
hazard <- 0.015 * exp(
  0.45 * dat$exposure_primary +
    0.025 * (dat$age_years - 50) +
    0.30 * (dat$sex == "Male") +
    0.40 * (dat$smoking_status == "Current")
)
event_time <- rexp(n, rate = hazard)
censor_time <- runif(n, min = 25, max = 140)
dat$time_days <- pmin(event_time, censor_time)
dat$event <- as.integer(event_time <= censor_time)

# Introduce realistic missingness and labels
dat$region[sample(seq_len(n), 15)] <- NA
dat$smoking_status[sample(seq_len(n), 20)] <- NA
attr(dat$exposure_primary, "label") <- "Primary exposure score"

labels <- c(
  age_years = "Age (years)",
  sex = "Sex",
  region = "Region",
  smoking_status = "Smoking status",
  bmi = "Body mass index",
  deprivation = "Deprivation quintile",
  exposure_primary = "Primary exposure score"
)


# 2. Data cleaning & missingness helpers ---------------------------------------
cleaned_preview <- data.frame(
  raw_1_2 = dat$binary_1_2[1:6],
  clean_1_2 = binaryCleaner(dat$binary_1_2[1:6]),
  raw_1_0 = dat$binary_1_0[1:6],
  clean_1_0 = binaryCleaner_1_0(dat$binary_1_0[1:6]),
  raw_cont = round(dat$continuous_dirty[1:6], 2),
  clean_cont = round(continuousCleaner(dat$continuous_dirty[1:6], floornumber = 0), 2),
  clean_date = as.character(dateCleaner(dat$sample_date_chr[1:6], dateFormat = "%d/%m/%Y"))
)
print(cleaned_preview)

na_summary <- naReporter(
  dat[, c("age_years", "region", "smoking_status", "binary_1_2", "binary_1_0")]
)
print(na_summary)


# 3. Cross-tabulations, Table 1 & tableOneDisplay() ----------------------------
ct_smoking_sex <- crossTab(
  dat = dat,
  rowvar = "smoking_status",
  colvar = "sex",
  include_percentages = TRUE,
  confint = FALSE
)
print(ct_smoking_sex)

tab1 <- tableOne(
  dat = dat,
  rowvars = c("age_years", "bmi", "sex", "smoking_status", "region"),
  colvar = "outcome_group",
  cov_names = labels,
  statistical_test = TRUE,
  smd = TRUE
)
print(tab1)
print(attr(tab1, "tests"))

if (requireNamespace("gt", quietly = TRUE)) {
  tab1_gt <- tableOneDisplay(tab1, output = "gt", title = "Baseline characteristics by outcome")
  print(tab1_gt)
}


# 4. Prevalence & stratified prevalence tables ---------------------------------
prevalence <- makeTablesNew(
  dat = dat,
  result_var = "outcome_bin",
  covariates = c("sex", "smoking_status", "region"),
  cov_name_list = labels,
  output_list = FALSE
)
print(prevalence)

strat_prev <- stratifiedTables(
  dat = dat,
  result_var = "outcome_bin",
  covariates = c("smoking_status", "region"),
  cov_name_list = labels,
  strat_var = "sex",
  include_counts = FALSE
)
print(strat_prev)

strat_plot_df <- makeStratifiedPrevalenceTablePlottable(strat_prev)
print(head(strat_plot_df))


# 5. Single-model effect tables (OR, RD, RR) -----------------------------------
fit_glm <- glm(
  outcome_bin ~ exposure_primary + smoking_status + age_years + sex,
  family = binomial(),
  data = dat
)
print(makeORTable(fit_glm, dp = 2))
print(makeRDTable(fit_glm, variable_name = "smoking_status", data = dat, n_sim = 50, dp = 3))
print(makeRRTable(fit_glm, variable_name = "smoking_status", data = dat, n_sim = 50, dp = 2))


# 6. Sequential & batch regression models --------------------------------------

# 6a. Binary outcomes: ORs, marginal RDs, marginal RRs, and BH q-values
models_bin <- ModelMakerMultiRD(
  dat = dat,
  list_of_variables_of_interest = c("exposure_primary", "smoking_status", "bmi"),
  outcome = "outcome_bin",
  joint_adjustment_vars = c("age_years", "sex", "region"),
  cov_name_list = labels,
  include_rd = TRUE,
  include_rr = TRUE,
  n_sim = 50,
  ncores = 1
)
print(models_bin$df_output)
print(models_bin$df_output_RDs)
print(models_bin$df_output_RRs)

# 6b. Modified Poisson (robust RR), Poisson rates (IRR), and Ordinal/Multinomial
models_mod_pois <- ModelMakerMultiRD(
  dat = dat,
  list_of_variables_of_interest = c("exposure_primary", "smoking_status"),
  outcome = "outcome_bin",
  joint_adjustment_vars = c("age_years", "sex"),
  cov_name_list = labels,
  family = "modified_poisson",
  ncores = 1
)
print(models_mod_pois$df_output)

models_rates <- ModelMakerMultiRD(
  dat = dat,
  list_of_variables_of_interest = "exposure_primary",
  outcome = "infections",
  joint_adjustment_vars = c("age_years", "sex"),
  cov_name_list = labels,
  family = "poisson",
  offset = "follow_up_years",
  ncores = 1
)
print(models_rates$df_output)

models_severity <- ModelMakerMultiRD(
  dat = dat,
  list_of_variables_of_interest = "smoking_status",
  outcome = "severity",
  joint_adjustment_vars = "age_years",
  cov_name_list = labels,
  ncores = 1
)
print(models_severity$df_output)

# 6c. Cox proportional-hazards models (HR, event counts, Schoenfeld PH tests)
models_cox <- ModelMakerMultiRD(
  dat = dat,
  list_of_variables_of_interest = c("exposure_primary", "smoking_status", "sex"),
  outcome = "event",
  time = "time_days",
  joint_adjustment_vars = c("age_years", "region"),
  cov_name_list = labels,
  ncores = 1
)
print(models_cox$df_output)
print(models_cox$diagnostics[, c(
  "Variable", "stage", "adjusted_vars", "nobs", "events",
  "ph_p_value", "ph_global_p_value"
)])


# 7. Publication-ready regression tables via modelOutputDisplay() --------------
reg_display_df <- modelOutputDisplay(
  models_bin,
  output = "data.frame",
  stages = "crude_and_full",
  stage_labels = c(
    crude = "Unadjusted",
    `adj_age_years+sex+region` = "Fully adjusted"
  ),
  include_rd = TRUE,
  include_rr = TRUE,
  include_p = TRUE,
  include_q = TRUE
)
print(reg_display_df)

cox_display_df <- modelOutputDisplay(
  models_cox,
  output = "data.frame",
  stages = "crude_and_full",
  stage_labels = c(
    crude = "Unadjusted",
    `adj_age_years+region` = "Adjusted for age & region"
  ),
  include_events = TRUE,
  include_p = TRUE
)
print(cox_display_df)

if (requireNamespace("gt", quietly = TRUE)) {
  reg_display_gt <- modelOutputDisplay(
    models_bin,
    output = "gt",
    stages = "crude_and_full",
    include_rd = TRUE,
    title = "Crude and adjusted associations with outcome"
  )
  print(reg_display_gt)
}


# 8. Forest plots & collinearity diagnostics -----------------------------------
p_forest_or <- plotReactForest(
  models_bin,
  adjustment_numbers = c(0, 3),
  adjustment_descriptions = c("Crude", "Adjusted for age, sex and region"),
  show_estimates = TRUE,
  title = "Odds ratios from ModelMakerMultiRD()"
)
print(p_forest_or)

p_forest_rd <- plotReactForest(
  models_bin,
  effect = "RD",
  adjustment_numbers = c(0, 3),
  adjustment_descriptions = c("Crude", "Adjusted"),
  title = "Marginal risk differences"
)
print(p_forest_rd)

p_forest_hr <- plotReactForest(
  models_cox,
  effect = "HR",
  adjustment_numbers = c(0, 2),
  adjustment_descriptions = c("Crude", "Adjusted for age and region"),
  show_estimates = TRUE,
  title = "Hazard ratios from Cox models"
)
print(p_forest_hr)

model_list <- list(
  Crude = glm(outcome_bin ~ exposure_primary + smoking_status, binomial, dat),
  Adjusted = glm(outcome_bin ~ exposure_primary + smoking_status + age_years + sex, binomial, dat)
)
p_forest_list <- plotReactForest(
  model_list,
  variables = c("exposure_primary", "smoking_status"),
  title = "Comparing fitted models directly"
)
print(p_forest_list)

vif_df <- data.frame(
  Variable = rep(c("Primary exposure score", "Body mass index"), each = 4),
  adjustment = rep(0:3, times = 2),
  vif = c(1.00, 1.08, 1.15, 1.24, 1.00, 1.12, 1.35, 1.62)
)
p_vif <- plotReactVIF(
  vif_df,
  adjustment_numbers = 0:3,
  adjustment_descriptions = c("Crude", "+ Age", "+ Sex", "+ Region")
)
print(p_vif)


# 9. Automated analysis & narrative report -------------------------------------
res_auto <- run_auto_tableone_regressions(
  dat = dat,
  outcome = "outcome_bin",
  covariates = c("age_years", "sex"),
  independent_variables = c("exposure_primary", "smoking_status", "bmi", "age_years"),
  cov_names = labels,
  include_rd = TRUE,
  n_sim = 30,
  ncores = 1,
  report_path = file.path(tempdir(), "overreact_auto_report.md")
)
cat(res_auto$report_markdown)
print(modelOutputDisplay(res_auto, output = "data.frame", stages = "crude_and_full"))


# 10. Epidemiological & formatting utilities -----------------------------------
epi_stats <- calculateEpiStats(
  prediction = dat$predicted_flag,
  outcome = dat$outcome_bin,
  returnAllStats = TRUE
)
print(epi_stats)

p_demo <- c(0.42, 0.038, 0.004, 0.00002)
print(data.frame(
  raw_p = p_demo,
  epi_format = pValEpiConverter(p_demo),
  asterisks = pvalAsterisker(p_demo, return_p = TRUE),
  decimal_3sf = specifyDecimal(c(pi, exp(1), 0.0456, 12.3), k = 3)
))


# 11. Themes, Imperial palettes & export helpers -------------------------------
p_box <- ggplot(dat, aes(x = sex, y = exposure_primary, fill = sex)) +
  geom_boxplot(width = 0.6) +
  scale_fill_imperial(palette = "cool") +
  theme_react() +
  labs(title = "theme_react() with Imperial 'cool' palette", x = NULL, y = "Primary exposure score")
print(p_box)

out_dir <- file.path(tempdir(), "overreact-demo")
dir.create(out_dir, showWarnings = FALSE)
saveREACTtable(prevalence, outpath = out_dir, filename = "prevalence")
saveREACTplot(p_box, figpath = out_dir, filename = "exposure_boxplot", width = 6, height = 4, savePDF = FALSE)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  savePrettyExcelWorkbook(
    listOfTables = list(`Table 1` = tab1, Prevalence = prevalence, Regression = reg_display_df),
    workbookName = "demo_tables",
    outpath = out_dir
  )
}
print(sort(list.files(out_dir)))
