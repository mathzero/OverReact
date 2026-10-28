# OverReact

OverReact provides descriptive tables, regression summaries, prevalence estimates
and graphics for epidemiological research. It was developed for the Imperial
REACT-2 study of COVID-19. Functions operate on data frames and return tabular
results, model diagnostics or `ggplot2` objects.

## Installation

The current source requires R ≥ 4.1. Install the development branch from GitHub:

```r
install.packages("remotes")
remotes::install_github("mathzero/OverReact", ref = "dev")

library(OverReact)
```

Specify analysis data, outcomes and adjustment variables explicitly. Several
legacy defaults refer to REACT-specific objects, variable names or assay
characteristics.

## Analytical workflow

| Task | Principal functions |
| --- | --- |
| Describe a study population | `tableOne()`, `tableCat()`, `tableCont()` |
| Estimate associations | `ModelMakerMulti()`, `ModelMakerMultiRD()` |
| Estimate prevalence | `makeTablesNew()`, `stratifiedTables()` |
| Combine description and regression | `run_auto_tableone_regressions()` |
| Plot estimates and diagnostics | `plotReactForest()`, `plotReactVIF()` |
| Export results | `saveREACTtable()`, `saveREACTplot()`, `savePrettyExcelWorkbook()` |

The examples below use synthetic data:

```r
set.seed(42)
n <- 600
clinical <- data.frame(
  age = round(rnorm(n, mean = 50, sd = 15)),
  sex = factor(sample(c("Female", "Male"), n, replace = TRUE)),
  vaccinated = factor(
    sample(c("No", "Yes"), n, replace = TRUE),
    levels = c("No", "Yes")
  ),
  status = factor(
    sample(c("Asymptomatic", "Non-persistent symptoms", "Long COVID", NA),
           n, replace = TRUE, prob = c(0.20, 0.55, 0.24, 0.01)),
    levels = c("Asymptomatic", "Non-persistent symptoms", "Long COVID")
  )
)
clinical$positive <- rbinom(
  n, size = 1,
  prob = plogis(-2 + 0.02 * (clinical$age - 50) -
                  0.6 * (clinical$vaccinated == "Yes"))
)
```

## Descriptive tables

| Function | Purpose and output |
| --- | --- |
| `tableOne()` | Combine summaries of `rowvars`, optionally stratified by `colvar`. Returns `Variable`, `Level`, `Missing`, `Overall`, group columns and optional `P-value`; an observation-count row is included by default. |
| `tableCat()` | Tabulate one categorical variable, with counts, row or column percentages, optional Wilson confidence intervals and a chi-squared test. Returns a block with a `units` column. |
| `tableCont()` | Summarise one numeric variable as mean (SD) or median (IQR), overall and by group; optionally add an ANOVA p-value. Returns one row with a `units` column. |
| `calculate_prop_ci()` | Calculate a binomial proportion and 95% Wilson or normal-approximation interval from a count `x` and positive integer denominator `n`. Returns `proportion`, `lower` and `upper`. |

```r
table_one <- tableOne(
  dat = clinical,
  rowvars = c("age", "sex", "vaccinated"),
  colvar = "status",
  cov_names = c(age = "Age (years)", sex = "Sex", vaccinated = "Vaccination"),
  summary_stat = "mean",
  rowwise_percentages = TRUE,
  includeNAsColvar = TRUE,
  includeNAsRowvar = FALSE,
  statistical_test = TRUE
)
```

Numeric variables containing only observed zeros and ones are categorical;
other numeric variables are continuous. Use factors to specify categorical
variables and their level order. Entirely missing numeric variables remain
continuous.

`rowwise_percentages = TRUE` describes the distribution across displayed groups
within each row level; `FALSE` describes the distribution of row levels within
each group. The legacy spelling, `rowwise_precentages`, remains valid in
`tableOne()` and `tableCat()`.

`includeNAsColvar` and `includeNAsRowvar` independently control missing groups
and missing categorical row levels. `Overall` and `Missing` describe the supplied
population even when missing groups are hidden. Continuous summaries omit
missing measurements. Empty groups return missing continuous summaries or
undefined categorical percentages (`NA%`). Group labels retain spaces and
punctuation; genuine collisions receive numeric suffixes. A literal `"NA"` group
and a missing group therefore remain distinct.

With `statistical_test = TRUE`, categorical comparisons use Pearson's
chi-squared test, with Yates' continuity correction for 2 × 2 tables. Continuous
comparisons use the standard one-way ANOVA F-test, including when medians are
displayed. Tests use complete cases for the variable and group, excluding empty
levels and displayed missing categories. They are omnibus comparisons without
multiplicity adjustment. No test is attached to the observation-count row, and
unidentifiable comparisons return missing p-values. Exact tests, Welch ANOVA
and non-parametric alternatives are not selected automatically.

`confint = TRUE` adds 95% Wilson intervals to categorical percentages;
`formatPvalsForEpiPaper = TRUE` formats available p-values with asterisks.
Categorical weights must name a non-negative finite numeric column; missing
weights are excluded and weighted cell totals are rounded for display. Weighted
intervals and chi-squared tests use the Kish effective sample size,
`sum(w)^2 / sum(w^2)`, of each denominator rather than the weighted total, so
rescaling the weights does not change them and constant weights reproduce the
unweighted results. This accounts for unequal weighting only, not clustering or
stratification. Weighted continuous summaries are unsupported and raise an error.

See [the table documentation](man/tableOne.Rd) and
[implementation](R/make_table_one.R).

## Regression models

| Function | Purpose and output |
| --- | --- |
| `ModelMakerMulti()` | Fit crude and cumulatively adjusted models for several predictors. Returns formatted `df_output`, numeric `plot_output` and `diagnostics`. |
| `ModelMakerMultiRD()` | Fit several predictors using cumulative or named adjustment sets, with optional risk differences, parallel execution, retries and disk checkpoints. Returns separate coefficient and risk-difference outputs plus diagnostics. |
| `modelMakerSequential()` | Fit one predictor with sequential adjustment. Returns `model_df`, `model_df_predictorORs_only`, `crude_model_output`, `adj_model_outputs` and diagnostics. |
| `modelMakerSequentialRD()` | Fit one predictor with cumulative adjustment and optional risk differences. Returns separate OR and RD tables, crude and adjusted results, diagnostics and sample counts. |
| `modelMakerAdjustedSetsRD()` | Fit one predictor using named `adj_sets`. Returns `model_df_ORs`, `model_df_RDs`, `crude_model_OR`, `crude_model_RD`, `adj_or_outputs`, `adj_rd_outputs`, `diagnostics` and sample counts. |
| `makeORTable()` | Extract coefficients, 95% normal-approximation Wald intervals and model-summary p-values from a fitted model. Binomial coefficients are exponentiated; other coefficients retain their original scale. Optional reference rows and term metadata are included; `dp = NULL` returns unrounded estimates. |
| `makeRDTable()` | Estimate marginal risk differences for a factor or numeric predictor in a binomial model, as described below. Returns `Level`, `RD`, `Lower`, `Upper` and `P_value`; intervals use the delta method, or coefficient simulation with `ci_method = "simulation"`. |
| `safe_makeRDTable()` | Wrap `makeRDTable()` with error capture and an explicit reference row for factor predictors. Returns `NULL` on a caught error. |

The modelling wrappers select a binomial logit model when the outcome has two
distinct non-missing values, otherwise a Gaussian identity model. Encode binary
outcomes as 0/1, logical values or a two-level factor. Multinomial regression is
not implemented. Character predictors and adjusters are converted to factors;
unused levels are dropped within the analysis sample.

```r
models <- ModelMakerMultiRD(
  dat = clinical,
  list_of_variables_of_interest = c("vaccinated", "sex"),
  outcome = "positive",
  incremental = FALSE,
  adj_sets = list(demographic = c("age", "sex")),
  include_crude = TRUE,
  include_rd = TRUE,
  sample_strategy = "common_per_predictor",
  ncores = 1
)

models$df_output
models$df_output_RDs
models$diagnostics
```

With `incremental = TRUE`, adjusters enter cumulatively in the order supplied
in `joint_adjustment_vars`; otherwise `adj_sets` specifies named models. A
predictor is removed from its own adjustment set. The default
`sample_strategy = "common_per_predictor"` uses complete cases across that
predictor, the outcome and all its requested adjusters, preserving a common
sample across adjustment stages. `"per_model"` permits the sample to vary by
stage. Different predictors may still have different analysis samples. Each
predictor's reference level and level order are fixed from its largest analysis
sample; under `"per_model"`, a stage whose sample lacks the reference level is
reported as failed in `diagnostics` instead of being estimated against another
reference. A `ref_level` supplied to the single-predictor wrappers is used for
estimation as well as labelling.

### Results and diagnostics

| `ModelMakerMultiRD()` element | Contents |
| --- | --- |
| `df_output` | Formatted coefficient or odds-ratio tables across model stages. |
| `plot_output` | Numeric estimates, `Lower`, `Upper`, `P_value`, reference indicators, stage labels and `Nobs_in_model`. |
| `df_output_RDs` | Formatted risk-difference tables; `NULL` when unavailable. |
| `plot_output_RDs` | Numeric risk differences and intervals; `NULL` when unavailable. |
| `diagnostics` | Stage, adjustment variables, status, errors, captured warnings, sample size, covariance validity, separation flags and attempt number. |

Gaussian models are labelled `Beta` in batch plotting outputs. Some lower-level
tables retain `OR` in column names for compatibility; consult `model_family`
before interpreting estimates. Coefficient intervals use the normal
approximation; p-values are taken from the fitted model summary. Numeric outputs
are unrounded; only the formatted tables are rounded.

Risk differences are estimated by standardisation over the analysis sample. For
a factor predictor, predicted risks are averaged with every observation set to
each level and contrasted with the reference level. For a numeric predictor, the
estimate is the average marginal effect: the mean derivative of predicted risk
per one-unit increase. Intervals use the delta method by default;
`rd_ci_method = "simulation"` instead takes the 2.5th and 97.5th percentiles of
`n_sim` multivariate-normal coefficient draws, using a fixed seed without
disturbing the session's random-number stream. P-values use a normal
approximation. These are model-based associations; causal interpretation
requires additional assumptions. Risk differences are produced only for binomial
models; `include_rd = FALSE` disables them.

`ncores = NULL` or `1` runs sequentially. Larger values request a `future`
backend. `ModelMakerMultiRD()` can retry incomplete results with revised GLM
controls and recover from parallel failures. Supplying `savepath` writes
aggregate CSV/RDS results and per-predictor checkpoints under `runs/`. Each
checkpoint stores a fingerprint of the model specification and the predictor's
data columns; a checkpoint that does not match the current call is recomputed
rather than reused. Checkpoints written by earlier versions have no fingerprint
and are recomputed once.

### Supporting functions

| Function | Purpose and output |
| --- | --- |
| `validate_factor_levels()` | Drop unused levels in specified factor columns; return the amended `data` and an `issues` list identifying inadequate factor levels. |
| `check_perfect_separation()` | Flag zero cells in a categorical predictor × binary outcome table. Returns a message or `NULL`; this is a screening heuristic, not a general separation diagnostic. |
| `build_cumulative_sets()` | Convert an ordered vector of adjusters to a named list of cumulative sets. |
| `compute_level_order()` | Return factor levels with a labelled reference first; return `NULL` for non-factors. |
| `get_pretty_name()` | Resolve a display label from `options(modelmaker.name_map = ...)`, a variable's `label` attribute, or automatic title casing; an optional `name_fun` transforms the fallback label. |

`cov_name_list` is retained in batch-model signatures but currently does not
apply labels. Use `modelmaker.name_map`, column labels or `name_fun` instead.
See [the model wrappers](R/bulk_run_models_ORs_RDs.R) and
[coefficient-only interface](R/make_OR_tables.R).

## Prevalence estimation

| Function | Purpose and output |
| --- | --- |
| `makeTablesNew()` | Estimate binary-outcome prevalence by each covariate, with optional weights, assay correction and report formatting. Returns a named list, or one data frame with `output_list = FALSE`. |
| `stratifiedTables()` | Repeat prevalence estimation within `strat_var` levels and return a wide table of interval strings; `include_counts = TRUE` also includes positive and total counts. |
| `add_conf_ints()` | Add binomial intervals and sensitivity/specificity correction to a count matrix with positive and negative rows. Returns a transposed matrix with totals, observed prevalence and adjusted estimates and bounds. |
| `makeStratifiedPrevalenceTablePlottable()` | Convert wide prevalence interval strings to long data with numeric `prev`, `lower` and `upper`. Expects comma-separated intervals and no additional count columns. |
| `orderMyTable()` | Reorder `Variable`/`Category` tables using REACT category conventions, including age, deprivation, smoking and response frequencies. |

```r
prevalence_table <- makeTablesNew(
  dat = clinical,
  result_var = "positive",
  pos_val = "1", neg_val = "0",
  covariates = c("sex", "vaccinated"),
  cov_name_list = c(sex = "Sex", vaccinated = "Vaccination"),
  sens = 1, spec = 1,
  percent = TRUE,
  output_list = FALSE
)
```

`add_conf_ints()` uses Wilson intervals by default. Assay correction applies
`(p + spec - 1) / (sens + spec - 1)` to the estimate and interval bounds,
constraining results to [0, 1]. Sensitivity and specificity are treated as fixed;
their uncertainty is not propagated. `makeTablesNew()` and `stratifiedTables()`
default to `sens = 1` and `spec = 1`, which return uncorrected estimates; supply
assay values explicitly to correct for test performance. (Earlier versions
silently applied the REACT assay values `sens = 0.844` and `spec = 0.986`.)

Numeric `makeTablesNew()` output contains `Positive`, `Total`, `Prevalence`,
`Lower` and `Upper`; prevalence columns refer to assay-adjusted estimates, which
equal the observed estimates under the defaults. `percent = TRUE` expresses these
on a percentage scale. `for_report = TRUE` returns observed and adjusted
estimates as formatted percentage intervals. Supply `cov_name_list` explicitly,
including `NULL` when labels are unnecessary. `pos_val` and `neg_val` name the
outcome's positive and negative values; other values, such as missing-data
codes, are excluded.

Weighted counts are rounded before binomial interval calculation. This is not
survey-design inference. `stratifiedTables()` forwards outcome coding, assay
values, weights and `percent` to `makeTablesNew()`.
See [the prevalence implementation](R/make_prevalence_tables.R).

## Automated analysis

`run_auto_tableone_regressions()` combines `tableOne()` and
`ModelMakerMultiRD()`. Supply `dat`, `outcome`, adjustment `covariates` and
`independent_variables`. It returns `table_one`, `model_outputs` and
`report_markdown`; `save_dir` and `report_path` optionally save results and the
report.

The descriptive table is stratified when the outcome is factor, character or
logical, or numeric with at most `outcome_cat_unique_threshold` distinct values
(default 6). This rule controls table layout, independently of the regression
family. `include_rd` defaults to `FALSE`; supplying `NULL` enables automatic
selection for binary outcomes. The report ranks associations by p-value,
selecting one category per predictor from that predictor's most-adjusted model;
a predictor that is also a covariate is not adjusted for itself. Treat this
ranking as exploratory, particularly when adjustment sets are not nested.
See [the driver](R/automated_analysis.R).

## Figures, themes and colours

| Function | Purpose and output |
| --- | --- |
| `plotReactForest()` | Create a forest plot from batch-model results, numeric plotting data, an `lm`/`glm`/`gam` fit, or a named list of fits. Supports `effect = "OR"`, `"Beta"` or `"RD"`, adjustment labels, reference rows and variable selection. |
| `plotReactVIF()` | Plot externally supplied variance inflation factors by adjustment stage. Requires `Variable`, `adjustment` and `vif`; batch model wrappers do not currently return VIFs. |
| `theme_react()` | Apply the REACT theme with white backgrounds, small text and configurable facet and title styling. |
| `theme_mw()` | Apply a related theme with panel borders, ticks and configurable grids and legend placement. |
| `imperial_cols()` | Retrieve named colours; with no arguments, return the complete named vector. |
| `imperial_pal_discrete()` | Return a function generating discrete colours, retaining the specified palette and optionally extending it with distinct HCL colours. |
| `imperial_pal_continuous()` | Return a palette interpolation function using CIELAB colour space. |
| `scale_color_new()`, `scale_fill_new()` | Apply package palettes to `ggplot2` colour or fill aesthetics; select discrete or continuous scales with `discrete`. |
| `scale_color_imperial()`, `scale_fill_imperial()` | Aliases of the corresponding `*_new()` scales. |
| `normalise_hex()` | Trim whitespace and convert colour strings to uppercase. |
| `hex_to_lab()` | Convert R colour specifications to a matrix of CIELAB coordinates. |
| `pick_fallback_colours()` | Select additional qualitative HCL colours, favouring separation from existing colours in CIELAB space. |

```r
forest <- plotReactForest(models, effect = "OR", title = "Adjusted associations")
forest

# Risk differences from the same fitted models.
plotReactForest(models, effect = "RD", title = "Marginal risk differences")

ggplot2::ggplot(clinical, ggplot2::aes(x = age, fill = sex)) +
  ggplot2::geom_histogram(binwidth = 10, position = "identity", alpha = 0.5) +
  scale_fill_imperial(palette = "two_col_blue_green") +
  theme_react()
```

Forest plots use a logarithmic odds-ratio scale with null value 1, and linear
coefficient or risk-difference scales with null value 0. `xrange` sets the
visible odds-ratio range (default 1/4 to 16); estimates or interval bounds beyond
it are clipped at the panel edge, with a warning. Pass numeric
`plot_output` data rather than formatted table strings.

`imperial_colours` stores named hexadecimal colours; `imperial_palettes` stores
palette definitions. Available palettes are `default`, `gradient`, `earth`,
`cool`, `warm`, `soft`, `bold`, `nature`, `two_col_grey_teal`,
`two_col_pink_purple` and `two_col_blue_green`. Discrete palettes extend by
default; `fallback = FALSE` enforces the original palette length.
See [plots](R/plots.R), [themes](R/ggplot_themes.R) and
[palette functions](R/imperial_colour_scheme_functions.R).

## Data preparation and reporting utilities

| Function | Purpose and output |
| --- | --- |
| `reactCleaner()` | Dispatch binary, continuous or date cleaning. Binary recoding uses `negval`/`posval`; continuous cleaning replaces values below `floornumber`; date cleaning returns formatted character strings. |
| `binaryCleaner()` | Recode 1 → 1 and 2 → 0; other values become missing. |
| `binaryCleaner_1_0()` | Retain 0/1 values and replace other values with missing. |
| `continuousCleaner()` | Replace values below `floornumber` (default 1) with missing. |
| `dateCleaner()` | Parse dates using `dateFormat`, treating `"#NULL!"`, `"-66"` and `"-99"` as missing. Returns a `Date` vector. |
| `canBeNumeric()` | Test whether numeric conversion introduces additional missing values; return a logical scalar. |
| `checkConvertNumeric()` | Convert every eligible data-frame column to numeric; return a data frame. Numeric-looking categorical codes also qualify. |
| `calculateEpiStats()` | Calculate sensitivity, specificity, positive predictive value and negative predictive value from binary 0/1 prediction and outcome vectors. `returnAllStats = TRUE` adds sample and confusion-matrix counts. |
| `pValEpiConverter()` | Convert p-values to threshold labels with three or four significance-asterisk levels. |
| `pvalAsterisker()` | Return significance asterisks, optionally including rounded p-values and non-significance labels. |
| `specifyDecimal()` | Format numbers using `formatC()` and `k` digits; `format = "f"` fixes decimal places, whereas the default `"fg"` uses significant-digit formatting. `simpleround = TRUE` returns rounded numbers. |
| `comma_sep()` | Format numbers with comma thousands separators and integer precision. |

Clean missing-value codes before calculating diagnostic accuracy: the count
calculations omit missing products, while the optional sample counts are not
restricted to complete prediction–outcome pairs. Formatting functions change
presentation only; asterisks do not implement a multiple-testing correction.
See [cleaning](R/cleaner_functions.R), [epidemiological utilities](R/epi_functions.R)
and [numeric formatting](R/specify_decimal.R).

## Export

| Function | Purpose and output |
| --- | --- |
| `saveREACTtable()` | Write a table as CSV, with optional RDS via `save_rds = TRUE`. |
| `saveREACTplot()` | Save a plot in requested `filetypes` (default PNG), with an additional PDF by default. Dimensions are in inches; raster resolution is 300 dpi. PDF export retries with Cairo if standard export fails. |
| `saveREACT()` | Dispatch plots to figure export, tables/matrices to CSV, and other objects to RDS. |
| `savePrettyExcelWorkbook()` | Write a named list of tables to a styled XLSX workbook, one sheet per table, with category separators and numeric formatting. Existing workbooks are overwritten. |

```r
results_dir <- file.path(tempdir(), "overreact-example")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Table and workbook functions concatenate paths; include the final separator.
outpath <- paste0(results_dir, .Platform$file.sep)
saveREACTtable(table_one, outpath = outpath, filename = "table_one", save_rds = TRUE)
savePrettyExcelWorkbook(
  listOfTables = list(Descriptive = table_one, Prevalence = prevalence_table),
  workbookName = "analysis_tables",
  outpath = outpath
)
saveREACTplot(forest, figpath = results_dir, filename = "forest",
              width = 8, height = 5, savePDF = FALSE)
```

Create output directories before export. Workbook sheet names are truncated to
30 characters; supply distinct names. See [export functions](R/save_functions.R).

## Legacy interfaces

These functions remain exported for compatibility. Their behaviour differs from
the current descriptive and modelling interfaces.

| Function | Behaviour and limitations |
| --- | --- |
| `crossTab()` | Categorical counts, margins, percentages, optional Wilson intervals and a chi-squared p-value. Uses the spelling `rowwise_precentages`. With weights and `confint = FALSE`, percentages are currently calculated from unweighted counts. |
| `crossTabContinuous()` | Group-specific mean (SD) or median (IQR), pooled summaries and optional ANOVA. Weights affect group means and SDs only; pooled summaries and medians remain unweighted. |
| `crossTabMulti()` | Combine legacy categorical and continuous cross-tabs into `Variable`/`Category` rows with a `Pooled` column. `rowvar_list` is deprecated in favour of `rowvars`; continuous weights are not forwarded. |
| `xtabPercentageExtractor()` | Extract text or numeric values between configurable regular-expression delimiters, commonly percentages or interval bounds. |
| `makeXtabPlottable()` | Intended to extract percentages and optionally pivot a cross-tab to long format. The current implementation references the undefined helper `xtab_percentage_extractor` and requires repair before use. |
| `GAMModelMaker()` | Legacy sequential binomial GAM wrapper with optional smooth adjustment terms. The current implementation uses an ambient `dat` object and an output schema incompatible with the current coefficient extractor. |
| `naReporter()` | Intended to audit REACT missing codes (`-91`, `-92`, `-77`, `-99`) and ordinary missingness. The current implementation constructs a report but returns `NULL`. |

See [legacy cross-tabs](R/make_cross_tabs.R) and
[legacy modelling code](R/make_OR_tables.R).

## Reproducibility

Document outcome coding, factor reference levels, adjustment sets, missing-data
strategy and any weighting or assay correction. Review `diagnostics` before
interpreting model output. Standard table tests and binomial intervals do not
account for complex survey designs, and the package does not apply multiplicity
corrections automatically.

For development checks:

```r
devtools::test()
devtools::check()
sessionInfo()
```
