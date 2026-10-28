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

Specify analysis data, outcomes and adjustment variables explicitly. Data
arguments have no defaults; some outcome and adjuster defaults still use
REACT-specific variable names.

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
| `tableOneDisplay()` | Render a `tableOne()` result as a `flextable` or `gt` table, with variable rows in bold, indented levels, formatted p-values and SMDs, and a footnote naming each variable's test. |
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

With `statistical_test = TRUE`, tests follow the data by default:
`cat_test = "auto"` uses Fisher's exact test when any expected count is below 5
and Pearson's chi-squared test (with Yates' correction for 2 × 2 tables)
otherwise, and `cont_test = "auto"` uses one-way ANOVA for means and the
Kruskal–Wallis test for medians. `"chisq"`, `"fisher"`, `"anova"` and `"kruskal"`
force a test; weighted tables always use the chi-squared test. The result's
`"tests"` attribute names the test behind each p-value. Tests use complete cases
for the variable and group, excluding empty levels and displayed missing
categories. They are omnibus comparisons without multiplicity adjustment. No test
is attached to the observation-count row, and unidentifiable comparisons return
missing p-values.

`smd = TRUE` adds an `SMD` column of absolute standardised mean differences
between the observed groups: the pooled-SD difference for continuous variables
and the Yang–Dalton multivariate form for categorical ones (for two categories,
`|p1 - p2| / sqrt((p1(1 - p1) + p2(1 - p2)) / 2)`). With more than two groups the
mean of the pairwise SMDs is shown, as in the tableone package. `tableOneDisplay()`
turns the table into a `flextable` (for Word) or `gt` (for HTML) table.

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
| `ModelMakerMultiRD()` | Fit several predictors using cumulative or named adjustment sets and any supported `family`, with optional marginal risk differences and risk ratios, parallel execution, retries and disk checkpoints. Returns separate coefficient, risk-difference and risk-ratio outputs plus diagnostics. |
| `modelMakerSequential()` | Fit one predictor with sequential adjustment. Returns `model_df`, `model_df_predictorORs_only`, `crude_model_output`, `adj_model_outputs` and diagnostics. |
| `modelMakerSequentialRD()` | Fit one predictor with cumulative adjustment and optional risk differences and ratios. Returns separate coefficient, RD and RR tables, crude and adjusted results, diagnostics and sample counts. |
| `modelMakerAdjustedSetsRD()` | Fit one predictor using named `adj_sets`. Returns `model_df_ORs`, `model_df_RDs`, `model_df_RRs`, the matching `crude_model_*` and `adj_*_outputs` tables, `diagnostics`, the `family` and `measure`, and sample counts. |
| `makeORTable()` | Extract coefficients, 95% Wald intervals and p-values from a fitted `glm`, `lm`, `gam`, `polr` or `multinom` model. Logit-link coefficients are exponentiated to odds ratios and log-link coefficients to risk or rate ratios; other links keep their original scale. The `measure` column says which (`OR`, `RR`, `IRR`, `Beta` or `Coef`). Optional reference rows and term metadata are included; `dp = NULL` returns unrounded estimates. |
| `makeRDTable()` | Estimate marginal risk differences for a factor or numeric predictor in a binomial model, as described below. Returns `Level`, `RD`, `Lower`, `Upper` and `P_value`; intervals use the delta method, or coefficient simulation with `ci_method = "simulation"`. |
| `makeRRTable()` | Estimate marginal risk ratios in the same way, returning `Level`, `RR`, `Lower`, `Upper` and `P_value`. |
| `safe_makeRDTable()` | Wrap `makeRDTable()` with error capture and an explicit reference row for factor predictors. Returns `NULL` on a caught error. |

### Model families

The `family` argument of the modelling wrappers selects the model; each family
reports one effect measure, and estimate columns are named after it.

| `family` | Model | Measure |
| --- | --- | --- |
| `"binomial"` | Logistic regression | `OR` |
| `"gaussian"` | Linear regression | `Beta` |
| `"poisson"`, `"quasipoisson"` | Log-linear count model; `offset` names a column of exposure times, entered as `offset(log(...))` | `IRR` |
| `"log_binomial"` | Binomial regression with a log link, started inside the parameter space | `RR` |
| `"modified_poisson"` | Poisson regression of a binary outcome with HC0 sandwich standard errors | `RR` |
| `"ordinal"` | Proportional-odds model (`MASS::polr()`) | `OR` |
| `"multinomial"` | Multinomial logit (`nnet::multinom()`); one block of rows per non-reference outcome level | `OR` |

`family = "auto"` (the default) chooses binomial for two distinct values,
ordinal for an ordered factor, multinomial for another categorical outcome with
three or more levels, and Gaussian otherwise. Encode binary outcomes as 0/1,
logical values or a two-level factor; other two-value codings (such as 1/2), and
outcomes that do not suit an explicitly chosen family, raise an error before any
model is fitted. Character predictors and adjusters are converted to
factors, and ordered factors are treated as unordered, so every level is compared
with the reference. Unused levels are dropped within the analysis sample. The crude
model is stage 0 and each adjusted model adds one to the stage number. Reference
rows show `-` in the formatted tables, and every stage keeps its column even when
its model cannot be fitted.

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

# Risk ratios from a modified Poisson model, and marginal risk ratios.
rr_models <- ModelMakerMultiRD(
  dat = clinical,
  list_of_variables_of_interest = c("vaccinated", "sex"),
  outcome = "positive",
  joint_adjustment_vars = "age",
  family = "modified_poisson",
  include_rr = TRUE,
  ncores = 1
)
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
| `df_output` | Formatted estimates across model stages; columns end in the family's measure (for example `crude_mod_OR`, `adj_age_IRR`). Multinomial models add an `Outcome` column. |
| `plot_output` | Numeric estimates (in a column named after the measure), `Lower`, `Upper`, `P_value`, `Q_value`, reference indicators, `measure`, `model_family`, stage labels and `Nobs_in_model`; multinomial models add `outcome_level`. |
| `df_output_RDs` | Formatted risk-difference tables; `NULL` when unavailable. |
| `plot_output_RDs` | Numeric risk differences and intervals; `NULL` when unavailable. |
| `df_output_RRs`, `plot_output_RRs` | Formatted and numeric marginal risk ratios with `include_rr = TRUE`; `NULL` when unavailable. |
| `diagnostics` | Stage, adjustment variables, status, errors, captured warnings, sample size, covariance validity, separation flags and attempt number. |

`Q_value` holds multiple-testing adjusted p-values: Benjamini–Hochberg by
default, or any `stats::p.adjust()` method through `p_adjust_method`, applied
within each adjustment stage across every predictor and level (reference rows are
not tests). `p_adjust_method = "none"` omits the column. The same adjustment is
applied to the risk-difference and risk-ratio tables, and `ModelMakerMulti()`
supports it too.

Lower-level tables (from `makeORTable()` and the single-predictor wrappers) keep
the estimate column name `OR` for compatibility; their `measure` column gives the
actual measure. Coefficient intervals use normal quantiles, or t
quantiles with the residual degrees of freedom when the model reports t tests (as
Gaussian models do), so intervals agree with the p-values from the fitted model
summary. Numeric outputs are unrounded; only the formatted tables are rounded.

Marginal risk differences (`include_rd`) and risk ratios (`include_rr`) are
estimated by standardisation over the analysis sample. For a factor predictor,
predicted risks are averaged with every observation set to each level and
contrasted with the reference level, as a difference or a ratio. For a numeric
predictor, the risk difference is the average marginal effect (the mean
derivative of predicted risk per one-unit increase), and the risk ratio is the
proportional change in mean risk per unit, `exp(d log(mean risk) / dx)`.
Intervals use the delta method by default, on the log scale for ratios;
`rd_ci_method = "simulation"` instead takes the 2.5th and 97.5th percentiles of
`n_sim` multivariate-normal coefficient draws, using a fixed seed without
disturbing the session's random-number stream. P-values use a normal
approximation. These are model-based associations; causal interpretation
requires additional assumptions. Marginal effects need a binary outcome (the
binomial, log-binomial and modified Poisson families) and are skipped otherwise;
`include_rd = FALSE` disables risk differences.

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
| `get_pretty_name()` | Resolve a display label from `name_map`, `options(modelmaker.name_map = ...)`, a variable's `label` attribute, or automatic title casing; an optional `name_fun` transforms the fallback label. |

`cov_name_list`, a named vector or list, sets display labels in the batch
wrappers. Variables whose labels coincide (for example `age_grp` and `age grp`)
get their raw names appended, so their results are never merged.
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
`family`, which is passed to `ModelMakerMultiRD()`. `include_rd` defaults to
`FALSE`; supplying `NULL` enables automatic selection for binary outcomes, and
`include_rr = TRUE` adds marginal risk ratios. Each report line also shows a
q-value, adjusted over all the tests that section ranks (every level of each
predictor's most-adjusted model); `p_adjust_method = NULL` omits them. The report ranks associations by p-value,
selecting one category per predictor from that predictor's most-adjusted model;
a predictor that is also a covariate is not adjusted for itself. Treat this
ranking as exploratory, particularly when adjustment sets are not nested.
See [the driver](R/automated_analysis.R).

## Figures, themes and colours

| Function | Purpose and output |
| --- | --- |
| `plotReactForest()` | Create a forest plot from batch-model results, numeric plotting data, an `lm`/`glm`/`gam` fit, or a named list of fits. Supports `effect = "OR"`, `"RR"`, `"IRR"`, `"Beta"` or `"RD"`, adjustment labels, reference rows, variable selection and an optional column of estimates, intervals, p-values and sample sizes. |
| `plotReactVIF()` | Plot externally supplied variance inflation factors by adjustment stage. Requires `Variable`, `adjustment` and `vif`; batch model wrappers do not currently return VIFs. |
| `theme_react()` | Apply the REACT theme with white backgrounds, small text and configurable base size, facet and title styling; further theme elements can be passed through `...`. |
| `theme_mw()` | Apply a related theme with panel borders, ticks and configurable grids and legend placement; a numeric `legend.position` places the legend inside the panel. |
| `imperial_cols()` | Retrieve named colours; with no arguments, return the complete named vector. |
| `imperial_pal_discrete()` | Return a function generating discrete colours, retaining the specified palette and optionally extending it with distinct HCL colours. |
| `imperial_pal_continuous()` | Return a palette interpolation function using CIELAB colour space. |
| `scale_color_new()`, `scale_fill_new()` | Apply package palettes to `ggplot2` colour or fill aesthetics; select discrete or continuous scales with `discrete`. |
| `scale_color_imperial()`, `scale_fill_imperial()` | Aliases of the corresponding `*_new()` scales. |
| `normalise_hex()` | Trim whitespace and convert colour strings to uppercase. |
| `hex_to_lab()` | Convert R colour specifications to a matrix of CIELAB coordinates. |
| `pick_fallback_colours()` | Select additional qualitative HCL colours, favouring separation from existing colours in CIELAB space. |

```r
forest <- plotReactForest(models, effect = "OR", title = "Adjusted associations",
                          show_estimates = TRUE)
forest

# Risk differences from the same fitted models.
plotReactForest(models, effect = "RD", title = "Marginal risk differences")

ggplot2::ggplot(clinical, ggplot2::aes(x = age, fill = sex)) +
  ggplot2::geom_histogram(binwidth = 10, position = "identity", alpha = 0.5) +
  scale_fill_imperial(palette = "two_col_blue_green") +
  theme_react()
```

Forest plots use a logarithmic scale with null value 1 for ratios (OR, RR,
IRR), and linear coefficient or risk-difference scales with null value 0. For a
batch result, `effect = "RR"` plots the marginal risk ratios when they were
requested, and multinomial rows are labelled with their outcome level.
`show_estimates = TRUE` adds a column to the right of each panel giving every
row's estimate (95% CI), p-value and N, from the last plotted model or the stage
named by `estimate_adjustment`; `estimate_digits` sets the decimal places. `xrange`
sets the visible ratio range (default 1/4 to 16); estimates or interval bounds beyond
it are clipped at the panel edge, with a warning. Pass numeric
`plot_output` data rather than formatted table strings.

`imperial_colours` stores named hexadecimal colours; `imperial_palettes` stores
palette definitions. Available palettes are `default`, `gradient`, `earth`,
`cool`, `warm`, `soft`, `bold`, `nature`, `two_col_grey_teal`,
`two_col_pink_purple` and `two_col_blue_green`. Palettes leave out the two colours
that are nearly invisible on the white theme (`White_Smoke` and `Yellow`), which
remain available through `imperial_cols()`. Discrete palettes extend by
default; `fallback = FALSE` enforces the original palette length.
See [plots](R/plots.R), [themes](R/ggplot_themes.R) and
[palette functions](R/imperial_colour_scheme_functions.R).

## Data preparation and reporting utilities

| Function | Purpose and output |
| --- | --- |
| `reactCleaner()` | Dispatch binary, continuous or date cleaning; other `datatype` values raise an error. Binary recoding uses `negval`/`posval`; continuous cleaning replaces values below `floornumber`; date cleaning returns formatted character strings. |
| `binaryCleaner()` | Recode 1 → 1 and 2 → 0; other values become missing. |
| `binaryCleaner_1_0()` | Retain 0/1 values and replace other values with missing. |
| `continuousCleaner()` | Replace values below `floornumber` (default 1) with missing. |
| `dateCleaner()` | Parse dates using `dateFormat`, treating `"#NULL!"`, `"-66"` and `"-99"` as missing. Returns a `Date` vector. |
| `canBeNumeric()` | Test whether numeric conversion introduces additional missing values; return a logical scalar. |
| `checkConvertNumeric()` | Convert every eligible column to numeric, keeping column names. A column converts only if no new missing values appear and distinct values stay distinct, so codes such as `"01"` and `"1"` are left unchanged. Numeric-looking factor codes otherwise qualify. |
| `calculateEpiStats()` | Calculate sensitivity, specificity, positive predictive value and negative predictive value from binary 0/1 prediction and outcome vectors. `returnAllStats = TRUE` adds sample and confusion-matrix counts. |
| `pValEpiConverter()` | Convert p-values to threshold labels with three or four significance-asterisk levels; missing p-values stay missing. |
| `pvalAsterisker()` | Return significance asterisks, optionally including rounded p-values (in fixed notation, such as `<0.0001`) and non-significance labels; missing p-values stay missing. |
| `specifyDecimal()` | Format numbers using `formatC()` and `k` digits; `format = "f"` fixes decimal places, whereas the default `"fg"` uses significant-digit formatting. `simpleround = TRUE` returns rounded numbers. |
| `comma_sep()` | Format numbers with comma thousands separators and integer precision. |

Clean missing-value codes before calculating diagnostic accuracy;
`calculateEpiStats()` uses complete prediction–outcome pairs for every count.
Formatting functions change
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

# Paths are concatenated, so outpath may end in a filename prefix; an existing
# directory also works without the final separator.
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

Create output directories before export. Matrices and two-way tables are
written with their layout and row labels. Workbook sheet names are truncated to 30
characters, characters Excel forbids are replaced, and duplicates are numbered. See [export functions](R/save_functions.R).

## Legacy interfaces

These functions remain exported for compatibility. Their behaviour differs from
the current descriptive and modelling interfaces.

| Function | Behaviour and limitations |
| --- | --- |
| `crossTab()` | Categorical counts, margins, percentages, optional Wilson intervals and a chi-squared p-value. Uses the spelling `rowwise_precentages`. With weights and `confint = FALSE`, percentages are currently calculated from unweighted counts. |
| `crossTabContinuous()` | Group-specific mean (SD) or median (IQR), pooled summaries and optional ANOVA. Weights affect group means and SDs only; pooled summaries and medians remain unweighted. |
| `crossTabMulti()` | Combine legacy categorical and continuous cross-tabs into `Variable`/`Category` rows with a `Pooled` column. `rowvar_list` is deprecated in favour of `rowvars`; continuous weights are not forwarded. |
| `xtabPercentageExtractor()` | Extract text or numeric values between configurable regular-expression delimiters, commonly percentages or interval bounds. |
| `makeXtabPlottable()` | Extract the percentages from a legacy cross-tab and optionally pivot it to long `Variable`, `Category`, `name`, `value` format for plotting. |
| `GAMModelMaker()` | Legacy sequential binomial GAM wrapper; `spline_vars` indexes the adjusters entered as smooth terms. Returns formatted `model_df` columns per stage and the coefficient tables. |
| `naReporter()` | Count REACT missing codes (`-91`, `-92`, `-77`, `-99`) and ordinary missing values; returns one row per variable. |

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
