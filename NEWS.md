# OverReact (development version)

## New features

* The modelling wrappers take a `family` argument: binomial, Gaussian,
  Poisson and quasi-Poisson (incidence rate ratios, with an optional `offset`
  of exposure times), log-binomial and modified Poisson (risk ratios, the latter
  with sandwich standard errors), ordinal (proportional odds) and multinomial.
  `family = "auto"` picks ordinal or multinomial models for categorical outcomes
  with three or more levels.
* `include_rr = TRUE` adds marginal (standardised) risk ratios alongside risk
  differences, with `makeRRTable()` for single models.
* Estimate columns are named after the family's measure (`OR`, `RR`, `IRR` or
  `Beta`), and coefficient tables gain a `measure` column; probit and other
  non-log links are no longer exponentiated.
* `plotReactForest()` plots RR and IRR estimates on a ratio scale and labels
  multinomial rows with their outcome level.
* Batch plotting outputs gain a `Q_value` column: Benjamini-Hochberg (or any
  `p_adjust_method`) adjusted p-values within each adjustment stage, across all
  predictors and levels. The automated report shows q-values for the tests it
  ranks.

## Breaking changes

* Data arguments no longer default to global `dfRes` or `covs` objects.
* `ModelMakerMulti()` and `modelMakerSequential()` number the crude model 0,
  as `ModelMakerMultiRD()` does.
* Ordered factors are modelled with treatment contrasts (one estimate per
  level) instead of polynomial contrasts.
* Outcomes coded other than 0/1, logical or two-level factor raise an error up
  front, and categorical outcomes with three or more levels get ordinal or
  multinomial models instead of failing Gaussian fits.
* Gaussian batch outputs name every stage column with `_Beta` (only the crude
  column was renamed before).
* `makeTablesNew()` and `stratifiedTables()` default to `sens = 1` and
  `spec = 1` (no assay correction). Pass `sens = 0.844, spec = 0.986` to
  reproduce earlier REACT defaults.
* Risk-difference intervals use the delta method by default
  (`rd_ci_method = "simulation"` restores simulated intervals), and numeric
  outputs are no longer rounded.
* Checkpoints written by earlier versions are recomputed once.

## Bug fixes

* Numeric-predictor risk differences are now average marginal effects.
* `run_auto_tableone_regressions()` reports every predictor, including those
  that are also covariates.
* `plotReactForest()` clips out-of-range estimates instead of dropping them.
* Weighted `tableCat()` intervals and tests use the Kish effective sample size.
* Checkpoints are reused only for an identical specification and data.
* Reference levels are fixed per predictor across adjustment stages.
* `cov_name_list` now labels variables in the batch wrappers, and variables
  with colliding labels are kept apart.
* Gaussian coefficient intervals use t quantiles; Gaussian GAM tables are no
  longer empty.
* Formatted reference rows show `-`, and stages that cannot be fitted keep
  their column.
* `GAMModelMaker()` works again and estimates against the labelled reference.
* Checkpoint files are written atomically without using the session RNG.
* Weighted `crossTabContinuous()` no longer errors when the grouping variable
  has missing values.
* `pvalAsterisker()` and `pValEpiConverter()` keep missing p-values missing
  (they were labelled "ns") and print small thresholds as `<0.0001`.
* `theme_react()` and `theme_mw()` honour `base_size`, `strip_text_margin` and
  `...`, and no longer warn about strip margins.
* `saveREACT()` and `saveREACTtable()` write matrices and tables;
  `savePrettyExcelWorkbook()` checks its input, accepts a single data frame and
  makes sheet names valid and unique; a directory `outpath` no longer needs a
  trailing separator.
* `dateCleaner()` and `reactCleaner()` handle factor and Date input, and an
  invalid `datatype` is an error.
* `checkConvertNumeric()` keeps column names and leaves codes such as "01"/"1"
  unconverted.
* `orderMyTable()` orders weight and COVID categories correctly;
  `validate_factor_levels()` reports empty levels.
* `makeXtabPlottable()` and `naReporter()` work; `calculateEpiStats()` uses
  complete pairs; report RD intervals use the requested precision.

## Packaging

* MIT licence, maintainer details and an explicit export list.
* `plotReactForest()` no longer needs the undeclared ggnewscale package.
* prevalence (which needs a system JAGS install), questionr and plyr are no
  longer dependencies; base-R replacements give identical results.
* GitHub Actions run `R CMD check` on push and pull request.
