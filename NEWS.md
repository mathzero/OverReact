# OverReact (development version)

## Breaking changes

* Data arguments no longer default to global `dfRes` or `covs` objects.
* `ModelMakerMulti()` and `modelMakerSequential()` number the crude model 0,
  as `ModelMakerMultiRD()` does.
* Ordered factors are modelled with treatment contrasts (one estimate per
  level) instead of polynomial contrasts.
* Outcomes coded other than 0/1, logical or two-level factor, and
  categorical outcomes with three or more levels, raise an error up front.
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

## Packaging

* MIT licence, maintainer details and an explicit export list.
* `plotReactForest()` no longer needs the undeclared ggnewscale package.
* prevalence (which needs a system JAGS install), questionr and plyr are no
  longer dependencies; base-R replacements give identical results.
* GitHub Actions run `R CMD check` on push and pull request.
