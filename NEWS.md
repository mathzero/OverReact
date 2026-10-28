# OverReact (development version)

## Breaking changes

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
* Weighted `crossTabContinuous()` no longer errors when the grouping variable
  has missing values.

## Packaging

* MIT licence, maintainer details and an explicit export list.
* `plotReactForest()` no longer needs the undeclared ggnewscale package.
* prevalence (which needs a system JAGS install), questionr and plyr are no
  longer dependencies; base-R replacements give identical results.
* GitHub Actions run `R CMD check` on push and pull request.
