\name{modelOutputDisplay}
\alias{modelOutputDisplay}
\title{Display Tables for Regression Model Outputs}
\description{
Render regression outputs from \code{ModelMakerMultiRD()}, \code{ModelMakerMulti()},
\code{modelMakerSequential*()}, or \code{run_auto_tableone_regressions()} as a
publication-ready \code{flextable} or \code{gt} table with bold predictor headers,
indented category rows, sample sizes, effect estimates, p-values, q-values, and
marginal risk differences or risk ratios.
}
\usage{
modelOutputDisplay(
  model_out,
  output = c("flextable", "gt", "data.frame"),
  title = NULL,
  stages = c("all", "crude_and_full", "full"),
  include_p = TRUE,
  include_q = TRUE,
  include_rd = TRUE,
  include_rr = TRUE,
  include_n = TRUE,
  include_events = TRUE,
  stage_labels = NULL,
  p_digits = 3
)
}
\arguments{
  \item{model_out}{A result object returned by \code{ModelMakerMultiRD()},
    \code{ModelMakerMulti()}, \code{modelMakerSequentialRD()},
    \code{modelMakerAdjustedSetsRD()}, \code{modelMakerSequential()},
    \code{run_auto_tableone_regressions()}, or a wide \code{df_output} data frame.}
  \item{output}{Table class to build: \code{"flextable"}, \code{"gt"} (each needs its package),
    or \code{"data.frame"} for the formatted display data frame.}
  \item{title}{Optional table title or caption.}
  \item{stages}{Which adjustment stages to include: \code{"all"} (every stage),
    \code{"crude_and_full"} (first and last stage), or \code{"full"} (final stage only).}
  \item{include_p}{Logical; include formatted p-value columns when available.}
  \item{include_q}{Logical; include formatted multiple-testing q-value columns when available.}
  \item{include_rd}{Logical; include marginal risk difference (\code{RD}) columns when present.}
  \item{include_rr}{Logical; include marginal risk ratio (\code{RR}) columns when present.}
  \item{include_n}{Logical; include a column showing the model sample size \code{N}.}
  \item{include_events}{Logical; for Cox proportional-hazards models, include an \code{Events} column when available.}
  \item{stage_labels}{Optional character vector (named by stage column or stem, or unnamed matching the selected stages) overriding stage display labels.}
  \item{p_digits}{Number of decimal places for formatted p-values and q-values.}
}
\value{
A \code{flextable} or \code{gt_tbl} object with predictor rows in bold, category
rows indented, and a source note describing the model family, effect measure,
marginal contrasts, and multiple-testing adjustment.
}
\examples{
dat <- data.frame(
  y = c(0, 1, 0, 1, 1, 0, 1, 0),
  age = c(32, 54, 41, 63, 70, 29, 58, 46),
  sex = factor(c("F", "M", "F", "M", "F", "F", "M", "M"))
)
mods <- ModelMakerMultiRD(dat, c("age", "sex"), outcome = "y",
                          joint_adjustment_vars = "sex", include_rd = FALSE)
if (requireNamespace("flextable", quietly = TRUE)) {
  modelOutputDisplay(mods, output = "flextable", title = "Regression results")
}
}
