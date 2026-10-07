\name{tableOne}
\alias{tableOne}
\alias{tableCat}
\alias{tableCont}
\title{Descriptive Tables by Group}
\description{
Summarise continuous and categorical variables in a display-ready data frame,
optionally stratified by a column variable.
}
\usage{
tableOne(dat, rowvars, colvar = NULL, cov_names = NULL,
  confint = FALSE, include_percentages = TRUE,
  rowwise_precentages = TRUE, weights = NULL, summary_stat = "mean",
  comma_thousands = FALSE, statistical_test = FALSE,
  includeNAsColvar = TRUE, includeNAsRowvar = TRUE,
  formatPvalsForEpiPaper = FALSE, addNobsTopRow = TRUE,
  rowwise_percentages = rowwise_precentages)

tableCat(dat, rowvar, colvar, confint = TRUE,
  include_percentages = TRUE, rowwise_precentages = TRUE,
  weights = NULL, comma_thousands = FALSE, statistical_test = FALSE,
  includeNAsColvar = TRUE, includeNAsRowvar = TRUE,
  rowwise_percentages = rowwise_precentages)

tableCont(dat, rowvar, colvar = NULL, weights = NULL,
  summary_stat = "mean", statistical_test = FALSE,
  includeNAsColvar = TRUE, includeNAsRowvar = TRUE)
}
\arguments{
  \item{dat}{A data frame. \code{tableOne} requires at least one row and unique column names.}
  \item{rowvars, rowvar}{Names of variables to summarise. Missing variables are reported and skipped.}
  \item{colvar}{Name of the grouping variable, or \code{NULL} for an unstratified table.}
  \item{cov_names}{Optional named vector or list mapping variable names to display labels.}
  \item{confint}{Include 95\% Wilson confidence intervals for categorical percentages.}
  \item{include_percentages}{Include percentages with categorical counts.}
  \item{rowwise_precentages}{Legacy spelling of \code{rowwise_percentages}, retained for compatibility.}
  \item{rowwise_percentages}{If \code{TRUE}, divide counts by the row total across displayed groups.
    If \code{FALSE}, divide by the column total across displayed row levels.
    This spelling takes precedence if both spellings are supplied.}
  \item{weights}{Optional column of non-negative finite numeric weights.
    Missing weights are excluded. Categorical cell counts are rounded to integers
    before calculating percentages and confidence intervals.
    Continuous summaries with weights raise an error; older versions silently ignored them.}
  \item{summary_stat}{\code{"mean"} for mean (SD), or \code{"median"} for median (IQR).}
  \item{comma_thousands}{Use comma separators in categorical counts.}
  \item{statistical_test}{Add Pearson chi-squared tests for categorical variables and
    one-way ANOVA for continuous variables (also when displaying medians).
    Tests use complete cases of the row and grouping variables, excluding empty levels.
    These tests do not account for a survey sampling design.}
  \item{includeNAsColvar}{Display missing grouping values as a separate column.}
  \item{includeNAsRowvar}{Display missing categorical row values as a separate level.
    Continuous summaries always omit missing measurements.}
  \item{formatPvalsForEpiPaper}{Format available p-values with rounding and significance asterisks.}
  \item{addNobsTopRow}{Prepend one observation-count row. Its group percentages are always
    proportions of the total across displayed groups, and it has no hypothesis test.}
}
\details{
\code{tableOne} treats numeric variables containing only observed zeros and ones
as categorical; other numeric variables, including entirely missing numeric
variables, are continuous. Factors, character vectors, and logical vectors are
categorical. Use factors to specify categorical variables and their level order.

Group labels retain spaces and punctuation. Unused factor levels remain visible.
The missing group is labelled \code{"NA"}. Actual duplicate labels and labels that
collide with output columns such as \code{"Overall"} receive unique numeric suffixes.
For example, a literal \code{"NA"} group and a missing group are displayed as
\code{"NA"} and \code{"NA.1"}, respectively; they are never merged.

\code{Overall} and \code{Missing} describe the supplied population even when
missing grouping values are hidden. Missing categorical row values contribute
only when \code{includeNAsRowvar = TRUE}. Row-wise overall cells display counts;
column-wise overall cells also display percentages of the overall total.
A categorical percentage with a zero denominator is displayed as \code{NA\%}.
A continuous group with no observed measurements returns \code{NA}; a mean based
on one measurement has \code{NA} for its SD. Tests that cannot compare at least
two observed groups return a missing p-value.
}
\value{
A data frame. \code{tableOne} includes \code{Variable}, \code{Level},
\code{Missing}, \code{Overall}, group columns, and optionally \code{P-value}.
Categorical blocks contain a header row followed by level rows; continuous
blocks contain one row. \code{tableCat} and \code{tableCont} return individual
blocks with a \code{units} column instead of \code{Variable}, or \code{NULL}
when the requested variable cannot be tabulated.
}
\examples{
dat <- data.frame(
  group = factor(c("Asymptomatic", "Long COVID", NA, "Long COVID")),
  height = c(170, 165, 172, 168),
  vaccinated = factor(c("Yes", "Yes", "No", "No"))
)
tableOne(dat, c("height", "vaccinated"), "group",
         rowwise_percentages = TRUE, includeNAsColvar = TRUE)
}
