\name{tableOne}
\alias{tableOne}
\alias{tableCat}
\alias{tableCont}
\alias{tableOneDisplay}
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
  rowwise_percentages = rowwise_precentages,
  cat_test = c("auto", "chisq", "fisher"),
  cont_test = c("auto", "anova", "kruskal"), smd = FALSE)

tableCat(dat, rowvar, colvar, confint = TRUE,
  include_percentages = TRUE, rowwise_precentages = TRUE,
  weights = NULL, comma_thousands = FALSE, statistical_test = FALSE,
  includeNAsColvar = TRUE, includeNAsRowvar = TRUE,
  rowwise_percentages = rowwise_precentages,
  cat_test = c("auto", "chisq", "fisher"), smd = FALSE)

tableCont(dat, rowvar, colvar = NULL, weights = NULL,
  summary_stat = "mean", statistical_test = FALSE,
  includeNAsColvar = TRUE, includeNAsRowvar = TRUE,
  cont_test = c("auto", "anova", "kruskal"), smd = FALSE)

tableOneDisplay(tab, output = c("flextable", "gt"), title = NULL,
  p_digits = 3, smd_digits = 2)
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
    Missing weights are excluded. Weighted categorical cell counts are rounded to
    integers for display and percentages. Confidence intervals and chi-squared tests
    use the Kish effective sample size, \code{sum(w)^2 / sum(w^2)}, of each
    denominator, so they do not depend on the scale of the weights.
    Continuous summaries with weights raise an error; older versions silently ignored them.}
  \item{summary_stat}{\code{"mean"} for mean (SD), or \code{"median"} for median (IQR).}
  \item{comma_thousands}{Use comma separators in categorical counts.}
  \item{statistical_test}{Add a p-value per variable, from the tests chosen by
    \code{cat_test} and \code{cont_test}.
    Tests use complete cases of the row and grouping variables, excluding empty levels.
    With weights, tests account for unequal weighting through the effective sample
    size, but not for clustering or stratification.}
  \item{includeNAsColvar}{Display missing grouping values as a separate column.}
  \item{includeNAsRowvar}{Display missing categorical row values as a separate level.
    Continuous summaries always omit missing measurements.}
  \item{formatPvalsForEpiPaper}{Format available p-values with rounding and significance asterisks.}
  \item{cat_test}{Test for categorical variables: \code{"auto"} uses Fisher's exact test
    when any expected count is below 5 and Pearson's chi-squared test otherwise;
    \code{"chisq"} or \code{"fisher"} force one. Weighted tables always use the
    chi-squared test.}
  \item{cont_test}{Test for continuous variables: \code{"auto"} uses one-way ANOVA for
    means and the Kruskal-Wallis test for medians; \code{"anova"} or \code{"kruskal"}
    force one.}
  \item{smd}{Add an \code{SMD} column of absolute standardised mean differences between
    the observed groups: pooled-SD differences for continuous variables and the
    Yang-Dalton multivariate form for categorical ones, averaged over pairs of groups
    when there are more than two.}
  \item{tab}{A \code{tableOne} result.}
  \item{output}{Table class to build: \code{"flextable"} or \code{"gt"} (each needs its package).}
  \item{title}{Optional table title.}
  \item{p_digits, smd_digits}{Decimal places for p-values and SMDs.}
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
A data frame. With \code{statistical_test = TRUE}, the \code{"tests"} attribute
of the \code{tableOne} result names the test used for each variable.
\code{tableOneDisplay} returns a \code{flextable} or \code{gt_tbl} with variable
rows in bold, level rows indented and a footnote naming the tests. \code{tableOne} includes \code{Variable}, \code{Level},
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
