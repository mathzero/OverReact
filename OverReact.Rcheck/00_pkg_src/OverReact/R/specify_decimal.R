#' Format numeric values with fixed decimals
#'
#' @param x Numeric vector to format.
#' @param k Number of decimal places to display.
#' @param format Format string passed to \code{formatC()}.
#' @param simpleround Logical; if \code{TRUE}, return rounded numeric values
#'   instead of formatted strings.
#'
#' @return A character vector of formatted values when
#'   \code{simpleround = FALSE}, or a rounded numeric vector when
#'   \code{simpleround = TRUE}.
specifyDecimal <- function(x,k, format = "fg", simpleround =F){
  if (simpleround) {
    return(round(x, k))
  }
  formatC(x, digits = k, format = format)
}
