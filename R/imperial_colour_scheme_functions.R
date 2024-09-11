#' Return function to interpolate a new color palette
#'
#' @param palette Character name of palette in new_palettes. Choose from:
#' \itemize{
#'  \item default, earth, cool, warm, soft, bold, nature,
#'  \item two_col_grey_teal, two_col_pink_purple, two_col_blue_green
#' }
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @import ggplot2
#' @import dichromat
#' @import tinter

new_pal <- function(palette = "default", reverse = FALSE, ...) {
  pal <- new_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for new colors
#'
#' @param palette Character name of palette in new_palettes. Choose from:
#' \itemize{
#'  \item default, earth, cool, warm, soft, bold, nature,
#'  \item two_col_grey_teal, two_col_pink_purple, two_col_blue_green
#' }
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to ggplot2::discrete_scale() or
#'            ggplot2::scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_new <- function(palette = "default", discrete = TRUE, reverse = FALSE, ...) {
  pal <- new_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("new_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for new colors
#'
#' @param palette Character name of palette in new_palettes. Choose from:
#' \itemize{
#'  \item default, earth, cool, warm, soft, bold, nature,
#'  \item two_col_grey_teal, two_col_pink_purple, two_col_blue_green
#' }
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to ggplot2::discrete_scale() or
#'            ggplot2::scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_new <- function(palette = "default", discrete = TRUE, reverse = FALSE, ...) {
  pal <- new_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("new_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
