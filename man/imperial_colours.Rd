\name{imperial_colours}
\alias{imperial_colours}
\alias{imperial_palettes}
\alias{imperial_cols}
\alias{imperial_pal_discrete}
\alias{imperial_pal_continuous}
\alias{scale_color_new}
\alias{scale_fill_new}
\alias{scale_color_imperial}
\alias{scale_fill_imperial}
\alias{normalise_hex}
\alias{hex_to_lab}
\alias{pick_fallback_colours}
\title{Package Colours, Palettes and ggplot2 Scales}
\description{
Named colours, palette definitions, palette generators and \pkg{ggplot2}
colour and fill scales built from them.
}
\usage{
imperial_colours
imperial_palettes
imperial_cols(...)

imperial_pal_discrete(palette = "default", reverse = FALSE, fallback = TRUE,
  fallback_palette = "Dynamic")
imperial_pal_continuous(palette = "default", reverse = FALSE, ...)

scale_color_new(palette = "default", discrete = TRUE, reverse = FALSE,
  fallback = TRUE, fallback_palette = "Dynamic", ...)
scale_fill_new(palette = "default", discrete = TRUE, reverse = FALSE,
  fallback = TRUE, fallback_palette = "Dynamic", ...)
scale_color_imperial(palette = "default", discrete = TRUE, reverse = FALSE,
  fallback = TRUE, fallback_palette = "Dynamic", ...)
scale_fill_imperial(palette = "default", discrete = TRUE, reverse = FALSE,
  fallback = TRUE, fallback_palette = "Dynamic", ...)

normalise_hex(x)
hex_to_lab(x)
pick_fallback_colours(n, existing = character(), fallback_palette = "Dynamic",
  candidate_pool = 64)
}
\arguments{
  \item{...}{For \code{imperial_cols}, names of colours to return. For
    \code{imperial_pal_continuous}, arguments passed to
    \code{\link[grDevices]{colorRampPalette}}. For the scales, arguments passed to
    \code{\link[ggplot2]{discrete_scale}} or the corresponding
    \code{scale_*_gradientn} function.}
  \item{palette}{Name of a palette in \code{imperial_palettes}.}
  \item{reverse}{Reverse the palette order.}
  \item{fallback}{When more colours are requested than the palette holds, extend
    it with distinct colours (\code{TRUE}) or raise an error (\code{FALSE}).}
  \item{fallback_palette}{Qualitative \code{\link[grDevices]{hcl.colors}} palette
    from which extra colours are drawn.}
  \item{discrete}{Build a discrete scale (\code{TRUE}) or a continuous gradient.}
  \item{x}{Character vector of colour specifications.}
  \item{n}{Number of colours to select.}
  \item{existing}{Colours the new colours should be distinct from.}
  \item{candidate_pool}{Minimum number of candidate colours considered.}
}
\details{
\code{scale_color_imperial} and \code{scale_fill_imperial} are aliases of
\code{scale_color_new} and \code{scale_fill_new}. Extra discrete colours are
chosen greedily to maximise their minimum CIELAB distance from the colours
already in use.
}
\value{
\code{imperial_colours} is a named character vector of hexadecimal colours and
\code{imperial_palettes} a named list of such vectors. \code{imperial_cols}
returns the requested colours (all of them when called without arguments).
\code{imperial_pal_discrete} returns a function of \code{n} giving \code{n}
colours; \code{imperial_pal_continuous} returns an interpolating function from
\code{\link[grDevices]{colorRampPalette}}. The scale functions return
\pkg{ggplot2} scales. \code{normalise_hex} returns trimmed, upper-case strings,
\code{hex_to_lab} a matrix of CIELAB coordinates, and
\code{pick_fallback_colours} a character vector of \code{n} colours.
}
\examples{
imperial_cols("Teal", "Crimson")
imperial_pal_discrete("cool")(7)
library(ggplot2)
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  scale_color_imperial()
}
