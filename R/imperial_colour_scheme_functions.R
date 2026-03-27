#' Return function to interpolate a new color palette
#'
#' @param palette Character name of palette in imperial_palettes. Choose from:
#' \itemize{
#'  \item default, earth, cool, warm, soft, bold, nature,
#'  \item two_col_grey_teal, two_col_pink_purple, two_col_blue_green
#' }
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @import ggplot2
#' @import dichromat
#' @import tinter


normalise_hex <- function(x) {
  toupper(trimws(x))
}

hex_to_lab <- function(x) {
  rgb <- t(grDevices::col2rgb(x) / 255)
  grDevices::convertColor(
    rgb,
    from = "sRGB",
    to = "Lab",
    scale.in = 1
  )
}

pick_fallback_colours <- function(
  n,
  existing = character(),
  fallback_palette = "Dynamic",
  candidate_pool = 64
) {
  if (n <= 0) {
    return(character(0))
  }

  available_fallbacks <- grDevices::hcl.pals(type = "qualitative")

  if (!fallback_palette %in% available_fallbacks) {
    stop(
      "Unknown fallback_palette '", fallback_palette, "'. ",
      "Choose one of: ",
      paste(available_fallbacks, collapse = ", "),
      call. = FALSE
    )
  }

  selected <- character(0)
  pool_size <- max(candidate_pool, n * 8)

  while (length(selected) < n) {
    candidates <- grDevices::hcl.colors(
      n = pool_size,
      palette = fallback_palette
    )

    candidates <- unique(candidates)
    candidates <- candidates[
      !normalise_hex(candidates) %in%
        normalise_hex(c(existing, selected))
    ]

    if (length(candidates) == 0) {
      pool_size <- pool_size * 2
      next
    }

    if (length(existing) + length(selected) == 0) {
      n_take <- min(n, length(candidates))
      selected <- c(selected, candidates[seq_len(n_take)])
      next
    }

    while (length(selected) < n && length(candidates) > 0) {
      reference <- c(existing, selected)

      ref_lab <- hex_to_lab(reference)
      cand_lab <- hex_to_lab(candidates)

      min_dist <- vapply(
        seq_len(nrow(cand_lab)),
        function(i) {
          diffs <- sweep(ref_lab, 2, cand_lab[i, ], FUN = "-")
          min(rowSums(diffs^2))
        },
        numeric(1)
      )

      best_idx <- which.max(min_dist)
      selected <- c(selected, candidates[best_idx])
      candidates <- candidates[-best_idx]
    }

    pool_size <- pool_size * 2
  }

  selected[seq_len(n)]
}

imperial_pal_discrete <- function(
  palette = "default",
  reverse = FALSE,
  fallback = TRUE,
  fallback_palette = "Dynamic"
) {
  base_pal <- imperial_palettes[[palette]]

  if (is.null(base_pal)) {
    stop(
      "Unknown palette '", palette, "'. ",
      "Choose one of: ",
      paste(names(imperial_palettes), collapse = ", "),
      call. = FALSE
    )
  }

  if (reverse) {
    base_pal <- rev(base_pal)
  }

  function(n) {
    if (n <= length(base_pal)) {
      return(unname(base_pal[seq_len(n)]))
    }

    if (!isTRUE(fallback)) {
      stop(
        "Palette '", palette, "' has ", length(base_pal),
        " colours, but ", n, " were requested.",
        call. = FALSE
      )
    }

    extras_needed <- n - length(base_pal)

    extras <- pick_fallback_colours(
      n = extras_needed,
      existing = base_pal,
      fallback_palette = fallback_palette
    )

    c(unname(base_pal), extras)
  }
}

imperial_pal_continuous <- function(
  palette = "default",
  reverse = FALSE,
  ...
) {
  pal <- imperial_palettes[[palette]]

  if (is.null(pal)) {
    stop(
      "Unknown palette '", palette, "'. ",
      "Choose one of: ",
      paste(names(imperial_palettes), collapse = ", "),
      call. = FALSE
    )
  }

  if (reverse) {
    pal <- rev(pal)
  }

  grDevices::colorRampPalette(pal, space = "Lab", ...)
}

scale_color_new <- function(
  palette = "default",
  discrete = TRUE,
  reverse = FALSE,
  fallback = TRUE,
  fallback_palette = "Dynamic",
  ...
) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      palette = imperial_pal_discrete(
        palette = palette,
        reverse = reverse,
        fallback = fallback,
        fallback_palette = fallback_palette
      ),
      ...
    )
  } else {
    pal <- imperial_pal_continuous(
      palette = palette,
      reverse = reverse
    )

    ggplot2::scale_color_gradientn(
      colours = pal(256),
      ...
    )
  }
}

scale_fill_new <- function(
  palette = "default",
  discrete = TRUE,
  reverse = FALSE,
  fallback = TRUE,
  fallback_palette = "Dynamic",
  ...
) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      palette = imperial_pal_discrete(
        palette = palette,
        reverse = reverse,
        fallback = fallback,
        fallback_palette = fallback_palette
      ),
      ...
    )
  } else {
    pal <- imperial_pal_continuous(
      palette = palette,
      reverse = reverse
    )

    ggplot2::scale_fill_gradientn(
      colours = pal(256),
      ...
    )
  }
}

scale_fill_imperial <- scale_fill_new
scale_color_imperial <- scale_color_new