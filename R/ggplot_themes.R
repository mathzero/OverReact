### GGplot theme for REACT plots

# Created by Matt Whitaker 23/11/2021

# Defaults reproduce the themes' long-standing look: 10pt base text and 2pt strip-text
# margins. Further theme elements can be supplied through `...`.
theme_react <- function(base_family = "sans",
                        base_size = 10,
                        strip_text_size = 9,
                        strip_text_margin = 2,
                        subtitle_size = 12,
                        subtitle_margin = 7,
                        plot_title_size = 14,
                        plot_title_margin = 7,
                        ...){

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size, base_family = base_family,
                      base_line_size = 0.2,
                      base_rect_size = 0.1),
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(fill = "white", linewidth = 0.2),
      axis.text = ggplot2::element_text(size = 8),
      strip.text = ggplot2::element_text(face = "bold", size = strip_text_size,
                                         margin = ggplot2::margin(strip_text_margin, strip_text_margin,
                                                                  strip_text_margin, strip_text_margin),
                                         family = base_family),
      plot.subtitle = ggplot2::element_text(hjust = 0, size=subtitle_size,
                                            margin=ggplot2::margin(b=subtitle_margin),
                                            family=base_family),
      plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size,face = "bold",
                                         margin=ggplot2::margin(b=plot_title_margin),
                                         family=base_family)
    )
  ) + ggplot2::theme(...)

}


theme_mw <- function(base_family = "sans",
                     base_size = 10,
                     strip_text_size = 9,
                     strip_text_margin = 2,
                     subtitle_size = 12,
                     subtitle_margin = 7,
                     plot_title_size = 14,
                     plot_title_margin = 7,
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor =  ggplot2::element_blank(),
                     legend.position=c(1, 1),
                     legend.justification = c(1, 1),
                     legend.title = ggplot2::element_text(face = "bold"),
                     ...){

  # A numeric position places the legend inside the panel (ggplot2 >= 3.5.0 syntax).
  legend_placement <- if (is.numeric(legend.position)) {
    ggplot2::theme(legend.position = "inside", legend.position.inside = legend.position)
  } else {
    ggplot2::theme(legend.position = legend.position)
  }

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size, base_family = base_family,
                      base_line_size = 0.2,
                      base_rect_size = 0.1),
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.3),
      axis.ticks = ggplot2::element_line(colour = "black",linewidth=0.3),
      legend.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(fill = "white", linewidth = 0.2),
      axis.text = ggplot2::element_text(size = 8),
      strip.text = ggplot2::element_text(face = "bold", size = strip_text_size,
                                         margin = ggplot2::margin(strip_text_margin, strip_text_margin,
                                                                  strip_text_margin, strip_text_margin),
                                         family = base_family),

      panel.grid.major = panel.grid.major,
      panel.grid.minor =  panel.grid.minor,

      # legend
      legend.box.background = ggplot2::element_rect(colour = "black",linewidth=0.1),
      legend.justification = legend.justification,
      legend.title=legend.title,

     # subtitle and title settings
      plot.subtitle = ggplot2::element_text(hjust = 0, size=subtitle_size,
                                            margin=ggplot2::margin(b=subtitle_margin),
                                            family=base_family, face="plain"),
      plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size,
                                         margin=ggplot2::margin(b=plot_title_margin),
                                         family=base_family,face="bold")
    )
  ) + legend_placement + ggplot2::theme(...)

}
