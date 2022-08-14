### GGplot theme for REACT plots

# Created by Matt Whitaker 23/11/2021
# devtools::document()
# devtools::check()

#' @import dplyr
#' @import ggplot2
#' @import extrafont
#' @import hrbrthemes
#' @import showtext
#' @import sysfonts


theme_react <- function(base_family = "IBM Plex Sans",
                        base_size = 11,
                        strip_text_size = 10,
                        strip_text_margin = 5,
                        subtitle_size = 12,
                        subtitle_margin = 7,
                        plot_title_size = 14,
                        plot_title_margin = 7,
                        ...){


  # check if selected font is in loaded fonts
  if(!base_family %in% as.data.frame(sysfonts::font_files())$family){
    print(paste0(base_family," is not loaded as a system font. Replacing with sans for now"))
    base_family <- "sans"
    semibold_family=base_family
  }else{
    semibold_family="IBM Plex Sans SemiBold"
  }


  theme_bw(base_size = 10, base_family = base_family,
           base_line_size = 0.2,
           base_rect_size = 0.1) %+replace%
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key =element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "white",size = 0.2),
      axis.text = element_text(size = 8),
      strip.text = element_text(face = "bold",size = strip_text_size,margin = ggplot2::margin(rep(2,4)),
                                family=semibold_family),

      # # from Silge
      # strip.text = ggplot2::element_text(hjust = 0, size=strip_text_size,
      #                                         margin=margin(b=strip_text_margin),
      #                                         family="IBM Plex Sans Medium"),
      plot.subtitle = ggplot2::element_text(hjust = 0, size=subtitle_size,
                                                 margin=ggplot2::margin(b=subtitle_margin),
                                                 family=base_family),
      plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size,
                                              margin=ggplot2::margin(b=plot_title_margin),
                                              family=semibold_family)
    )

}

