#' @import ggplot2
#' @import dichromat

# define master list of Imperial Brand colours
# colours are taken from here: https://www.imperial.ac.uk/brand-style-guide/visual-identity/brand-colours/
imperial_colours= c(`navy` = "#002147",
                    `imperial blue` = "#003E74",
                    `light grey` = "#EBEEEE",
                    `cool grey` = "#9D9D9D",
                    `light blue` = "#D4EFFC",
                    `dark grey` = "#373A36",

                    # cool colours
                    `blue` ="#006EAF",
                    `process blue` ="#0091D4",
                    `pool blue` = "#00ACD7",
                    `dark teal` ="#0f8291",
                    `teal` = "#009CBC",
                    `seaglass` ="#379f9f",
                    `dark green` = "#02893B",
                    `kermit green` = "#66a40a",
                    `lime` = "#BBCE00",

                    # warm colours
                    `orange`="#D24000",
                    `tangerine`="#EC7300",
                    `lemon yellow`="#FFDD00",
                    `brick`="#A51900",
                    `red`="#DD2501",
                    `cherry red`="#E40043",
                    `raspberry red`="#9F004E",
                    `magenta pink`="#C81E78",
                    `iris` = "#751E66",
                    `violet`="#960078",
                    `plum`="#321E6D",
                    `purple` = "#653098")

# we don't want capitals here so change to lower case
# names(imperial_colours) <- stringr::str_to_lower(names(imperial_colours))

#' Function to extract imperial colors as hex codes
#'
#' @param ... Character names of imperial_colors
#'
imperial_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (imperial_colours)
  imperial_colours[cols]
}

imperial_palettes <- list(

  `default` = imperial_cols(
    "navy",
    "pool blue",
    "lime",
    "tangerine",
    "violet"
  ),



  # main three palettes
  `core`  = imperial_cols(
    # "navy",
    "imperial blue" ,
    "pool blue",
    # "light grey",
    "cool grey",
    "dark grey"),

  `cool`  = imperial_cols("imperial blue",
                          "process blue",
                          # "pool blue",
                          # "teal",
                          "seaglass",
                          # "dark teal",
                          "dark green",
                          "kermit green",
                          "lime",
                          "pool blue"
  ),

  `warm`   = imperial_cols("brick","tangerine","cherry red","lemon yellow"),

  # extended
  `extended` =imperial_cols("navy",
                            "imperial blue",
                            # "blue",
                            "dark teal",
                            "dark green",
                            "lime",
                            "lemon yellow",
                            "tangerine",
                            "brick",
                            "cherry",
                            "purple",
                            "dark grey"),

  # website colour themes
  `pink` =  imperial_cols("magenta pink", "violet", "plum", "purple"),
  `green` = imperial_cols("dark green", "dark teal", "blue", "kermit green"),
  `red` =  imperial_cols("brick", "orange", "imperial blue", "tangerine"),
  `blue` = imperial_cols("blue", "imperial blue", "dark teal", "pool blue","process blue"),
  `tbp` = imperial_cols("dark teal", "blue", "plum", "seaglass"),
  `vbn` =imperial_cols("violet", "blue", "navy", "pool blue"),
  `obib`= imperial_cols("orange", "dark green", "imperial blue", "pool blue"),
  `bnt` = imperial_cols("brick", "navy", "dark teal", "tangerine"),


  # Two-colour themes
  `two_col_grey_blue` =  imperial_cols("imperial blue","cool grey"),
  `two_col_grey_pool` =  imperial_cols("pool blue","cool grey"),
  `two_col_blue_navy` =  imperial_cols("imperial blue","navy"),
  `two_col_grey_orng` =  imperial_cols("orange","cool grey"),



  # manufactured gradients
  `bgrey_grad` = imperial_cols("navy", "imperial blue" ,
                               "blue", "pool blue", "light blue",
                               "light grey", "cool grey", "dark grey"
  ),

  `bgrn_grad` = imperial_cols("navy", "imperial blue" ,
                              "blue", "pool blue", "light blue",
                              "lime", "dark green"
  )
)

