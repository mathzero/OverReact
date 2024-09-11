#' @import ggplot2
#' @import dichromat

# Define the new color palette based on the extracted HEX values
new_colours <- c(
  Dark = "#232333",
  Navy_Blue = "#000080",
  Saddle_Brown = "#8b4513",
  Teal = "#008080",
  Medium_Violet_Red = "#c71585",
  Indigo = "#4b0082",
  Crimson = "#dc143c",
  Orange_Red = "#ff4500",
  Dark_Green = "#006400",
  Slate_Gray = "#708090",
  Imperial_Blue = "#0000cd",
  Yellow = "#ffff00",
  Turquoise = "#40e0d0",
  Violet = "#ee82ee",
  Medium_Blue_Slate = "#7b68ee",
  Red = "#ff0000",
  Dark_Orange = "#ff8c00",
  Spring_Green = "#00ff7f",
  White_Smoke = "#f5f5f5",
  Deep_Sky_Blue = "#00bfff",
  Khaki = "#f0e68c",
  Pale_Turquoise = "#afeeee",
  Light_Pink = "#ffb6c1",
  Lavender = "#e6e6fa",
  Salmon = "#fa8072",
  Orange = "#ffa500",
  Pale_Green = "#98fb98"
)

# Function to extract new colors as hex codes
new_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (new_colours)
  new_colours[cols]
}

# Define new palettes
new_palettes <- list(

  `default` = new_cols(
    "Navy_Blue",
    "Teal",
    "Turquoise",
    "Lime",
    "Orange_Red",
    "Violet",
    "Dark_Green",
    "Deep_Sky_Blue",
    "Saddle_Brown",
    "Khaki"
  ),

  `earth` = new_cols(
    "Saddle_Brown",
    "Dark_Green",
    "Slate_Gray",
    "Khaki",
    "Pale_Green"
  ),

  `cool` = new_cols(
    "Teal",
    "Indigo",
    "Deep_Sky_Blue",
    "Turquoise",
    "Pale_Turquoise"
  ),

  `warm` = new_cols(
    "Orange_Red",
    "Dark_Orange",
    "Salmon",
    "Crimson",
    "Yellow"
  ),

  `soft` = new_cols(
    "White_Smoke",
    "Lavender",
    "Light_Pink",
    "Pale_Turquoise",
    "Pale_Green"
  ),

  `bold` = new_cols(
    "Red",
    "Dark_Orange",
    "Medium_Violet_Red",
    "Imperial_Blue",
    "Crimson"
  ),

  `nature` = new_cols(
    "Spring_Green",
    "Teal",
    "Dark_Green",
    "Khaki",
    "Slate_Gray"
  ),

  `two_col_grey_teal` = new_cols(
    "Slate_Gray", "Teal"
  ),

  `two_col_pink_purple` = new_cols(
    "Light_Pink", "Lavender"
  ),

  `two_col_blue_green` = new_cols(
    "Deep_Sky_Blue", "Pale_Green"
  )
)

# Example function to apply new palettes in ggplot2
apply_palette <- function(palette_name) {
  ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) +
    geom_bar() +
    scale_fill_manual(values = new_palettes[[palette_name]]) +
    theme_minimal()
}
