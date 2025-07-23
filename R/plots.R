#───────────────────────────────────────────────────────────────────────────────
# Plot VIF trajectories for the variables returned by ModelMakerMulti
#───────────────────────────────────────────────────────────────────────────────
# Requires:
#   * ymods$vif_output        – the tidy‑frame produced by the updated code
#   * ggplot2, dplyr, ggforce – plus OverReact if you want the same colour theme
#   * extrafont (optional)    – fonts are loaded exactly as in your forest plot
#───────────────────────────────────────────────────────────────────────────────
#
# # load the fonts you already use
# extrafont::loadfonts(device = "win")

plotReactVIF <- function(vif_df,
                         adjustment_numbers       = c(0, 1, 3, 5),
                         adjustment_descriptions  = NULL,
                         vline                    = 5,                 # common VIF “rule‑of‑thumb”
                         legend.position          = "bottom",
                         palette                  = "core",
                         strip_text_size          = 8) {

  library(dplyr)
  library(ggplot2)
  library(ggforce)

  ## 1  Tidy and label ---------------------------------------------------------
  if (is.null(adjustment_descriptions)) {
    adjustment_descriptions <- paste("Adj.", adjustment_numbers)
  }
  if (length(adjustment_descriptions) != length(adjustment_numbers)) {
    stop("adjustment_numbers and adjustment_descriptions must be the same length.")
  }

  adj_df <- data.frame(
    adjustment   = adjustment_numbers,
    adjust_desc  = factor(adjustment_descriptions,
                          levels = adjustment_descriptions)
  )


  # keep only the VIF for the predictor itself
  vif_plot_df <- vif_df %>%
    filter(                     # one row per variable/step
           adjustment %in% adjustment_numbers) %>%
    left_join(adj_df, by = "adjustment") %>%
    mutate(Variable = factor(Variable, levels = unique(Variable)),
           adjustment = as.integer(adjustment))    # ensure integer for x‑axis


  ## 2  Build plot -------------------------------------------------------------
  vif_df |>
    ggplot(aes(y=term, x=vif,col=adjust_desc))+
    geom_bar(stat = "identity", position=position_dodge2())+
    ggforce::facet_col(vars(Variable),
                       scales = "fixed",
                       space  = "free",
                       shrink = TRUE,
                       drop   = TRUE)


  p_vif <- ggplot(vif_plot_df,
                  aes(x = adjustment,
                      y = vif,
                      colour = adjust_desc,
                      group  = adjust_desc)) +
    geom_hline(yintercept = vline,
               linetype = "dashed",
               linewidth = 0.25,
               colour = "grey40") +
    geom_line(linewidth = 0.3) +
    geom_point(size = 1) +
    scale_x_continuous(breaks = adjustment_numbers) +
    # OverReact::scale_color_imperial(palette = palette) +
    ggforce::facet_col(vars(Variable),
                       scales = "fixed",
                       space  = "free",
                       shrink = TRUE,
                       drop   = TRUE) +
    OverReact::theme_react(strip_text_size = strip_text_size) +
    labs(x     = "Number of covariates in model",
         y     = "Variance inflation factor",
         colour = "") +
    theme(legend.position     = legend.position,
          panel.grid          = element_blank(),
          panel.grid.major.y  = element_line(size = rel(0.1),
                                             linetype = "dashed"))

  p_vif
}

# ───────────────────────────────── Usage example ──────────────────────────────
# ymods <- ModelMakerMulti(…)
# plotReactVIF(ymods$vif_output,
#              adjustment_numbers      = c(0, 2, 4),
#              adjustment_descriptions = c("Crude",
#                                         "Age + Sex",
#                                         "Full model"))
