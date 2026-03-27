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
  vif_plot_df <- vif_df |>
    dplyr::filter(adjustment %in% adjustment_numbers) |>
    dplyr::left_join(adj_df, by = "adjustment") |>
    dplyr::mutate(
      Variable = factor(Variable, levels = unique(Variable)),
      adjustment = as.integer(adjustment)
    )


  p_vif <- ggplot2::ggplot(
    vif_plot_df,
    ggplot2::aes(
      x = adjustment,
      y = vif,
      colour = adjust_desc,
      group = adjust_desc
    )
  ) +
    ggplot2::geom_hline(
      yintercept = vline,
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey40"
    ) +
    ggplot2::geom_line(linewidth = 0.3) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_x_continuous(breaks = adjustment_numbers) +
    # OverReact::scale_color_imperial(palette = palette) +
    ggforce::facet_col(ggplot2::vars(Variable),
                       scales = "fixed",
                       space  = "free",
                       shrink = TRUE,
                       drop   = TRUE) +
    OverReact::theme_react(strip_text_size = strip_text_size) +
    ggplot2::labs(x     = "Number of covariates in model",
                  y     = "Variance inflation factor",
                  colour = "") +
    ggplot2::theme(
      legend.position     = legend.position,
      panel.grid          = ggplot2::element_blank(),
      panel.grid.major.y  = ggplot2::element_line(
        linewidth = ggplot2::rel(0.1),
        linetype = "dashed"
      )
    )

  p_vif
}

# ───────────────────────────────── Usage example ──────────────────────────────
# ymods <- ModelMakerMulti(…)
# plotReactVIF(ymods$vif_output,
#              adjustment_numbers      = c(0, 2, 4),
#              adjustment_descriptions = c("Crude",
#                                         "Age + Sex",
#                                         "Full model"))


.react_pretty_name <- function(var, dat = NULL, name_fun = NULL, auto_pretty = TRUE) {
  if (exists("get_pretty_name", mode = "function")) {
    return(get_pretty_name(var, dat = dat, name_fun = name_fun, auto_pretty = auto_pretty))
  }

  out <- as.character(var)
  if (isTRUE(auto_pretty)) {
    out <- gsub("_", " ", out)
    out <- tools::toTitleCase(out)
  }
  if (is.function(name_fun)) {
    out <- tryCatch(name_fun(out), error = function(e) out)
  }
  out
}


.react_detect_effect <- function(df, effect = c("auto", "OR", "Beta", "RD")) {
  effect <- match.arg(effect)
  available <- intersect(c("OR", "Beta", "RD"), names(df))

  if (!length(available)) {
    stop("Could not find an effect column. Expected one of: OR, Beta, RD.")
  }

  if (effect != "auto") {
    if (!effect %in% available) {
      stop("Requested effect column '", effect, "' is not present in the input.")
    }
    return(effect)
  }

  if ("OR" %in% available) {
    return("OR")
  }
  if ("Beta" %in% available) {
    return("Beta")
  }
  "RD"
}


.react_apply_adjustment_labels <- function(df, adjustment_labels = NULL) {
  if (!"adjustment_label" %in% names(df)) {
    df$adjustment_label <- NA_character_
  }

  blank_label <- is.na(df$adjustment_label) | trimws(df$adjustment_label) == ""

  if ("model" %in% names(df)) {
    df$adjustment_label[blank_label] <- as.character(df$model[blank_label])
  }

  blank_label <- is.na(df$adjustment_label) | trimws(df$adjustment_label) == ""

  if ("adjustment" %in% names(df)) {
    df$adjustment_label[blank_label] <- as.character(df$adjustment[blank_label])
  }

  blank_label <- is.na(df$adjustment_label) | trimws(df$adjustment_label) == ""
  df$adjustment_label[blank_label] <- "Model"

  df$adjustment_label[df$adjustment_label %in% c("crude", "Crude")] <- "Crude"

  if (!is.null(adjustment_labels)) {
    if (is.null(names(adjustment_labels))) {
      seen <- unique(df$adjustment_label)
      n_replace <- min(length(seen), length(adjustment_labels))
      remap <- stats::setNames(adjustment_labels[seq_len(n_replace)], seen[seq_len(n_replace)])
      hit <- df$adjustment_label %in% names(remap)
      df$adjustment_label[hit] <- unname(remap[df$adjustment_label[hit]])
    } else {
      hit_label <- df$adjustment_label %in% names(adjustment_labels)
      df$adjustment_label[hit_label] <- unname(adjustment_labels[df$adjustment_label[hit_label]])

      if ("adjustment" %in% names(df)) {
        adj_chr <- as.character(df$adjustment)
        hit_adj <- adj_chr %in% names(adjustment_labels)
        df$adjustment_label[hit_adj] <- unname(adjustment_labels[adj_chr[hit_adj]])
      }
    }
  }

  df
}


.react_normalise_adjustment <- function(df) {
  if (!"adjustment" %in% names(df)) {
    df$adjustment <- NA_integer_
  }

  adjustment_num <- suppressWarnings(as.integer(as.character(df$adjustment)))

  if (all(is.na(adjustment_num))) {
    seen <- unique(df$adjustment_label)
    crude_first <- c(seen[seen == "Crude"], seen[seen != "Crude"])
    remap <- stats::setNames(seq_along(crude_first) - 1L, crude_first)
    df$adjustment <- unname(remap[df$adjustment_label])
  } else {
    df$adjustment <- adjustment_num

    missing_adjustment <- is.na(df$adjustment)
    if (any(missing_adjustment)) {
      seen <- unique(df$adjustment_label[missing_adjustment])
      start <- if (all(is.na(df$adjustment))) 0L else max(df$adjustment, na.rm = TRUE) + 1L
      remap <- stats::setNames(seq.int(from = start, length.out = length(seen)), seen)
      df$adjustment[missing_adjustment] <- unname(remap[df$adjustment_label[missing_adjustment]])
    }
  }

  df
}


.react_standardise_plot_df <- function(df,
                                       effect = c("auto", "OR", "Beta", "RD"),
                                       variables = NULL,
                                       adjustment_labels = NULL,
                                       include_reference = TRUE) {
  if (!is.data.frame(df)) {
    stop("Expected a data.frame-like plotting input.")
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if ("Level" %in% names(df) && !"Category" %in% names(df)) {
    df$Category <- df$Level
  }

  if (!"Variable" %in% names(df)) {
    stop("Plot tables must contain a Variable column. Pass an lm()/glm() object directly instead.")
  }

  effect_name <- .react_detect_effect(df, effect = effect)

  if (!"P_value" %in% names(df)) df$P_value <- NA_real_
  if (!"Lower" %in% names(df)) df$Lower <- NA_real_
  if (!"Upper" %in% names(df)) df$Upper <- NA_real_

  std <- data.frame(
    Variable = as.character(df$Variable),
    Category = as.character(df$Category),
    estimate = suppressWarnings(as.numeric(df[[effect_name]])),
    Lower = suppressWarnings(as.numeric(df$Lower)),
    Upper = suppressWarnings(as.numeric(df$Upper)),
    P_value = suppressWarnings(as.numeric(df$P_value)),
    stringsAsFactors = FALSE
  )

  if ("adjustment" %in% names(df)) std$adjustment <- df$adjustment
  if ("adjustment_label" %in% names(df)) std$adjustment_label <- as.character(df$adjustment_label)
  if ("model" %in% names(df)) std$model <- as.character(df$model)
  if ("term_raw" %in% names(df)) std$term_raw <- as.character(df$term_raw)
  if ("is_reference" %in% names(df)) std$is_reference <- as.logical(df$is_reference)
  if ("is_intercept" %in% names(df)) std$is_intercept <- as.logical(df$is_intercept)

  std$.input_order <- seq_len(nrow(std))
  std <- .react_apply_adjustment_labels(std, adjustment_labels = adjustment_labels)
  std <- .react_normalise_adjustment(std)

  std$Category_display <- std$Category
  blank_category <- is.na(std$Category_display) | trimws(std$Category_display) == ""
  std$Category_display[blank_category] <- std$Variable[blank_category]

  if (!include_reference) {
    if ("is_reference" %in% names(std)) {
      std <- std[!ifelse(is.na(std$is_reference), FALSE, std$is_reference), , drop = FALSE]
    } else {
      std <- std[!grepl("\\[reference\\]$", std$Category_display), , drop = FALSE]
    }
  }

  if (!is.null(variables)) {
    variables <- as.character(variables)
    pretty_variables <- tools::toTitleCase(gsub("_", " ", variables))
    std <- std[std$Variable %in% c(variables, pretty_variables), , drop = FALSE]
  }

  if (!nrow(std)) {
    stop("No plotting rows remain after filtering.")
  }

  attr(std, "effect_name") <- effect_name
  std
}


.react_model_to_plot_df <- function(model,
                                    variables = NULL,
                                    adjustment_label = NULL,
                                    adjustment = NULL,
                                    name_fun = NULL,
                                    auto_pretty = TRUE,
                                    include_reference = TRUE) {
  if (!inherits(model, c("lm", "glm", "gam"))) {
    stop("Models passed to plotReactForest() must inherit from lm, glm, or gam.")
  }

  coef_tab <- as.data.frame(makeORTable(model, ref_level = NULL), stringsAsFactors = FALSE)
  if (!nrow(coef_tab)) {
    stop("The supplied model did not return any coefficients to plot.")
  }

  if ("is_intercept" %in% names(coef_tab)) {
    coef_tab <- coef_tab[!coef_tab$is_intercept, , drop = FALSE]
  } else {
    coef_tab <- coef_tab[!grepl("Intercept", coef_tab$Level, ignore.case = TRUE), , drop = FALSE]
  }
  if (!nrow(coef_tab)) {
    stop("The supplied model only returned intercept terms.")
  }

  model_frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  model_terms <- tryCatch(stats::terms(model), error = function(e) NULL)
  term_labels <- attr(model_terms, "term.labels")
  mm <- tryCatch(stats::model.matrix(model), error = function(e) NULL)

  term_index <- rep(NA_integer_, nrow(coef_tab))
  if (!is.null(mm) && !is.null(term_labels)) {
    assign_index <- attr(mm, "assign")
    term_values <- if ("term_raw" %in% names(coef_tab)) coef_tab$term_raw else coef_tab$Level
    col_index <- match(term_values, colnames(mm))
    valid_cols <- !is.na(col_index)
    term_index[valid_cols] <- assign_index[col_index[valid_cols]]
  }

  out_list <- list()
  used_rows <- rep(FALSE, nrow(coef_tab))

  if (!is.null(term_labels) && length(term_labels)) {
    for (term_id in sort(unique(term_index[!is.na(term_index) & term_index > 0L]))) {
      term <- term_labels[[term_id]]
      term_name <- gsub("`", "", term, fixed = TRUE)
      pretty_term <- .react_pretty_name(term_name, dat = model_frame, name_fun = name_fun, auto_pretty = auto_pretty)

      if (!is.null(variables) && !term_name %in% variables && !pretty_term %in% variables) {
        next
      }

      rows <- which(term_index == term_id)
      if (!length(rows)) {
        next
      }

      used_rows[rows] <- TRUE
      tmp <- coef_tab[rows, , drop = FALSE]
      tmp$Variable <- pretty_term

      if (!is.null(model_frame) && term_name %in% names(model_frame) && is.factor(model_frame[[term_name]])) {
        term_pattern <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", term))
        tmp$Category <- sub(term_pattern, "", tmp$Level)

        if (include_reference) {
          ref_level <- levels(model_frame[[term_name]])[1]
          ref_row <- tmp[rep(1L, 1L), , drop = FALSE]
          ref_row[1, ] <- NA
          ref_row$Level <- paste0(term, ref_level, " [reference]")
          ref_row$term_raw <- NA_character_
          ref_row$OR <- NA_real_
          ref_row$Lower <- NA_real_
          ref_row$Upper <- NA_real_
          ref_row$P_value <- NA_real_
          ref_row$is_reference <- TRUE
          ref_row$is_intercept <- FALSE
          ref_row$Variable <- pretty_term
          ref_row$Category <- paste0(ref_level, " [reference]")
          tmp <- dplyr::bind_rows(ref_row, tmp)
        }
      } else if (nrow(tmp) == 1L) {
        tmp$Category <- term_name
      } else {
        tmp$Category <- tmp$Level
      }

      out_list[[length(out_list) + 1L]] <- tmp
    }
  }

  if (!length(out_list)) {
    remaining <- coef_tab
  } else {
    remaining <- coef_tab[!used_rows, , drop = FALSE]
  }

  if (nrow(remaining)) {
    if (!is.null(variables)) {
      keep_remaining <- remaining$Level %in% variables
      remaining <- remaining[keep_remaining, , drop = FALSE]
    }

    if (nrow(remaining)) {
      remaining$Variable <- vapply(
        remaining$Level,
        .react_pretty_name,
        FUN.VALUE = character(1),
        dat = model_frame,
        name_fun = name_fun,
        auto_pretty = auto_pretty
      )
      remaining$Category <- remaining$Level
      out_list[[length(out_list) + 1L]] <- remaining
    }
  }

  if (!length(out_list)) {
    stop("No model terms remain after filtering.")
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL

  effect_name <- "Beta"
  if (!is.null(model$family$family) && identical(model$family$family, "binomial")) {
    effect_name <- "OR"
  }

  out <- data.frame(
    Variable = as.character(out$Variable),
    Category = as.character(out$Category),
    estimate = suppressWarnings(as.numeric(out$OR)),
    Lower = suppressWarnings(as.numeric(out$Lower)),
    Upper = suppressWarnings(as.numeric(out$Upper)),
    P_value = suppressWarnings(as.numeric(out$P_value)),
    stringsAsFactors = FALSE
  )

  out$Category_display <- out$Category
  blank_category <- is.na(out$Category_display) | trimws(out$Category_display) == ""
  out$Category_display[blank_category] <- out$Variable[blank_category]
  out$adjustment_label <- if (is.null(adjustment_label)) "Model" else adjustment_label
  out$adjustment <- if (is.null(adjustment)) 0L else adjustment
  out$.input_order <- seq_len(nrow(out))

  attr(out, "effect_name") <- effect_name
  out
}


.react_coerce_forest_input <- function(x,
                                       effect = c("auto", "OR", "Beta", "RD"),
                                       variables = NULL,
                                       adjustment_labels = NULL,
                                       name_fun = NULL,
                                       auto_pretty = TRUE,
                                       include_reference = TRUE) {
  effect <- match.arg(effect)

  if (is.data.frame(x)) {
    return(.react_standardise_plot_df(
      x,
      effect = effect,
      variables = variables,
      adjustment_labels = adjustment_labels,
      include_reference = include_reference
    ))
  }

  if (inherits(x, c("lm", "glm", "gam"))) {
    out <- .react_model_to_plot_df(
      x,
      variables = variables,
      name_fun = name_fun,
      auto_pretty = auto_pretty,
      include_reference = include_reference
    )
    out <- .react_apply_adjustment_labels(out, adjustment_labels = adjustment_labels)
    out <- .react_normalise_adjustment(out)
    return(out)
  }

  if (is.list(x) && any(vapply(x, inherits, logical(1), what = c("lm", "glm", "gam")))) {
    if (!all(vapply(x, function(z) inherits(z, c("lm", "glm", "gam")), logical(1)))) {
      stop("Model lists supplied to plotReactForest() must contain only lm/glm/gam objects.")
    }

    model_labels <- names(x)
    if (is.null(model_labels) || any(model_labels == "")) {
      model_labels <- paste("Model", seq_along(x))
    }

    out_list <- Map(
      function(mod, lab, idx) {
        .react_model_to_plot_df(
          mod,
          variables = variables,
          adjustment_label = lab,
          adjustment = idx - 1L,
          name_fun = name_fun,
          auto_pretty = auto_pretty,
          include_reference = include_reference
        )
      },
      x,
      model_labels,
      seq_along(x)
    )

    out <- do.call(rbind, out_list)
    out <- .react_apply_adjustment_labels(out, adjustment_labels = adjustment_labels)
    out <- .react_normalise_adjustment(out)
    attr(out, "effect_name") <- attr(out_list[[1]], "effect_name")
    return(out)
  }

  if (is.list(x) && !is.null(x$plot_output_RDs) && identical(effect, "RD")) {
    return(.react_standardise_plot_df(
      x$plot_output_RDs,
      effect = "RD",
      variables = variables,
      adjustment_labels = adjustment_labels,
      include_reference = include_reference
    ))
  }

  if (is.list(x) && !is.null(x$plot_output)) {
    return(.react_standardise_plot_df(
      x$plot_output,
      effect = effect,
      variables = variables,
      adjustment_labels = adjustment_labels,
      include_reference = include_reference
    ))
  }

  stop("plotReactForest() expects a plot data frame, a ModelMaker* result object, an lm()/glm()/gam() fit, or a list of fitted models.")
}


plotReactForest <- function(univ_df_plot,
                            adjustment_numbers = NULL,
                            adjustment_descriptions = NULL,
                            xrange = c(0.25, 16),
                            legend.position = "bottom",
                            strip_borders = FALSE,
                            insignificant_results_greyed_out = TRUE,
                            palette = "default",
                            alpha_val = 0.8,
                            strip_text_size = 8,
                            arrangeByOR = FALSE,
                            variables = NULL,
                            effect = c("auto", "OR", "Beta", "RD"),
                            include_reference = TRUE,
                            name_fun = NULL,
                            auto_pretty = TRUE,
                            title = NULL,
                            subtitle = NULL,
                            xlab = NULL,
                            null_value = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install dplyr.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2.")
  if (!requireNamespace("ggforce", quietly = TRUE)) stop("Please install ggforce.")
  if (!requireNamespace("ggnewscale", quietly = TRUE)) stop("Please install ggnewscale.")
  if (!requireNamespace("MASS", quietly = TRUE)) stop("Please install MASS.")

  effect <- match.arg(effect)

  df <- .react_coerce_forest_input(
    univ_df_plot,
    effect = effect,
    variables = variables,
    adjustment_labels = NULL,
    name_fun = name_fun,
    auto_pretty = auto_pretty,
    include_reference = include_reference
  )

  effect_name <- attr(df, "effect_name")
  if (is.null(effect_name)) effect_name <- "OR"

  if (is.null(null_value)) {
    null_value <- if (identical(effect_name, "OR")) 1 else 0
  }

  if (is.null(xlab)) {
    xlab <- switch(effect_name,
                   OR = "Odds ratio",
                   Beta = "Beta coefficient",
                   RD = "Risk difference",
                   effect_name)
  }

  if (is.null(adjustment_numbers)) {
    adjustment_numbers <- unique(as.character(df$adjustment))
  }
  adjustment_numbers <- as.character(adjustment_numbers)

  available_adjustments <- unique(as.character(df$adjustment))
  if (!any(adjustment_numbers %in% available_adjustments)) {
    stop("None of the requested adjustment_numbers were found in the plotting input.")
  }

  adjustment_lookup <- df |>
    dplyr::mutate(adjustment = as.character(adjustment)) |>
    dplyr::distinct(adjustment, adjustment_label)

  if (is.null(adjustment_descriptions)) {
    adjustment_descriptions <- adjustment_lookup$adjustment_label[
      match(adjustment_numbers, adjustment_lookup$adjustment)
    ]
    missing_desc <- is.na(adjustment_descriptions) | adjustment_descriptions == ""
    adjustment_descriptions[missing_desc] <- adjustment_numbers[missing_desc]
  }

  if (length(adjustment_descriptions) != length(adjustment_numbers)) {
    stop("adjustment_numbers and adjustment_descriptions must be the same length.")
  }

  var_levels <- unique(as.character(df$Variable))

  df <- df |>
    dplyr::mutate(
      Variable = factor(as.character(Variable), levels = var_levels),
      Category = as.character(Category_display),
      adjustment = as.character(adjustment),
      effect_value = estimate
    )

  df <- df |>
    dplyr::group_by(Variable) |>
    dplyr::mutate(Category_order = match(Category, unique(Category))) |>
    dplyr::ungroup() |>
    dplyr::mutate(Category_key = interaction(Variable, Category, drop = TRUE, lex.order = TRUE))

  stripes_df <- df |>
    dplyr::group_by(Variable, Category, Category_key, Category_order) |>
    dplyr::summarise(mean_effect = mean(effect_value, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Variable) |>
    dplyr::arrange(Category_order, .by_group = TRUE) |>
    dplyr::mutate(row_in_panel = dplyr::row_number()) |>
    dplyr::ungroup()

  if (arrangeByOR) {
    stripes_df <- stripes_df |>
      dplyr::group_by(Variable) |>
      dplyr::arrange(mean_effect, .by_group = TRUE) |>
      dplyr::mutate(row_in_panel = dplyr::row_number()) |>
      dplyr::ungroup()
  }

  stripes_df <- stripes_df |>
    dplyr::mutate(
      stripe_col = dplyr::if_else(row_in_panel %% 2 == 0, "white", "grey96"),
      Variable = factor(Variable, levels = var_levels)
    )

  key_levels <- stripes_df |>
    dplyr::arrange(Variable, row_in_panel) |>
    dplyr::pull(Category_key) |>
    unique()

  label_map <- stripes_df |>
    dplyr::distinct(Category_key, Category) |>
    dplyr::mutate(idx = match(Category_key, key_levels)) |>
    dplyr::arrange(idx) |>
    (\(x) stats::setNames(x$Category, x$Category_key))()

  adj_df <- data.frame(
    adjustment = adjustment_numbers,
    adjust_desc = factor(adjustment_descriptions, levels = adjustment_descriptions),
    stringsAsFactors = FALSE
  )

  plot_df <- df |>
    dplyr::filter(adjustment %in% adjustment_numbers) |>
    dplyr::left_join(adj_df, by = "adjustment") |>
    dplyr::left_join(
      stripes_df[, c("Variable", "Category_key", "stripe_col")],
      by = c("Variable", "Category_key")
    ) |>
    dplyr::mutate(
      Category_key = factor(Category_key, levels = key_levels),
      Variable = factor(Variable, levels = var_levels),
      myshape = dplyr::case_when(
        !insignificant_results_greyed_out ~ "dark",
        is.na(P_value) ~ "dark",
        P_value > 0.05 ~ "light",
        TRUE ~ "dark"
      )
    )

  if (!nrow(plot_df)) {
    stop("No plotting rows remain after filtering by adjustment_numbers.")
  }

  panel_stripes <- stripes_df |>
    dplyr::mutate(
      Category_key = factor(Category_key, levels = key_levels),
      Variable = factor(Variable, levels = var_levels)
    )

  dodger <- if (length(adjustment_numbers) == 1L) {
    ggplot2::position_dodge2(width = 0.5)
  } else {
    ggplot2::position_dodge2(width = 0.5, reverse = TRUE)
  }

  if (!identical(effect_name, "OR") && identical(xrange, c(0.25, 16))) {
    xrange <- NULL
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Category_key, y = effect_value, colour = adjust_desc, group = adjust_desc)
  ) +
    ggplot2::geom_tile(
      data = panel_stripes,
      ggplot2::aes(x = Category_key, y = null_value, height = Inf, fill = stripe_col),
      inherit.aes = FALSE,
      alpha = alpha_val,
      colour = if (isTRUE(strip_borders)) "grey70" else NA,
      linetype = "dashed",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_hline(yintercept = null_value, linewidth = 0.1, colour = "grey30") +
    ggplot2::geom_point(
      position = dodger,
      size = 0.9,
      ggplot2::aes(shape = myshape, alpha = myshape),
      na.rm = TRUE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Lower, ymax = Upper, alpha = myshape),
      position = dodger,
      width = 0.5,
      show.legend = FALSE,
      linewidth = 0.2,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_shape_manual(values = c(light = 0, dark = 15), guide = "none") +
    ggforce::facet_col(
      facets = ggplot2::vars(Variable),
      scales = "free_y",
      space = "free",
      shrink = TRUE,
      drop = TRUE
    ) +
    ggplot2::scale_x_discrete(labels = label_map) +
    ggplot2::labs(
      x = "",
      y = xlab,
      colour = "",
      title = title,
      subtitle = subtitle
    ) +
    theme_react(strip_text_size = strip_text_size) +
    ggplot2::theme(
      legend.position = legend.position,
      panel.grid = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        linewidth = ggplot2::rel(0.05),
        linetype = "dotted"
      )
    ) +
    ggplot2::scale_alpha_manual(values = c(dark = 1, light = 0.3), guide = "none")

  if (identical(effect_name, "OR")) {
    if (is.null(xrange)) xrange <- c(0.25, 16)
    breaks <- 2 ^ seq(log2(xrange[[1]]), log2(xrange[[2]]))
    p <- p + ggplot2::scale_y_continuous(
      trans = "log2",
      labels = function(x) MASS::fractions(x),
      breaks = breaks,
      limits = xrange
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(limits = xrange)
  }

  if (exists("scale_color_imperial", mode = "function")) {
    p <- p + scale_color_imperial(palette = palette)
  }

  p
}


# ─────────────────────────────── Usage examples ───────────────────────────────
# mymods <- ModelMakerMultiRD(...)
# plotReactForest(mymods)
#
# fit_glm <- glm(y_bin ~ x1 + x2 + cat, data = dat, family = "binomial")
# plotReactForest(fit_glm, variables = c("x1", "cat"))
#
# fit_lm <- lm(y ~ x1 + x2 + cat, data = dat)
# plotReactForest(fit_lm, variables = c("x1", "cat"))
