# Display tables for tableOne() output -------------------------------------------

# Render a tableOne() result as a flextable or gt table: variable rows in bold, level rows
# indented, p-values and SMDs formatted, and a footnote naming the test behind each
# p-value. Both packages are optional and only needed for the chosen output.
tableOneDisplay <- function(tab, output = c("flextable", "gt"), title = NULL,
                            p_digits = 3, smd_digits = 2) {
  output <- match.arg(output)
  if (!requireNamespace(output, quietly = TRUE)) {
    stop("Install the ", output, " package to use output = \"", output, "\".")
  }
  if (!is.data.frame(tab) || !all(c("Variable", "Level") %in% names(tab))) {
    stop("`tab` must be a tableOne() result.")
  }

  header <- tab$Variable != " "
  display <- data.frame(Characteristic = ifelse(header, tab$Variable, tab$Level),
                        check.names = FALSE, stringsAsFactors = FALSE)
  for (col in setdiff(names(tab), c("Variable", "Level"))) display[[col]] <- tab[[col]]

  # Missing values stay missing (formatC() would print them as " NA") and display blank.
  fixed <- function(x, digits) ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
  if (is.numeric(display$`P-value`)) {
    p <- display$`P-value`
    display$`P-value` <- ifelse(p < 10^-p_digits, paste0("<", fixed(10^-p_digits, p_digits)), fixed(p, p_digits))
  }
  if (is.numeric(display$SMD)) display$SMD <- fixed(display$SMD, smd_digits)
  display[] <- lapply(display, function(col) {
    col <- as.character(col)
    col[is.na(col) | trimws(col) == ""] <- ""
    col
  })

  tests <- attr(tab, "tests")
  note <- NULL
  if (length(tests)) {
    by_test <- split(names(tests), factor(tests, levels = unique(tests)))
    note <- paste0("P-values: ", paste(sprintf("%s (%s)", names(by_test),
                                               vapply(by_test, paste, character(1), collapse = ", ")),
                                       collapse = "; "), ".")
  }
  header_rows <- which(header)
  level_rows <- which(!header)

  if (identical(output, "flextable")) {
    ft <- flextable::flextable(display)
    ft <- flextable::bold(ft, i = header_rows, j = 1, part = "body")
    if (length(level_rows)) ft <- flextable::padding(ft, i = level_rows, j = 1, padding.left = 15, part = "body")
    if (!is.null(title)) ft <- flextable::set_caption(ft, caption = title)
    if (!is.null(note)) ft <- flextable::add_footer_lines(ft, values = note)
    return(flextable::autofit(ft))
  }

  gt_tab <- gt::gt(display)
  gt_tab <- gt::tab_style(gt_tab, style = gt::cell_text(weight = "bold"),
                          locations = gt::cells_body(columns = "Characteristic", rows = header_rows))
  if (length(level_rows)) {
    gt_tab <- gt::tab_style(gt_tab, style = gt::cell_text(indent = gt::px(12)),
                            locations = gt::cells_body(columns = "Characteristic", rows = level_rows))
  }
  if (!is.null(title)) gt_tab <- gt::tab_header(gt_tab, title = title)
  if (!is.null(note)) gt_tab <- gt::tab_source_note(gt_tab, source_note = note)
  gt_tab
}


# Display tables for ModelMaker* regression outputs -------------------------------

# Render a ModelMakerMultiRD(), ModelMakerMulti(), modelMakerSequential*(), or
# run_auto_tableone_regressions() result as a publication-ready flextable or gt table.
modelOutputDisplay <- function(model_out,
                               output = c("flextable", "gt", "data.frame"),
                               title = NULL,
                               stages = c("all", "crude_and_full", "full"),
                               include_p = TRUE,
                               include_q = TRUE,
                               include_rd = TRUE,
                               include_rr = TRUE,
                               include_n = TRUE,
                               include_events = TRUE,
                               stage_labels = NULL,
                               p_digits = 3) {
  output <- match.arg(output)
  stages <- match.arg(stages)
  if (output %in% c("flextable", "gt") && !requireNamespace(output, quietly = TRUE)) {
    stop("Install the ", output, " package to use output = \"", output, "\".")
  }

  # Unwrap run_auto_tableone_regressions() output if supplied directly.
  if (is.list(model_out) && !is.data.frame(model_out) && !is.null(model_out$model_outputs)) {
    model_out <- model_out$model_outputs
  }

  df_or <- NULL
  plot_or <- NULL
  df_rd <- NULL
  df_rr <- NULL
  diag_df <- NULL

  if (is.data.frame(model_out)) {
    df_or <- model_out
  } else if (is.list(model_out)) {
    if (!is.null(model_out$df_output)) {
      df_or <- model_out$df_output
      plot_or <- model_out$plot_output
      df_rd <- model_out$df_output_RDs
      df_rr <- model_out$df_output_RRs
      diag_df <- model_out$diagnostics
    } else if (!is.null(model_out$model_df_ORs)) {
      df_or <- model_out$model_df_ORs
      df_rd <- model_out$model_df_RDs
      df_rr <- model_out$model_df_RRs
      diag_df <- model_out$diagnostics
      or_parts <- c(list(crude = model_out$crude_model_OR), model_out$adj_or_outputs)
      or_parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, or_parts)
      if (length(or_parts)) plot_or <- dplyr::bind_rows(or_parts)
    } else if (!is.null(model_out$model_df)) {
      df_or <- model_out$model_df
      diag_df <- model_out$diagnostics
      or_parts <- model_out$adj_model_outputs
      or_parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, or_parts)
      if (length(or_parts)) plot_or <- dplyr::bind_rows(or_parts)
    }
  }

  if (!is.data.frame(df_or) || !nrow(df_or) || !any(c("Category", "Level") %in% names(df_or))) {
    stop("`model_out` must be a ModelMakerMultiRD(), ModelMakerMulti(), or modelMakerSequential*() result.")
  }

  df_or <- as.data.frame(df_or, stringsAsFactors = FALSE)
  if ("Level" %in% names(df_or) && !"Category" %in% names(df_or)) {
    names(df_or)[names(df_or) == "Level"] <- "Category"
  }
  if (!"Variable" %in% names(df_or)) {
    var_fallback <- if (!is.null(plot_or) && "Variable" %in% names(plot_or)) plot_or$Variable[1] else "Predictor"
    df_or$Variable <- var_fallback
    df_or <- df_or[, c("Variable", setdiff(names(df_or), "Variable")), drop = FALSE]
  }

  if (!is.null(plot_or) && is.data.frame(plot_or) && nrow(plot_or)) {
    plot_or <- as.data.frame(plot_or, stringsAsFactors = FALSE)
    if ("Level" %in% names(plot_or) && !"Category" %in% names(plot_or)) {
      names(plot_or)[names(plot_or) == "Level"] <- "Category"
    }
    if (!"Variable" %in% names(plot_or)) plot_or$Variable <- df_or$Variable[1]
    if ("outcome_level" %in% names(plot_or) && !"Outcome" %in% names(plot_or)) {
      names(plot_or)[names(plot_or) == "outcome_level"] <- "Outcome"
    }
  } else {
    plot_or <- NULL
  }

  normalize_wide <- function(df) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    if ("Level" %in% names(df) && !"Category" %in% names(df)) {
      names(df)[names(df) == "Level"] <- "Category"
    }
    if ("outcome_level" %in% names(df) && !"Outcome" %in% names(df)) {
      names(df)[names(df) == "outcome_level"] <- "Outcome"
    }
    if (!"Variable" %in% names(df)) df$Variable <- df_or$Variable[1]
    df
  }
  df_rd <- if (isTRUE(include_rd)) normalize_wide(df_rd) else NULL
  df_rr <- if (isTRUE(include_rr)) normalize_wide(df_rr) else NULL

  meta_cols <- c("Variable", "Outcome", "outcome_level", "Category", "Level", "Nobs_in_model")
  stage_cols <- setdiff(names(df_or), meta_cols)
  if (!length(stage_cols)) {
    stop("No model stage columns found in `model_out`.")
  }

  # Detect effect measure and model family.
  measure <- NULL
  if (!is.null(plot_or) && "measure" %in% names(plot_or)) {
    m_vals <- stats::na.omit(unique(plot_or$measure))
    if (length(m_vals)) measure <- m_vals[1]
  }
  if (is.null(measure)) {
    suffix_hit <- regmatches(stage_cols, regexpr("_(OR|RR|IRR|HR|Beta|Coef|Ratio)$", stage_cols))
    if (length(suffix_hit)) measure <- sub("^_", "", suffix_hit[1]) else measure <- "OR"
  }

  family <- NULL
  if (!is.null(plot_or) && "model_family" %in% names(plot_or)) {
    f_vals <- stats::na.omit(unique(plot_or$model_family))
    if (length(f_vals)) family <- f_vals[1]
  } else if (!is.null(diag_df) && "model_family" %in% names(diag_df)) {
    f_vals <- stats::na.omit(unique(diag_df$model_family))
    if (length(f_vals)) family <- f_vals[1]
  }

  stage_stem <- function(col) {
    sub("_(OR|RR|IRR|HR|Beta|RD|Ratio|Coef)$", "", col)
  }
  # Pick the fullest stage column: when a covariate is also in independent_variables,
  # its reduced stage column (e.g. adj_sex_OR) is appended after adj_age_years+sex_OR.
  full_stage_col <- function(cols) {
    stems <- vapply(cols, stage_stem, character(1))
    n_adj <- vapply(stems, function(s) {
      if (s %in% c("crude", "crude_mod")) return(0L)
      if (grepl("^adj_", s)) return(length(strsplit(sub("^adj_", "", s), "\\+")[[1]]))
      1L
    }, integer(1))
    max_idx <- which(n_adj == max(n_adj))
    cols[max_idx[length(max_idx)]]
  }

  if (identical(stages, "crude_and_full") && length(stage_cols) > 2L) {
    stage_cols <- unique(c(stage_cols[1], full_stage_col(stage_cols)))
  } else if (identical(stages, "full") && length(stage_cols) > 1L) {
    stage_cols <- full_stage_col(stage_cols)
  }

  default_stage_label <- function(col) {
    stem <- stage_stem(col)
    if (identical(stem, "crude_mod") || identical(stem, "crude")) return("Crude")
    if (grepl("^adj_", stem)) {
      inner <- sub("^adj_", "", stem)
      if (identical(inner, "full")) return("Adjusted")
      return(paste0("Adjusted (", gsub("\\+", " + ", inner), ")"))
    }
    if (grepl("^plus_", stem)) {
      inner <- sub("^plus_", "", stem)
      return(paste0("+ ", gsub("_", " ", inner)))
    }
    stem
  }

  s_labels <- vapply(stage_cols, default_stage_label, character(1), USE.NAMES = TRUE)
  if (!is.null(stage_labels)) {
    if (!is.null(names(stage_labels))) {
      default_lbls <- s_labels
      for (i in seq_along(stage_labels)) {
        nm <- names(stage_labels)[i]
        val <- as.character(stage_labels[[i]])
        for (sc in names(s_labels)) {
          st <- stage_stem(sc)
          is_crude <- st %in% c("crude", "crude_mod")
          is_full <- identical(sc, stage_cols[length(stage_cols)]) && !is_crude
          if (identical(nm, sc) || identical(nm, st) ||
              identical(tolower(nm), tolower(default_lbls[[sc]])) ||
              (identical(tolower(nm), "crude") && is_crude) ||
              (identical(tolower(nm), "full") && is_full)) {
            s_labels[[sc]] <- val
          }
        }
      }
    } else if (length(stage_labels) == length(s_labels)) {
      s_labels[] <- as.character(stage_labels)
    }
  }

  fixed_p <- function(x) {
    ifelse(is.na(x), "",
           ifelse(x < 10^-p_digits,
                  paste0("<", formatC(10^-p_digits, format = "f", digits = p_digits)),
                  formatC(x, format = "f", digits = p_digits)))
  }

  row_key <- function(df) {
    out_col <- if ("Outcome" %in% names(df)) as.character(df$Outcome) else rep("", nrow(df))
    paste(as.character(df$Variable), out_col, as.character(df$Category), sep = "\r")
  }
  df_or$.key <- row_key(df_or)
  if (!is.null(df_rd)) df_rd$.key <- row_key(df_rd)
  if (!is.null(df_rr)) df_rr$.key <- row_key(df_rr)
  if (!is.null(plot_or)) {
    plot_or$.key <- row_key(plot_or)
    plot_or$.stem <- if ("wide_col" %in% names(plot_or)) stage_stem(plot_or$wide_col) else ""
  }

  # Extract per-variable N and (for Cox) Events.
  var_n <- list()
  var_events <- list()
  for (v in unique(df_or$Variable)) {
    n_val <- NA_integer_
    if ("Nobs_in_model" %in% names(df_or)) {
      nv <- stats::na.omit(unique(df_or$Nobs_in_model[df_or$Variable == v]))
      if (length(nv)) n_val <- nv[1]
    }
    if (is.na(n_val) && !is.null(plot_or) && "Nobs_in_model" %in% names(plot_or)) {
      nv <- stats::na.omit(plot_or$Nobs_in_model[plot_or$Variable == v])
      if (length(nv)) n_val <- max(nv)
    }
    if (is.na(n_val) && !is.null(diag_df) && all(c("Variable", "nobs") %in% names(diag_df))) {
      nv <- stats::na.omit(diag_df$nobs[diag_df$Variable == v])
      if (length(nv)) n_val <- max(nv)
    }
    var_n[[v]] <- if (is.na(n_val)) "" else formatC(as.integer(n_val), format = "d", big.mark = ",")

    ev_val <- NA_integer_
    if (!is.null(diag_df) && all(c("Variable", "events") %in% names(diag_df))) {
      ev <- stats::na.omit(diag_df$events[diag_df$Variable == v])
      if (length(ev)) ev_val <- max(ev)
    }
    var_events[[v]] <- if (is.na(ev_val)) "" else formatC(as.integer(ev_val), format = "d", big.mark = ",")
  }

  has_n <- isTRUE(include_n) && any(nzchar(unlist(var_n)))
  has_events <- isTRUE(include_events) && identical(family, "cox") && any(nzchar(unlist(var_events)))
  has_p <- isTRUE(include_p) && !is.null(plot_or) && "P_value" %in% names(plot_or)
  has_q <- isTRUE(include_q) && !is.null(plot_or) && "Q_value" %in% names(plot_or)

  # Build the flat display data.frame with header rows and indented level rows.
  rows_out <- list()
  is_header_vec <- logical()

  make_blank_row <- function() {
    r <- list(Characteristic = "")
    if (has_n) r[["N"]] <- ""
    if (has_events) r[["Events"]] <- ""
    for (sc in stage_cols) {
      lbl <- s_labels[[sc]]
      r[[paste0(lbl, " ", measure, " (95% CI)")]] <- ""
      if (has_p) r[[paste0(lbl, " P-value")]] <- ""
      if (has_q) r[[paste0(lbl, " Q-value")]] <- ""
      stem <- stage_stem(sc)
      if (!is.null(df_rd) && paste0(stem, "_RD") %in% names(df_rd)) {
        r[[paste0(lbl, " RD (95% CI)")]] <- ""
      }
      if (!is.null(df_rr) && paste0(stem, "_RR") %in% names(df_rr)) {
        r[[paste0(lbl, " Marginal RR (95% CI)")]] <- ""
      }
    }
    r
  }

  fill_stage_cells <- function(r, sub_row) {
    k <- sub_row$.key[1]
    for (sc in stage_cols) {
      lbl <- s_labels[[sc]]
      stem <- stage_stem(sc)
      val <- as.character(sub_row[[sc]][1])
      if (is.na(val)) val <- "-"
      r[[paste0(lbl, " ", measure, " (95% CI)")]] <- val

      if (has_p || has_q) {
        p_hit <- plot_or[plot_or$.key == k & (plot_or$wide_col == sc | plot_or$.stem == stem), , drop = FALSE]
        if (has_p) {
          r[[paste0(lbl, " P-value")]] <- if (nrow(p_hit)) fixed_p(p_hit$P_value[1]) else ""
        }
        if (has_q) {
          r[[paste0(lbl, " Q-value")]] <- if (nrow(p_hit)) fixed_p(p_hit$Q_value[1]) else ""
        }
      }
      if (!is.null(df_rd) && paste0(stem, "_RD") %in% names(df_rd)) {
        rd_col <- paste0(stem, "_RD")
        rd_hit <- df_rd[df_rd$.key == k, , drop = FALSE]
        rd_val <- if (nrow(rd_hit)) as.character(rd_hit[[rd_col]][1]) else "-"
        if (is.na(rd_val)) rd_val <- "-"
        r[[paste0(lbl, " RD (95% CI)")]] <- rd_val
      }
      if (!is.null(df_rr) && paste0(stem, "_RR") %in% names(df_rr)) {
        rr_col <- paste0(stem, "_RR")
        rr_hit <- df_rr[df_rr$.key == k, , drop = FALSE]
        rr_val <- if (nrow(rr_hit)) as.character(rr_hit[[rr_col]][1]) else "-"
        if (is.na(rr_val)) rr_val <- "-"
        r[[paste0(lbl, " Marginal RR (95% CI)")]] <- rr_val
      }
    }
    r
  }

  for (v in unique(df_or$Variable)) {
    v_rows <- df_or[df_or$Variable == v, , drop = FALSE]
    is_single_continuous <- nrow(v_rows) == 1L &&
      !grepl("\\[reference\\]$", v_rows$Category[1]) &&
      !("Outcome" %in% names(v_rows) && !is.na(v_rows$Outcome[1]))

    if (is_single_continuous) {
      r <- make_blank_row()
      r$Characteristic <- v
      if (has_n) r[["N"]] <- var_n[[v]]
      if (has_events) r[["Events"]] <- var_events[[v]]
      r <- fill_stage_cells(r, v_rows[1, , drop = FALSE])
      rows_out[[length(rows_out) + 1L]] <- as.data.frame(r, check.names = FALSE, stringsAsFactors = FALSE)
      is_header_vec <- c(is_header_vec, TRUE)
    } else {
      hdr <- make_blank_row()
      hdr$Characteristic <- v
      if (has_n) hdr[["N"]] <- var_n[[v]]
      if (has_events) hdr[["Events"]] <- var_events[[v]]
      rows_out[[length(rows_out) + 1L]] <- as.data.frame(hdr, check.names = FALSE, stringsAsFactors = FALSE)
      is_header_vec <- c(is_header_vec, TRUE)

      for (j in seq_len(nrow(v_rows))) {
        lvl_row <- v_rows[j, , drop = FALSE]
        r <- make_blank_row()
        cat_label <- as.character(lvl_row$Category[1])
        if ("Outcome" %in% names(lvl_row) && !is.na(lvl_row$Outcome[1]) && nzchar(lvl_row$Outcome[1])) {
          cat_label <- paste0(lvl_row$Outcome[1], ": ", cat_label)
        }
        r$Characteristic <- cat_label
        r <- fill_stage_cells(r, lvl_row)
        rows_out[[length(rows_out) + 1L]] <- as.data.frame(r, check.names = FALSE, stringsAsFactors = FALSE)
        is_header_vec <- c(is_header_vec, FALSE)
      }
    }
  }

  display <- do.call(rbind, rows_out)
  rownames(display) <- NULL

  # Compose footnote / source note.
  notes <- character()
  if (!is.null(family) && family %in% .react_families && !identical(family, "auto")) {
    spec_info <- .react_family_spec(family)
    fam_label <- paste0(toupper(substring(spec_info$label, 1, 1)), substring(spec_info$label, 2))
    notes <- c(notes, sprintf("%s models reporting %s (95%% CI).", fam_label, measure))
  } else {
    notes <- c(notes, sprintf("Models reporting %s (95%% CI).", measure))
  }
  if (!is.null(df_rd) || !is.null(df_rr)) {
    marg_names <- c(if (!is.null(df_rd)) "RD", if (!is.null(df_rr)) "marginal RR")
    notes <- c(notes, sprintf("Marginal %s estimated by g-computation.", paste(marg_names, collapse = " and ")))
  }
  if (has_q) {
    notes <- c(notes, "Q-values adjusted for multiple testing within each adjustment stage.")
  }
  note <- if (length(notes)) paste(notes, collapse = " ") else NULL

  header_rows <- which(is_header_vec)
  level_rows <- which(!is_header_vec)

  if (identical(output, "data.frame")) {
    attr(display, "header_rows") <- header_rows
    attr(display, "level_rows") <- level_rows
    attr(display, "title") <- title
    attr(display, "note") <- note
    return(display)
  }

  if (identical(output, "flextable")) {
    ft <- flextable::flextable(display)
    if (length(header_rows)) ft <- flextable::bold(ft, i = header_rows, j = 1, part = "body")
    if (length(level_rows)) ft <- flextable::padding(ft, i = level_rows, j = 1, padding.left = 15, part = "body")
    if (!is.null(title)) ft <- flextable::set_caption(ft, caption = title)
    if (!is.null(note)) ft <- flextable::add_footer_lines(ft, values = note)
    return(flextable::autofit(ft))
  }

  gt_tab <- gt::gt(display)
  if (length(header_rows)) {
    gt_tab <- gt::tab_style(gt_tab, style = gt::cell_text(weight = "bold"),
                            locations = gt::cells_body(columns = "Characteristic", rows = header_rows))
  }
  if (length(level_rows)) {
    gt_tab <- gt::tab_style(gt_tab, style = gt::cell_text(indent = gt::px(12)),
                            locations = gt::cells_body(columns = "Characteristic", rows = level_rows))
  }
  if (!is.null(title)) gt_tab <- gt::tab_header(gt_tab, title = title)
  if (!is.null(note)) gt_tab <- gt::tab_source_note(gt_tab, source_note = note)
  gt_tab
}
