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
