# Base-R stand-ins for the few tidyr and stringr calls the package used, which keeps its
# dependencies small enough to install from an offline bundle in the enclave.

# Wide format, as tidyr::pivot_wider() gives it: one row per combination of id_cols, in
# order of first appearance, and one column per names_from value, also in order of first
# appearance. With several values_from columns the new columns are named
# "<value>_<name>", with the names varying fastest.
.react_pivot_wider <- function(df, id_cols, names_from, values_from) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  key_part <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "\001"
    x
  }
  key <- do.call(paste, c(lapply(df[id_cols], key_part), sep = "\002"))
  first <- !duplicated(key)
  out <- df[first, id_cols, drop = FALSE]
  rownames(out) <- NULL
  row <- match(key, key[first])
  names_vals <- as.character(df[[names_from]])
  for (value in values_from) {
    for (name in unique(names_vals)) {
      hit <- names_vals %in% name
      col <- df[[value]][rep(NA_integer_, nrow(out))]
      col[row[hit]] <- df[[value]][hit]
      out[[if (length(values_from) > 1L) paste(value, name, sep = "_") else name]] <- col
    }
  }
  out
}

# Long format, as tidyr::pivot_longer() gives it: one row per input row and pivoted
# column, in that order, with the other columns repeated beside "name" and "value".
.react_pivot_longer <- function(df, cols) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  rows <- rep(seq_len(nrow(df)), each = length(cols))
  out <- df[rows, setdiff(names(df), cols), drop = FALSE]
  out$name <- rep(cols, times = nrow(df))
  values <- unlist(df[cols], use.names = FALSE)
  out$value <- values[as.vector(t(matrix(seq_along(values), nrow = nrow(df))))]
  rownames(out) <- NULL
  out
}

# The n-th space-separated word of each string, counting from the end when n is
# negative, as stringr::word() returns it.
.react_word <- function(x, n) {
  # The extra space keeps a trailing empty word, which strsplit() would otherwise drop.
  words <- strsplit(paste0(as.character(x), " "), " ", fixed = TRUE)
  out <- vapply(words, function(w) {
    i <- if (n < 0) length(w) + n + 1L else n
    if (i >= 1L && i <= length(w)) w[[i]] else NA_character_
  }, character(1))
  out[is.na(x)] <- NA_character_
  out
}
