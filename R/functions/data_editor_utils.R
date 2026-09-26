# =============================================================================
# DATA EDITOR HELPERS
# =============================================================================
# Pure helpers behind dataeditor_inline_server.R, testable without a session.

#' Apply one DT cell edit to the species-info frame
#'
#' DT sends the edited value as a string. Assigning it raw turned the whole
#' numeric column into character (one edit to meanB broke every estimator).
#' DT::coerceValue() converts it to the column's existing class instead; a
#' value that does not fit the column (text in a number column, an fg that is
#' not a canonical level) is rejected rather than written as NA.
#'
#' @param df The species-info data frame shown in the table.
#' @param row 1-based row index from `input$<id>_cell_edit$row`.
#' @param col Column index from `input$<id>_cell_edit$col`. The table renders
#'   with `rownames = TRUE`, so DT's column 0 is the row-name column and
#'   column c is `df[[c]]`.
#' @param value The edited value (character).
#' @return `df` with the cell updated; unchanged (with a warning) when the
#'   edit targets the row-name column, lies outside the frame, or cannot be
#'   coerced to the column's type.
#' @export
apply_cell_edit <- function(df, row, col, value) {
  row <- suppressWarnings(as.integer(row))
  col <- suppressWarnings(as.integer(col))
  if (length(row) != 1 || length(col) != 1 || is.na(row) || is.na(col) ||
        row < 1 || row > nrow(df) || col < 1 || col > ncol(df)) {
    warning(sprintf("[data editor] ignored edit outside the data (row %s, col %s)", row, col),
            call. = FALSE)
    return(df)
  }
  # An integer column (meanB read from an integer-valued CSV) would make
  # coerceValue() truncate "2.5" to 2; promote it to double so decimals survive.
  target <- df[[col]]
  if (is.integer(target)) target <- as.numeric(target)
  new_value <- suppressWarnings(DT::coerceValue(value, target))
  blank <- is.null(value) || is.na(value) || identical(trimws(as.character(value)), "")
  if (is.na(new_value) && !blank) {
    warning(sprintf("[data editor] '%s' is not a valid value for column '%s'; edit ignored",
                    value, names(df)[col]), call. = FALSE)
    return(df)
  }
  df[[col]] <- target
  df[row, col] <- new_value
  df
}

#' Name the required numeric info columns that are not numeric
#'
#' @param df Species-info data frame.
#' @param cols Columns that must be numeric.
#' @return Character vector of offending column names (empty when all good).
#' @export
non_numeric_info_columns <- function(df, cols = c("meanB", "bodymasses", "efficiencies")) {
  present <- intersect(cols, names(df))
  present[!vapply(present, function(col) is.numeric(df[[col]]), logical(1))]
}
