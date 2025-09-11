#' Create a mockup dataset using numeric patterns
#'
#' @description
#' Generates a dataset where numeric values are replaced with masked representations
#' based on specified patterns for each column. Useful for creating anonymized examples or mock datasets.
#'
#' @param data A data.frame or tibble.
#' @param patterns A named list of patterns, where each name corresponds to a column in the dataset.
#'   Patterns can be a single pattern (applied to all rows) or a vector with the same length as the column.
#'
#' @return A data.frame with the same structure as `data` but with numeric values masked according to the patterns.
#' @examples
#' df <- tibble::tibble(
#'   stat = c("10 (50.0%)", "23 (45.0%)", "50 (90.1%)"),
#'   mean_sd = c("23.5 (2.1)", "10.4 (3.2)", "12.0 (4.1)")
#' )
#' mockup(df, patterns = list(
#'   stat = "i(2)+f(2,1,.)",
#'   mean_sd = "f(2,1,.)+f(1,1,.)"
#' ))
#' @export
mockup <- function(data, patterns) {
  stopifnot(is.data.frame(data))
  stopifnot(is.list(patterns))

  out <- data
  col_names <- names(patterns)

  for (col in col_names) {
    if (!col %in% names(data)) {
      stop(sprintf("Column '%s' does not exist in the dataset.", col))
    }

    vec <- data[[col]]
    pat <- patterns[[col]]

    if (length(pat) == 1 || length(pat) == length(vec)) {
      out[[col]] <- mock_str(vec, pat)
    } else {
      stop(sprintf(
        "The pattern of column '%s' must have length 1 or match the number of rows.",
        col
      ))
    }
  }

  out
}
