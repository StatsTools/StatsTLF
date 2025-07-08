#' Set ADaM attributes for a given dataset and metadata file
#'
#' @param dataset A tibble with the dataset to be compared.
#' @param path A character to specify the path of the metadata file in .xlsx format based on package template.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' x <- set_adam_attr(dataset, 'ADSL.xlsx')
set_adam_attr <- function(dataset, path) {
  template <- create_adam_dataset(path)

  cols_dataset <- names(dataset)
  cols_template <- names(template)

  common_cols <- intersect(cols_template, cols_dataset)

  for (col in common_cols) {
    attr(dataset[[col]], "group") <- attr(template[[col]], "group")
    attr(dataset[[col]], "name") <- attr(template[[col]], "name")
    attr(dataset[[col]], "label") <- attr(template[[col]], "label")
    attr(dataset[[col]], "dtype") <- attr(template[[col]], "dtype")
    attr(dataset[[col]], "type")  <- attr(template[[col]], "type")
    attr(dataset[[col]], "source")  <- attr(template[[col]], "source")
    attr(dataset[[col]], "related_vars")  <- attr(template[[col]], "related_vars")
    attr(dataset[[col]], "closed") <- attr(template[[col]], "closed")
    attr(dataset[[col]], "factor_levels") <- attr(template[[col]], "factor_levels")
    attr(dataset[[col]], "factor_labels") <- attr(template[[col]], "factor_labels")
    attr(dataset[[col]], "rule") <- attr(template[[col]], "rule")
    attr(dataset[[col]], "use_levels") <- attr(template[[col]], "use_levels")
  }

  attr(dataset, "path") <- attr(template, "path")

  return(dataset)
}
