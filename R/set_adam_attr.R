#' Set ADaM attributes for a given dataset and metadata file
#'
#' @param dataset A tibble with the dataset to be compared.
#' @param path A character to specify the path of the metadata file in .xlsx format based on package template.
#' @param name A character to specify the name of dataset in the metadata file.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' x <- set_adam_attr(dataset, 'ADSL.xlsx')
set_adam_attr <- function(dataset, path, name) {
  template <- create_adam_dataset(path)

  match_idx <- which(purrr::map_chr(template, ~ attr(.x, "name")) == name)
  stopifnot('Dataset name not found in metadata.' = length(match_idx) == 1)
  template <- template[[match_idx]]

  cols_dataset <- names(dataset)
  cols_template <- names(template)

  common_cols <- intersect(cols_template, cols_dataset)

  for (col in common_cols) {
    attr(dataset[[col]], "label") <- attr(template[[col]], "label")
    attr(dataset[[col]], "data_type") <- attr(template[[col]], "data_type")
    attr(dataset[[col]], "origin") <- attr(template[[col]], "origin")
    attr(dataset[[col]], "closed") <- attr(template[[col]], "closed")
    attr(dataset[[col]], "levels") <- attr(template[[col]], "levels")
  }

  attr(dataset, "path") <- attr(template, "path")
  attr(dataset, "name") <- attr(template, "name")

  return(dataset)
}
