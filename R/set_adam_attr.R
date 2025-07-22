#' Set ADaM Attributes for a Given Dataset and Metadata File
#'
#' This function sets ADaM attributes for a given dataset based on a specified metadata file. It ensures that the dataset's columns have the correct labels, data types, origins, and levels as defined in the metadata.
#'
#' @param dataset A tibble containing the dataset to be updated with ADaM attributes.
#' @param path A character string specifying the path to the metadata file in .xlsx format, based on the package template.
#' @param name A character string specifying the name of the dataset in the metadata file.
#'
#' @return The dataset tibble with updated ADaM attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Set ADaM attributes for a dataset using metadata from 'ADSL.xlsx'
#' x <- set_adam_attr(dataset, 'ADSL.xlsx', 'ADSL')
#' }
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
