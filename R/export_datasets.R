#' Export Dataset Package
#'
#' This function exports a dataset package to a specified file format.
#'
#' @param package An object of class `ContentPackage`.
#'
#' @return The path to the exported package.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create and export a content package
#' x <- create_content_package() |>
#'  add_to_package(
#'   create_content(
#'    create_content_backbone(
#'     'analysis of dataset',
#'     'T',
#'     function(dataset) {
#'      dataset |>
#'       flextable::flextable()
#'     }
#'    ),
#'    dataset = tibble::tibble(x = c(1, 2, 3))
#'   )
#' ) |>
#' export_package('teste')
#' }
export_datasets <- function(package) {

  # Validation Step -------------------------------------------------------------
  stopifnot("Package must contain at least one content to be exported." = length(package@content_list) > 0)

  stopifnot(
    "Folder './04_Datasets' doesn't exist." = dir.exists(here::here('04_Datasets'))
  )
  # -----------------------------------------------------------------------------

  return(export_datasets_method(x = package))
}
