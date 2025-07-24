#' Add Content to a Content Package
#'
#' This function adds content to an existing content package. It requires that the package and content are of specific classes.
#'
#' @param package An object of class `ContentPackage`. This represents the package to which content will be added.
#' @param content An object of class `Content`. This represents the content to be added to the package.
#'
#' @return An object of class `ContentPackage` with the new content added.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a content package and add content to it
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
#' )
#' }
add_to_package <- function(package, content) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`package`must be a ContentPackage object." = class(package) == 'ContentPackage')
 stopifnot("`content`must be a Content object." = class(content) == 'Content')
 # -----------------------------------------------------------------------------

 return(add_to_package_method(x = package, content = content))
}
