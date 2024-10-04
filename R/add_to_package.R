#' Add content to package
#'
#' @param package An object with class ContentPackage.
#' @param content An object with class Content.
#'
#' @return A package with content added.
#' @export
#'
#' @examples
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
add_to_package <- function(package, content) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`package`must be a ContentPackage object." = class(package) == 'ContentPackage')
 stopifnot("`content`must be a Content object." = class(content) == 'Content')
 # -----------------------------------------------------------------------------

 return(add_to_package_method(x = package, content = content))
}
