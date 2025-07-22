#' Create Content Backbone
#'
#' This function creates a content backbone with a specified title, type, and content-generating function. The type determines the expected return of the function: 'T' for a flextable, 'F' for a ggplot, and 'L' for a tibble.
#'
#' @param title A character string specifying the title of the content.
#' @param type A character string specifying the type of the content. Use 'T', 'F', or 'L'.
#' @param fun A function that creates the content. The function must return a flextable for 'T', a ggplot for 'F', and a tibble for 'L'.
#'
#' @return A content backbone object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a content backbone for a table
#' x <- create_content_backbone(
#'  'analysis of dataset',
#'  'T',
#'  function(dataset) {
#'   dataset |> flextable::flextable()
#'  }
#' )
#' }
create_content_backbone <- function(title, type, fun) {

 # Validation Step -------------------------------------------------------------
 stopifnot(
  "`title` must be provided." = !is.na(title),
  "`title` must be a character." = is.character(title),
  "`title` cannot be an array." = length(title) == 1
 )

 stopifnot(
  "`type` must be provided." = !is.na(type),
  "`type` must be a character." = is.character(type),
  "`type` cannot be an array." = length(type) == 1,
  "`type` must be 'T', 'F' or 'L'." = type %in% c('T', 'F', 'L')
 )

 stopifnot("`fun` must be a function." = is.function(fun))
 # -----------------------------------------------------------------------------

 content_backbone <- new('ContentBackbone', title = title, type = type, fun = fun)

 return(content_backbone)
}
