#' Create content backbone
#'
#' @param title A character to specify the title of the content.
#' @param type A character to specify the type of the content. Use 'T', 'F' or 'L'.
#' @param fun A function that creates the content. The return of the function must be a flextable for 'T', ggplot for 'F' and tibble for 'L'.
#'
#' @return A content backbone.
#' @export
#'
#' @examples
#' x <- create_content_backbone(
#'  'analysis of dataset',
#'  'T',
#'  function(dataset) {
#'   dataset |> flextable::flextable()
#'  }
#' )
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

 cat('  Creating content backbone:', type, '->', title, '...')

 content_backbone <- new('ContentBackbone', title = title, type = type, fun = fun)

 cat(' Done!\n')

 return(content_backbone)
}
