#' Create content from content backbone
#'
#' @param content_backbone An object with class ContentBackbone.
#' @param dataset A tibble dataset to apply the content backbone.
#' @param subtitle A character to specify the content subtitle (optional).
#' @param population A character to specify the content population (optional).
#' @param section A character to specify the content section (optional).
#' @param fdim A list of 3 elements 'width', 'height' and 'dpi', respectively, with 3 numeric values associated. Only used for type = 'F'.
#' @param ... Extra arguments to pass to 'fun' in content backbone (optional).
#'
#' @return A content.
#' @export
#'
#' @examples
#' x <- create_content(
#'  create_content_backbone(
#'   'analysis of dataset',
#'   'T',
#'   function(dataset) {
#'    dataset |>
#'     flextable::flextable()
#'   }
#'  ),
#'  dataset = tibble::tibble(x = c(1, 2, 3))
#' )
create_content <- function(content_backbone, dataset, subtitle = NA_character_, population = NA_character_, section = NA_character_, fdim = list(width = 9, height = 5, dpi = 600), ...) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`content_backbone`must be a ContentBackbone object." = class(content_backbone) == 'ContentBackbone')

 stopifnot("`dataset` must be a tibble." = tibble::is_tibble(dataset))

 stopifnot(
  "`subtitle` must be a character." = is.character(subtitle),
  "`subtitle` cannot be an array." = length(subtitle) == 1
 )

 stopifnot(
  "`population` must be a character." = is.character(population),
  "`population` cannot be an array." = length(population) == 1
 )

 stopifnot(
  "`section` must be a character." = is.character(section),
  "`section` cannot be an array." = length(section) == 1
 )

 value <- tryCatch(content_backbone@fun(dataset, ...), error = function(e) stop(paste('Error applying the dataset to the content backbone. Please check `fun` code.\n', e)))

 stopifnot("Return of `fun` must be a flextable, tibble or ggplot object." = any(class(value) %in% c('flextable', 'gg', 'tbl_df', 'gtable')))

 if (content_backbone@type == 'T') stopifnot("`fun` must return a flextable for `type` = 'T'." = any(class(value) == 'flextable'))
 if (content_backbone@type == 'F') stopifnot("`fun` must return a ggplot for `type` = 'F'." = any(class(value) %in% c('gg', 'gtable')))
 if (content_backbone@type == 'L') stopifnot("`fun` must return a tibble for `type` = 'L'." = any(class(value) == 'tbl_df'))

 stopifnot(
   "`fdim` must be provided." = !is.na(fdim),
   "`fdim` must be a list." = is.list(fdim),
   "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = length(fdim) == 3,
   "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = names(fdim) == c('width', 'height', 'dpi'),
   "`fdim` values must be numeric." = is.numeric(unlist(fdim))
 )
 # -----------------------------------------------------------------------------

 return(create_content_method(x = content_backbone, value = value, subtitle = subtitle, population = population, section = section, fdim = fdim, ...))
}
