#' Create content package
#'
#' @param start_number A list of 3 elements 'T', 'F' and 'L', respectively, with 3 integer values associated.
#' @param sep_subtitle A character to specify the separator of title and subtitle. Use 'newline' to break a line.
#' @param sep_population A character to specify the separator of title or subtitle and population Use 'newline' to break a line.
#' @param language A character to specify the language of the titles. Use 'PT-BR' or 'EN-US'.
#'
#' @return A content package.
#' @export
#'
#' @examples
#' x <- create_content_package(
#'  start_number = list('T' = 10L, 'F' = 15L, 'L' = 1L),
#'  sep_subtitle = ', ',
#'  sep_population = ': ',
#'  language = 'PT-BR'
#' )
create_content_package <- function(start_number = list('T' = 1L, 'F' = 1L, 'L' = 1L), sep_subtitle = 'newline', sep_population = 'newline', language = 'PT-BR') {

 # Validation Step -------------------------------------------------------------
 stopifnot(
  "`start_number` must be provided." = !is.na(start_number),
  "`start_number` must be a list." = is.list(start_number),
  "`start_number` must have 3 elements with names: 'T', 'F' and 'L', respectively." = length(start_number) == 3,
  "`start_number` must have 3 elements with names: 'T', 'F' and 'L', respectively." = names(start_number) == c('T', 'F', 'L'),
  "`start_number` values must be integers." = is.integer(unlist(start_number))
 )

 stopifnot(
  "`sep_subtitle` must be provided." = !is.na(sep_subtitle),
  "`sep_subtitle` must be a character." = is.character(sep_subtitle),
  "`sep_subtitle` cannot be an array." = length(sep_subtitle) == 1
 )

 stopifnot(
  "`sep_population` must be provided." = !is.na(sep_population),
  "`sep_population` must be a character." = is.character(sep_population),
  "`sep_population` cannot be an array." = length(sep_population) == 1
 )

 stopifnot(
  "`language` must be provided." = !is.na(language),
  "`language` must be a character." = is.character(language),
  "`language` cannot be an array." = length(language) == 1,
  "`language` must be 'PT-BR' or 'EN-US'." = language %in% c('PT-BR', 'EN-US')
 )
 # -----------------------------------------------------------------------------

 cat('  \u2500 Creating content package ...')

 content_package <- new(
  'ContentPackage',
  content_list = list(),
  start_number = start_number,
  sep_subtitle = sep_subtitle,
  sep_population = sep_population,
  language = language
 )

 cat(' Done!\n')

 return(content_package)
}
