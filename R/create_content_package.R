#' Create Content Package
#'
#' This function creates a content package with specified numbering start points, separators for titles and subtitles, and language settings for the titles.
#'
#' @param name A character string with the name of the package. Must be unique inside 03_Algorithm.
#' @param start_number A list of 3 elements: 'T', 'F', and 'L', each associated with an integer value indicating the starting number for tables, figures, and listings, respectively.
#' @param sep_subtitle A character string specifying the separator between the title and subtitle. Use 'newline' to insert a line break.
#' @param sep_population A character string specifying the separator between the title or subtitle and the population. Use 'newline' to insert a line break.
#' @param language A character string specifying the language of the titles. Use 'PT-BR' for Portuguese or 'EN-US' for English.
#'
#' @return A content package object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a content package with specific settings
#' x <- create_content_package(
#'  name = 'Teste',
#'  start_number = list('T' = 10L, 'F' = 15L, 'L' = 1L),
#'  sep_subtitle = ', ',
#'  sep_population = ': ',
#'  language = 'PT-BR'
#' )
#' }
create_content_package <- function(name, start_number = list('T' = 1L, 'F' = 1L, 'L' = 1L), sep_subtitle = 'newline', sep_population = 'newline', language = 'PT-BR') {

 # Validation Step -------------------------------------------------------------
 stopifnot(
   "`name` must be provided." = !is.na(name),
   "`name` must be a character." = is.character(name),
   "`name` cannot be an array." = length(name) == 1
 )

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

 content_package <- new(
  'ContentPackage',
  name = name,
  content_list = list(),
  start_number = start_number,
  sep_subtitle = sep_subtitle,
  sep_population = sep_population,
  language = language
 )

 return(content_package)
}
