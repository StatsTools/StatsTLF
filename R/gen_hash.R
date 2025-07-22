#' Generate a SHA1 Hash from a Log File
#'
#' This function generates a SHA1 hash from the contents of a specified file.
#'
#' @param path A character string specifying the path to the file. The path must be a single character string and the file must exist.
#'
#' @return A character string representing the SHA1 hash of the file contents.
#' @export
#'
#' @examples
#' # Example usage:
#' \dontrun{
#' x <- gen_hash('./teste/table_teste_backbone.R')
#' }
gen_hash <- function(path) {

  # Validation Step -------------------------------------------------------------
  stopifnot(
    "`path` must be a character." = is.character(path),
    "`path` cannot be an array." = length(path) == 1
  )

  stopifnot("File not found." = file.exists(path))
  # -----------------------------------------------------------------------------

  true_hash <- digest::sha1(readLines(path, warn = FALSE))

  return(true_hash)
}
