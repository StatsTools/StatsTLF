#' Validate a hash SHA1 file given in log file.
#'
#' @param hash A character to specify the hash of the file.
#' @param path A character to specify the path of the file.
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' x <- validate_hash('abcdkfj28747', './teste/table_teste_backbone.R')
validate_hash <- function(hash, path) {

  # Validation Step -------------------------------------------------------------
  stopifnot(
    "`hash` must be a character." = is.character(hash),
    "`hash` cannot be an array." = length(hash) == 1
  )

  stopifnot(
    "`path` must be a character." = is.character(path),
    "`path` cannot be an array." = length(path) == 1
  )

  stopifnot("File not found." = file.exists(path))
  # -----------------------------------------------------------------------------

  true_hash <- digest::sha1(readLines(path, warn = FALSE))

  cat('- File  Hash:', true_hash, '\n')
  cat('- Given Hash:', hash, '\n')
  cat('- Validation: ', true_hash == hash)


  return(invisible(true_hash == hash))
}
