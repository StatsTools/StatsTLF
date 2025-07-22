#' Validate a SHA1 Hash for a Given File
#'
#' This function validates a given SHA1 hash against the actual hash of a specified file, verifying the integrity of the file.
#'
#' @param hash A character string specifying the expected SHA1 hash of the file.
#' @param path A character string specifying the path to the file.
#'
#' @return TRUE if the hash matches the file's actual hash; FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Validate the hash of a file
#' x <- validate_hash('abcdkfj28747', './teste/table_teste_backbone.R')
#' }
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
