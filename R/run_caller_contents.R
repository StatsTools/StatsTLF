#' Run Contents from Caller Contents File
#'
#' This function executes contents from a specified caller_contents file, adding them to a given content package.
#'
#' @param package An object of class `ContentPackage`.
#'
#' @return The content package with contents added.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Run contents from the caller_contents file for a specified package
#' x <- run_caller_contents(package_init, 'name of the package')
#' }
run_caller_contents <- function(package) {
  # Validation Step -------------------------------------------------------------
  stopifnot("`package`must be a ContentPackage object." = class(package) == 'ContentPackage')
  # -----------------------------------------------------------------------------

  pkg_name <- package@name

  path <- here::here('03_Algorithm')
  file_path <- paste0(path, '\\', pkg_name, '\\caller_contents.R')

  source(file_path, local = TRUE)

  package <- caller_contents(package)

  return(package)
}
