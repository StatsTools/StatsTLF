#' Run contents from caller_contents file
#'
#' @param package An object with class ContentPackage.
#' @param pkg_name A character with the package name.
#'
#' @return A package with content added.
#' @export
#'
#' @examples
#' x <- run_caller_contents(package_init, 'name of the package')
run_caller_contents <- function(package, pkg_name) {
  # Validation Step -------------------------------------------------------------
  stopifnot("`package`must be a ContentPackage object." = class(package) == 'ContentPackage')

  stopifnot(
    "`pkg_name` must be provided." = !is.na(pkg_name),
    "`pkg_name` must be a character." = is.character(pkg_name),
    "`pkg_name` cannot be an array." = length(pkg_name) == 1
  )
  # -----------------------------------------------------------------------------

  path <- here::here('03_Algorithm')
  file_path <- paste0(path, '\\', pkg_name, '\\caller_contents.R')

  source(file_path, local = TRUE)

  backbones <- source_backbones(pkg_name)

  package <- caller_contents(package, backbones)

  return(package)
}
