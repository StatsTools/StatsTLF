#' Source backbones from backbones folder within a package
#'
#' @param pkg_name A character to specify the name of the package.
#'
#' @return A path to the backbone structure folder
#' @export
#'
#' @examples
#' source_backbones('teste2')
source_backbones <- function(pkg_name) {
 stopifnot(
  "`pkg_name` must be provided." = !is.na(pkg_name),
  "`pkg_name` must be a character." = is.character(pkg_name),
  "`pkg_name` cannot be an array." = length(pkg_name) == 1
 )

 path <- here::here('03_Algorithm')
 path <- paste0(path, '\\', pkg_name)

 stopifnot("Package folder doesn't exist." = dir.exists(path))

 backbone_path <- paste0(path, '\\backbones')

 stopifnot("Backbone folder doesn't exist." = dir.exists(backbone_path))

 files.sources = list.files(backbone_path, full.names = TRUE)
 sapply(files.sources, source)

 return(backbone_path)
}
