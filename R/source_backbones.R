#' Source Backbones from Backbones Folder Within a Package
#'
#' This function sources all backbones from the backbones folder within a specified package, returning the loaded backbones.
#'
#' @param pkg_name A character string specifying the name of the package.
#'
#' @return A list of sourced backbones.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Source backbones from the package named 'teste2'
#' source_backbones('teste2')
#' }
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
 sapply(files.sources, source, local = environment())

 backbones <- mget(ls(pattern = "table_|figure_|listing_"))

 lapply(seq_along(backbones), function(i) {
   bb <- backbones[[i]]

   cat('  \u2500 Content Backbone:\n')
   cat('    \u2500 Name:', ifelse(is.na(names(backbones)[i]), "", names(backbones)[i]), '\n')
   cat('    \u2500 Title:', ifelse(is.na(bb@title), "", bb@title), '\n')
   cat('    \u2500 Type:', ifelse(is.na(bb@type), "", bb@type), '\n')
   cat('  Done!\n')
 })

 return(backbones)
}
