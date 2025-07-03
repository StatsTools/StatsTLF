#' Create backbone structure folder
#'
#' @param backbone_name A character to specify the name of the backbone.
#' @param backbone_type A character to specify the type of the backbone. Please use 'T', 'F' or 'L'.
#' @param pkg_name A character to specify the name of the package.
#'
#' @return A path to the backbone structure folder
#' @export
#'
#' @examples
#' create_backbone_structure('teste3', 'T', 'teste2')
create_backbone_structure <- function(backbone_name, backbone_type, pkg_name) {
 stopifnot(
  "`backbone_name` must be provided." = !is.na(backbone_name),
  "`backbone_name` must be a character." = is.character(backbone_name),
  "`backbone_name` cannot be an array." = length(backbone_name) == 1
 )

 stopifnot(
  "`backbone_type` must be provided." = !is.na(backbone_type),
  "`backbone_type` must be a character." = is.character(backbone_type),
  "`backbone_type` cannot be an array." = length(backbone_type) == 1
 )

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

 stopifnot(
  "`backbone_type` must be provided." = !is.na(backbone_type),
  "`backbone_type` must be a character." = is.character(backbone_type),
  "`backbone_type` cannot be an array." = length(backbone_type) == 1,
  "`backbone_type` must be 'T', 'F' or 'L'." = backbone_type %in% c('T', 'F', 'L')
 )

 if (backbone_type == 'T') {
  backbone_type_text <- 'table'
  backbone_return_text <- 'tab'
 } else if (backbone_type == 'F') {
  backbone_type_text <- 'figure'
  backbone_return_text <- 'fig'
 } else if (backbone_type == 'L') {
  backbone_type_text <- 'listing'
  backbone_return_text <- 'listing'
 }

 backbone_text <- paste0(backbone_type_text, '_', backbone_name, '_backbone')

 template_dir <- system.file("", package = "StatsTLF")
 template_path <- paste0(template_dir, '\\backbone.R')

 file.copy(template_path, paste0(backbone_path, '\\backbone.R'))
 file.rename(paste0(backbone_path, '\\backbone.R'), paste0(backbone_path, "\\", backbone_text, '.R'))

 backbone <- readLines(paste0(backbone_path, "\\", backbone_text, '.R'))
 backbone[27] <- paste0(backbone_text, ' <- StatsTLF::create_content_backbone(title = "", type = "', backbone_type, '", fun = function(dataset, ...) {')
 backbone[67] <- paste0(' return(', backbone_return_text, ')')

 if (backbone_type == 'T') {
   backbone[1] <- "#' Title: Create Table – XXXXXXXXXX"
   backbone[15] <- "#'   - A flextable object containing the summary table."
   backbone[25] <- "#' @return flextable object"
 } else if (backbone_type == 'F') {
   backbone[1] <- "#' Title: Create Figure – XXXXXXXXXX"
   backbone[15] <- "#'   - A ggplot object containing the summary figure."
   backbone[25] <- "#' @return ggplot object"
 } else if (backbone_type == 'L') {
   backbone[1] <- "#' Title: Create Listing – XXXXXXXXXX"
   backbone[15] <- "#'   - A tibble object containing the summary listing."
   backbone[25] <- "#' @return tibble object"
 }

 write(backbone, file = paste0(backbone_path, "\\", backbone_text, '.R'))

 file.edit(paste0(backbone_path, "\\", backbone_text, '.R'))

 return(backbone_path)
}
