#' Create project with template
#'
#' @param proj_name A character to specify the name of the project.
#' @param path A character to specify the path to create the project.
#' @param open A boolean to specify whether the project should be opened.
#' @param rstudio A boolean to specify whether the rstudio project file should be created.
#'
#' @return A path to the project.
#' @export
#'
#' @examples
#' create_project('Teste')
create_project <- function(proj_name, path = NA_character_, open = TRUE, rstudio = TRUE) {
 if (is.na(path)) {
    path <- tcltk::tk_choose.dir()
 }

 stopifnot("`path` must be provided." = !is.na(path))

 path <- paste0(path, '\\', proj_name)

 stopifnot("Project name folder already exists in `path`." = !dir.exists(path))

 usethis::create_project(path = path, open = open, rstudio = open)

 dir.create(paste0(path, '\\00_Template'))
 dir.create(paste0(path, '\\01_Documents'))
 dir.create(paste0(path, '\\02_ExternalDatasets'))
 dir.create(paste0(path, '\\03_Algorithm'))
 dir.create(paste0(path, '\\04_Datasets'))
 dir.create(paste0(path, '\\05_Results'))

 template_dir <- system.file("", package = "StatsTLF")
 template_name <- '\\template_PT-BR.docx'
 template_name2 <- '\\template_EN-US.docx'
 template_path <- paste0(template_dir, template_name)
 template_path2 <- paste0(template_dir, template_name2)

 file.copy(template_path, paste0(path, '\\00_Template', template_name))
 file.copy(template_path2, paste0(path, '\\00_Template', template_name2))

 rprofile_path <- paste0(template_dir, '\\Rprofile.R')

 file.copy(rprofile_path, paste0(path, '\\Rprofile.R'))
 file.rename(paste0(path, '\\Rprofile.R'), paste0(path, '\\.Rprofile'))

 unlink(paste0(path, '\\R'), recursive = TRUE)

 return(path)
}
