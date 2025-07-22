#' Create Project with Template
#'
#' This function creates a new project with a specified template structure. It sets up directories and template files, and optionally opens the project in RStudio.
#'
#' @param proj_name A character string specifying the name of the project.
#' @param path A character string specifying the path where the project should be created. If not provided, a directory chooser will be opened.
#' @param open A boolean indicating whether the project should be opened after creation. Defaults to TRUE.
#' @param rstudio A boolean indicating whether an RStudio project file should be created. Defaults to TRUE.
#'
#' @return The path to the created project.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a project named 'Teste'
#' create_project('Teste')
#' }
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
 template_name3 <- '\\template_spec.xlsx'
 template_name4 <- '\\snippets.txt'
 template_path <- paste0(template_dir, template_name)
 template_path2 <- paste0(template_dir, template_name2)
 template_path3 <- paste0(template_dir, template_name3)
 template_path4 <- paste0(template_dir, template_name4)

 file.copy(template_path, paste0(path, '\\00_Template', template_name))
 file.copy(template_path2, paste0(path, '\\00_Template', template_name2))
 file.copy(template_path3, paste0(path, '\\01_Documents', template_name3))
 file.copy(template_path4, paste0(path, '\\01_Documents', template_name4))

 rprofile_path <- paste0(template_dir, '\\Rprofile.R')

 file.copy(rprofile_path, paste0(path, '\\Rprofile.R'))
 file.rename(paste0(path, '\\Rprofile.R'), paste0(path, '\\.Rprofile'))

 unlink(paste0(path, '\\R'), recursive = TRUE)

 return(path)
}
