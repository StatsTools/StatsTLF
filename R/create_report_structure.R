#' Create Report Structure Folder
#'
#' This function creates a report structure folder for a specified package name within the '03_Algorithm' directory. It sets up necessary template files and directories for the report.
#'
#' @param pkg_name A character string specifying the name of the package.
#'
#' @return The path to the created report structure folder.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a report structure for 'Teste'
#' create_report_structure('Teste')
#' }
create_report_structure <- function(pkg_name) {
 path <- here::here('03_Algorithm')
 path <- paste0(path, '\\', pkg_name)

 stopifnot("Package name folder already exists in `03_Algorithm`." = !dir.exists(path))

 dir.create(path)

 template_dir <- system.file("", package = "StatsTLF")

 template_path <- paste0(template_dir, '\\caller_report.R')
 file.copy(template_path, paste0(path, '\\caller_report.R'))
 file.rename(paste0(path, '\\caller_report.R'), paste0(path, "\\caller_", pkg_name, '.R'))

 template_path2 <- paste0(template_dir, '\\caller_contents.R')
 file.copy(template_path2, paste0(path, '\\caller_contents.R'))

 dir.create(paste0(path, '\\backbones'))

 caller <- readLines(paste0(path, "\\caller_", pkg_name, '.R'))
 caller[4] <- paste0(" name = '", pkg_name, "',")

 write(caller, file = paste0(path, "\\caller_", pkg_name, '.R'))

 caller <- readLines(paste0(path, "\\caller_contents.R"))
 caller[3] <- paste0("  backbones <- StatsTLF::source_backbones('", pkg_name, "')")
 write(caller, file = paste0(path, "\\caller_contents.R"))

 file.edit(paste0(path, "\\caller_", pkg_name, '.R'))
 file.edit(paste0(path, "\\caller_contents.R"))

 return(path)
}
