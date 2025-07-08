#' Create package structure folder
#'
#' @param pkg_name A character to specify the name of the package.
#' @param dataset A boolean to specify if the package is a dataset package, not a report.
#'
#' @return A path to the package structure folder
#' @export
#'
#' @examples
#' create_package_structure('Teste')
create_package_structure <- function(pkg_name, dataset = FALSE) {
 path <- here::here('03_Algorithm')
 path <- paste0(path, '\\', pkg_name)

 stopifnot("Package name folder already exists in `03_Algorithm`." = !dir.exists(path))

 stopifnot(
   "`dataset` must be provided." = !is.na(dataset),
   "`dataset` must be a boolean." = is.logical(dataset),
   "`dataset` cannot be an array." = length(dataset) == 1
 )

 dir.create(path)

 template_dir <- system.file("", package = "StatsTLF")

 template_path <- paste0(template_dir, '\\caller.R')
 file.copy(template_path, paste0(path, '\\caller.R'))
 file.rename(paste0(path, '\\caller.R'), paste0(path, "\\caller_", pkg_name, '.R'))

 template_path2 <- paste0(template_dir, '\\caller_contents.R')
 file.copy(template_path2, paste0(path, '\\caller_contents.R'))

 dir.create(paste0(path, '\\backbones'))

 caller <- readLines(paste0(path, "\\caller_", pkg_name, '.R'))
 caller[10] <- paste0("package <- StatsTLF::run_caller_contents(package_init, '", pkg_name, "')")

 if (dataset) {
   caller[14] <- paste0(" report_name = 'Datasets - ", pkg_name, "', # Please, do not change this if going to use `build_submission()`.")
   caller[17] <- " dataset = TRUE,"
   caller <- caller[-c(15, 16)]
   caller <- caller[-c(4:8)]
   caller[3] <- "package_init <- StatsTLF::create_content_package()"
 } else {
   caller[14] <- paste0(" report_name = 'SAR - ", pkg_name, "', # Please, do not change this if going to use `build_submission()`.")
 }
 write(caller, file = paste0(path, "\\caller_", pkg_name, '.R'))

 caller <- readLines(paste0(path, "\\caller_contents.R"))
 caller[3] <- paste0("  # backbones <- StatsTLF::source_backbones('", pkg_name, "')")
 write(caller, file = paste0(path, "\\caller_contents.R"))

 file.edit(paste0(path, "\\caller_", pkg_name, '.R'))
 file.edit(paste0(path, "\\caller_contents.R"))

 return(path)
}
