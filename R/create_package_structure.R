#' Create package structure folder
#'
#' @param pkg_name A character to specify the name of the package.
#'
#' @return A path to the package structure folder
#' @export
#'
#' @examples
#' create_package_structure('Teste')
create_package_structure <- function(pkg_name) {
 path <- here::here('03_Algorithm')
 path <- paste0(path, '\\', pkg_name)

 stopifnot("Package name folder already exists in `path`." = !dir.exists(path))

 dir.create(path)

 template_dir <- system.file("", package = "StatsTLF")
 template_path <- paste0(template_dir, '\\caller.R')

 file.copy(template_path, paste0(path, '\\caller.R'))
 file.rename(paste0(path, '\\caller.R'), paste0(path, "\\caller_", pkg_name, '.R'))

 template_path2 <- paste0(template_dir, '\\caller_contents.R')
 file.copy(template_path2, paste0(path, '\\caller_contents.R'))

 dir.create(paste0(path, '\\backbones'))

 caller <- readLines(paste0(path, "\\caller_", pkg_name, '.R'))
 caller[7] <- paste0("backbones <- source_backbones('", pkg_name, "')")
 caller[16] <- paste0("package <- run_caller_contents(package_init, backbones, '", pkg_name, "')")
 caller[20] <- paste0(" report_name = 'Results_", pkg_name, "',")
 write(caller, file = paste0(path, "\\caller_", pkg_name, '.R'))

 file.edit(paste0(path, "\\caller_", pkg_name, '.R'))

 return(path)
}
