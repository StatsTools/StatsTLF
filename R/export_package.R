#' Export package
#'
#' @param package An object with class ContentPackage.
#' @param report_name A character name to the exported file.
#' @param template_name A character with the name to the template report file in folder ./00_Template.
#' @param supp A boolean to specify if 'S' should be added before content number in export.
#' @param dataset A boolean to specify if export is a dataset type and not a report.
#' @param add_toc A boolean to specify if table of contents should be added to the report.
#'
#' @return A path to the exported package.
#' @export
#'
#' @examples
#' x <- create_content_package() |>
#'  add_to_package(
#'   create_content(
#'    create_content_backbone(
#'     'analysis of dataset',
#'     'T',
#'     function(dataset) {
#'      dataset |>
#'       flextable::flextable()
#'     }
#'    ),
#'    dataset = tibble::tibble(x = c(1, 2, 3))
#'   )
#' ) |>
#' export_package('teste')
export_package <- function(package, report_name, template_name = "template_PT-BR.docx", supp = FALSE, dataset = FALSE, add_toc = TRUE) {

 # Validation Step -------------------------------------------------------------
 stopifnot(
  "`report_name` must be provided." = !is.na(report_name),
  "`report_name` must be a character." = is.character(report_name),
  "`report_name` cannot be an array." = length(report_name) == 1,
  "File from `report_name` cannot have extension." = tools::file_ext(report_name) == ''
 )

 stopifnot(
  "`template_name` must be provided." = !is.na(template_name),
  "`template_name` must be a character." = is.character(template_name),
  "`template_name` cannot be an array." = length(template_name) == 1
 )

 stopifnot("Template file in `template_name` doesn't exist." = file.exists(paste0(here::here('00_Template'), '\\', template_name)))

 stopifnot(
   "`supp` must be provided." = !is.na(supp),
   "`supp` must be a boolean." = is.logical(supp),
   "`supp` cannot be an array." = length(supp) == 1
 )

 stopifnot(
   "`dataset` must be provided." = !is.na(dataset),
   "`dataset` must be a boolean." = is.logical(dataset),
   "`dataset` cannot be an array." = length(dataset) == 1
 )

 if (dataset) {
   stopifnot(
     "Folder './04_Datasets' doesn't exist." = dir.exists(here::here('04_Datasets'))
   )
 } else {
   stopifnot(
     "Folder './05_Results' doesn't exist." = dir.exists(here::here('05_Results'))
   )
 }

 stopifnot("Package must contain at least one content to be exported." = length(package@content_list) > 0)

 stopifnot(
   "`add_toc` must be provided." = !is.na(add_toc),
   "`add_toc` must be a boolean." = is.logical(add_toc),
   "`add_toc` cannot be an array." = length(add_toc) == 1
 )

 # -----------------------------------------------------------------------------

 return(export_package_method(x = package, report_name = report_name, template_name = template_name, supp = supp, dataset = dataset, add_toc = add_toc))
}
