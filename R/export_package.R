#' Export package
#'
#' @param package An object with class ContentPackage.
#' @param report_name A character name to the exported file.
#' @param template_name A character with the name to the template report file in folder ./00_Template.
#' @param supp A boolean to specify if 'S' should be added before content number in export.
#'
#' @return A path to the report file.
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
export_package <- function(package, report_name, template_name = "template_PT-BR.docx", supp = FALSE) {

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
   "Folder './05_Results' doesn't exist." = dir.exists(here::here('05_Results'))
 )
 # -----------------------------------------------------------------------------

 return(export_package_method(x = package, report_name = report_name, template_name = template_name, supp = supp))
}
