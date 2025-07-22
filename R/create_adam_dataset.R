#' Create an analysis dataset based on metadata file
#'
#' @param path A character to specify the path of the metadata file in .xlsx format based on package template.
#'
#' @return A tibble based on metadata file with zero rows
#' @export
#'
#' @examples
#' x <- create_adam_dataset('ADSL.xlsx')
create_adam_dataset <- function(path) {

  # Validation Step -------------------------------------------------------------
  stopifnot(
    "`path` must be a character." = is.character(path),
    "`path` cannot be an array." = length(path) == 1
  )

  stopifnot("File not found." = file.exists(path))

 datasets_spec <- readxl::read_xlsx(path, col_types = 'text', sheet = 'Datasets')
 variables_spec <- readxl::read_xlsx(path, col_types = 'text', sheet = 'Variables')
 codelists_spec <- readxl::read_xlsx(path, col_types = 'text', sheet = 'Codelists')

 # -----------------------------------------------------------------------------

 names(datasets_spec) <- tolower(names(datasets_spec))
 names(variables_spec) <- tolower(names(variables_spec))
 names(codelists_spec) <- tolower(names(codelists_spec))

 codelists <- dplyr::filter(codelists_spec, !is.na(id)) |>
   dplyr::group_by(id) |>
   dplyr::summarise(values = list(term), .groups = "drop") |>
   tibble::deframe()

 create_column <- function(data_type, label, codelist_name, origin) {
   convert_levels_to_type <- function(levels_vec, data_type) {
     switch(tolower(data_type),
            "integer" = as.integer(levels_vec),
            "float" = as.numeric(levels_vec),
            "datetime" = as.POSIXct(levels_vec),
            "date" = as.Date(levels_vec),
            "time" = hms::as_hms(levels_vec),
            "partialdate" = as.character(levels_vec),
            "partialtime" = as.character(levels_vec),
            "partialdatetime" = as.character(levels_vec),
            "text" = as.character(levels_vec),
            as.character(levels_vec)
     )
   }

   col <- switch(
     tolower(data_type),
     "text" = character(),
     "integer" = integer(),
     "float" = numeric(),
     "datetime" = as.POSIXct(character()),
     "date" = as.Date(character()),
     "time" = hms::as_hms(character()),
     "partialdate" = character(),
     "partialtime" = character(),
     "partialdatetime" = character(),
     character()
   )

   if (!is.null(codelist_name) && codelist_name %in% names(codelists)) {
     levels_converted <- convert_levels_to_type(codelists[[codelist_name]], data_type)
     col <- factor(col, levels = levels_converted)
   }

   attr(col, "label") <- label
   attr(col, "data_type") <- data_type
   attr(col, "origin") <- origin
   attr(col, "closed") <- !is.null(codelist_name) && codelist_name %in% names(codelists)

   if (!is.null(attr(col, "tzone"))) attr(col, "tzone") <- NULL

   col
 }

 datasets <- list()

 for (dataset_name in datasets_spec$dataset) {
   var_spec <- dplyr::filter(variables_spec, tolower(dataset) == tolower(dataset_name))

   col_list <- purrr::map2(
     .x = var_spec$`data type`,
     .y = var_spec$variable,
     .f = function(dtype, varname) {
       label <- var_spec$label[var_spec$variable == varname]
       codelist <- var_spec$codelist[var_spec$variable == varname]
       origin <- var_spec$origin[var_spec$variable == varname]
       col <- create_column(dtype, label, codelist, origin)
       rlang::set_names(list(col), varname)
     }
   ) |> purrr::flatten()

   datasets[[dataset_name]] <- tibble::as_tibble(col_list)
   attr(datasets[[dataset_name]], "path") <- path
   attr(datasets[[dataset_name]], "name") <- dataset_name
 }

 return(datasets)
}
