#' Create an analysis dataset based on metadata file
#'
#' @param path A character to specify the path of the metadata file in .xlsx format based on package template.
#' @param use_labels A boolean to specify if factor labels should be used instead of factor levels.
#'
#' @return A tibble based on metadata file with zero rows
#' @export
#'
#' @examples
#' x <- create_adam_dataset('ADSL.xlsx')
create_adam_dataset <- function(path, use_labels = FALSE) {

  split_outside_quotes <- function(x) {
    if (is.na(x) || x == "-" || x == "") return(x)
    # parts <- stringr::str_split(x, ", (?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")[[1]]
    parts <- stringi::stri_split_regex(x,
                     pattern = ',(?=(?:[^"]*"[^"]*")*[^"]*$)',
                     omit_empty = TRUE
    )[[1]]
    cleaned <- parts |>
      stringr::str_replace_all('^\\s*\"', '') |>
      stringr::str_replace_all('\"\\s*$', '') |>
      # stringr::str_replace_all('\\\\\"', '') |>
      stringr::str_trim()
    return(cleaned)
  }

  # Validation Step -------------------------------------------------------------
  stopifnot(
    "`path` must be a character." = is.character(path),
    "`path` cannot be an array." = length(path) == 1
  )

  stopifnot("File not found." = file.exists(path))

  stopifnot(
    "`use_labels` must be provided." = !is.na(use_labels),
    "`use_labels` must be a boolean." = is.logical(use_labels),
    "`use_labels` cannot be an array." = length(use_labels) == 1
  )

 adam_metadata <- readxl::read_xlsx(path, col_types = 'text')

 required_cols <- c('Variable Group', 'Variable', 'Variable Label', 'Data Type', 'Type', 'Source', 'Related Variables', 'Closed', 'Level Options', 'Label Options', 'Rule')
 stopifnot('The metadata is missing one or more required columns: `Variable Group`, `Variable`, `Variable Label`, `Data Type`, `Type`, `Source`, `Related Variables`, `Closed`, `Level Options`, `Label Options` or `Rule`' = all(required_cols %in% names(adam_metadata)))

 stopifnot('The `Variable` column contains missing or empty values.' = all(!is.na(adam_metadata[["Variable"]]) & adam_metadata[["Variable"]] != ""))
 stopifnot('The `Variable Label` column contains missing or empty values.' = all(!is.na(adam_metadata[["Variable Label"]]) & adam_metadata[["Variable Label"]] != ""))
 stopifnot('The `Data Type` column contains missing or empty values.' = all(!is.na(adam_metadata[["Data Type"]]) & adam_metadata[["Data Type"]] != ""))
 stopifnot('The `Type` column contains missing or empty values.' = all(!is.na(adam_metadata[["Type"]]) & adam_metadata[["Type"]] != ""))
 stopifnot('The `Source` column contains missing or empty values.' = all(!is.na(adam_metadata[["Source"]]) & adam_metadata[["Source"]] != ""))
 stopifnot('The `Related Variables` column contains missing or empty values.' = all(!is.na(adam_metadata[["Related Variables"]]) & adam_metadata[["Related Variables"]] != ""))
 stopifnot('The `Closed` column contains missing or empty values.' = all(!is.na(adam_metadata[["Closed"]]) & adam_metadata[["Closed"]] != ""))
 stopifnot('The `Rule` column contains missing or empty values.' = all(!is.na(adam_metadata[["Rule"]]) & adam_metadata[["Rule"]] != ""))

 stopifnot('The `Data Type` column contains invalid values. Allowed values are "Character", "Numeric" or "Date"' = all(adam_metadata[["Data Type"]] %in% c("Character", "Numeric", "Date")))
 stopifnot('The `Type` column contains invalid values. Allowed values are "Fixed", "External" or "Derived"' = all(adam_metadata[["Type"]] %in% c("Fixed", "External", "Derived")))
 stopifnot('The `Closed` column contains invalid values. Allowed values are "N" or "Y"' = all(adam_metadata[["Closed"]] %in% c("N", "Y")))

 stopifnot('The `Variable` column must be unique. Duplicate rows found.' = !any(duplicated(adam_metadata[c("Variable")])))
 stopifnot('The `Variable Label` column must be unique. Duplicate rows found.' = !any(duplicated(adam_metadata[c("Variable Label")])))

 idx_Y <- which(adam_metadata[["Closed"]] == "Y")
 idx_N <- which(adam_metadata[["Closed"]] == "N")
 idx_fixed <- which(adam_metadata[["Type"]] == "Fixed")

 adam_metadata <- adam_metadata[, c('Variable Group', 'Variable', 'Variable Label', 'Data Type', 'Type', 'Source', 'Related Variables', 'Closed', 'Level Options', 'Label Options', 'Rule')]
 adam_metadata <- adam_metadata |>
   dplyr::mutate(
     `Level Options` = purrr::map(`Level Options`, split_outside_quotes),
     `Label Options` = purrr::map(`Label Options`, split_outside_quotes)
   )

 stopifnot('The `Level Options` and `Label Options` columns must be lists after parsing.' = is.list(adam_metadata[["Level Options"]]) && is.list(adam_metadata[["Label Options"]]))

 # When Closed == "Y"
 stopifnot('`Level Options` contains missing values for rows where `Closed` is "Y"' = all(!is.na(adam_metadata[["Level Options"]][idx_Y])))
 stopifnot('`Label Options` contains missing values for rows where `Closed` is "Y"' = all(!is.na(adam_metadata[["Label Options"]][idx_Y])))

 stopifnot('`Level Options` contains empty strings for rows where `Closed` is "Y"' = all(adam_metadata[["Level Options"]][idx_Y] != ""))
 stopifnot('`Label Options` contains empty strings for rows where `Closed` is "Y"' = all(adam_metadata[["Label Options"]][idx_Y] != ""))

 stopifnot('`Level Options` must not be "-" when `Closed` is "Y"' = all(adam_metadata[["Level Options"]][idx_Y] != "-"))
 stopifnot('`Label Options` must not be "-" when `Closed` is "Y"' = all(adam_metadata[["Label Options"]][idx_Y] != "-"))

 stopifnot('The number of `Level Options` must match the number of `Label Options` for each row where `Closed` is "Y"' = all(mapply(length, adam_metadata[["Level Options"]][idx_Y]) ==
                 mapply(length, adam_metadata[["Label Options"]][idx_Y])))

 # When Closed == "N"
 stopifnot('`Level Options` must be exactly "-" when `Closed` is "N"' = all(adam_metadata[["Level Options"]][idx_N] == "-"))
 stopifnot('`Label Options` must be exactly "-" when `Closed` is "N"' = all(adam_metadata[["Label Options"]][idx_N] == "-"))

 stopifnot('`Source` must be "-" when `Type` is "Fixed"' = all(adam_metadata[["Source"]][idx_fixed] == "-"))
 stopifnot('`Source` must be "-" when `Type` is "Fixed"' = all(adam_metadata[["Related Variables"]][idx_fixed] == "-"))

 # -----------------------------------------------------------------------------

 create_column <- function(group, name, label, dtype, type, source, related_vars, closed, levels, labels, rule, use_levels = use_labels) {
   col <- switch(
     dtype,
     "Character" = character(),
     "Numeric" = numeric(),
     "Date" = as.Date(NA),
     stop(paste("Data Type not supported:", dtype))
   )

   if (closed == "Y") {
     if (use_labels) {
       col <- factor(col, levels = labels)
     } else {
       col <- factor(col, levels = levels)
     }
   }

   attr(col, "group") <- group
   attr(col, "name") <- name
   attr(col, "label") <- label
   attr(col, "dtype") <- dtype
   attr(col, "type")  <- type
   attr(col, "source")  <- source
   attr(col, "related_vars")  <- related_vars
   attr(col, "closed")  <- closed
   attr(col, "factor_levels")  <- levels
   attr(col, "factor_labels")  <- labels
   attr(col, "rule")  <- rule
   attr(col, "use_levels")  <- use_levels
   return(rlang::set_names(list(col), name))
 }

 adam_map <- purrr::pmap(
   list(
     group = adam_metadata$`Variable Group`,
     name = adam_metadata$Variable,
     label = adam_metadata$`Variable Label`,
     dtype = adam_metadata$`Data Type`,
     type = adam_metadata$Type,
     source = adam_metadata$Source,
     related_vars = adam_metadata$`Related Variables`,
     closed = adam_metadata$Closed,
     levels = adam_metadata$`Level Options`,
     labels = adam_metadata$`Label Options`,
     rule = adam_metadata$Rule
   ),
   create_column
 )

 adam_dataset <- tibble::tibble(!!!purrr::list_flatten(adam_map))

 attr(adam_dataset, "path") <- path

 return(adam_dataset)
}
