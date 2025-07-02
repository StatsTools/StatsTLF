#' Create a report with comparisons against given dataset and metadata file and fix attributes issues
#'
#' @param dataset A tibble with the dataset to be compared.
#' @param path A character to specify the path of the metadata file in .xlsx format based on package template.
#'
#' @return A tibble with the dataset given but with attributes fixed.
#' @export
#'
#' @examples
#' x <- validate_adam_dataset(dataset, 'ADSL.xlsx')
validate_adam_dataset <- function(dataset, path) {
  template <- create_adam_dataset(path)

  issues <- list()

  # 1. Check if columns order are as specified
  cols_dataset <- names(dataset)
  cols_template <- names(template)

  missing_cols <- setdiff(cols_template, cols_dataset)
  extra_cols   <- setdiff(cols_dataset, cols_template)

  if (length(missing_cols) > 0) {
    issues[["Missing columns"]] <- missing_cols
  }
  if (length(extra_cols) > 0) {
    issues[["Extra columns"]] <- extra_cols
  }

  # 2. Compare each comum column
  common_cols <- intersect(cols_template, cols_dataset)

  for (col in common_cols) {
    col_dataset <- dataset[[col]]
    col_template <- template[[col]]

    # Comparar classe
    if (!identical(class(col_dataset), class(col_template))) {
      issues[[paste0("Class mismatch: ", col)]] <- list(
        expected = class(col_template),
        found = class(col_dataset)
      )
    }

    # If facotr, compare levels
    if ("factor" %in% class(col_template)) {
      levels_dataset <- levels(col_dataset)
      levels_template <- levels(col_template)
      if (!identical(levels_dataset, levels_template)) {
        issues[[paste0("Levels mismatch: ", col)]] <- list(
          expected = levels_template,
          found = levels_dataset
        )
      }
    }

    attr(dataset[[col]], "group") <- attr(template[[col]], "group")
    attr(dataset[[col]], "name") <- attr(template[[col]], "name")
    attr(dataset[[col]], "label") <- attr(template[[col]], "label")
    attr(dataset[[col]], "dtype") <- attr(template[[col]], "dtype")
    attr(dataset[[col]], "type")  <- attr(template[[col]], "type")
    attr(dataset[[col]], "source")  <- attr(template[[col]], "source")
    attr(dataset[[col]], "related_vars")  <- attr(template[[col]], "related_vars")
    attr(dataset[[col]], "closed") <- attr(template[[col]], "closed")
    attr(dataset[[col]], "factor_levels") <- attr(template[[col]], "factor_levels")
    attr(dataset[[col]], "factor_labels") <- attr(template[[col]], "factor_labels")
    attr(dataset[[col]], "rule") <- attr(template[[col]], "rule")
    attr(dataset[[col]], "use_levels") <- attr(template[[col]], "use_levels")

    # # Check aditional attributes
    # attrs_template <- attributes(col_template)
    # attrs_dataset <- attributes(col_dataset)
    #
    # for (attr_name in names(attrs_template)) {
    #   val_template <- attrs_template[[attr_name]]
    #   val_dataset <- attrs_dataset[[attr_name]]
    #   if (!identical(val_template, val_dataset)) {
    #     issues[[paste0("Attribute mismatch: ", col, " [", attr_name, "]")]] <- list(
    #       expected = val_template,
    #       found = val_dataset
    #     )
    #   }
    # }
  }

  if (length(issues) == 0) {
    message("✅ No structural mismatches found between dataset and metadata")
    return(invisible(NULL))
  } else {
    message("⚠️ Structural mismatches found:\n")
    for (issue in names(issues)) {
      cat("•", issue, ":\n")
      print(issues[[issue]])
      cat("\n")
    }
    return(invisible(dataset))
  }
}
