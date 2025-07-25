#' Validate ADaM Dataset Against Metadata File
#'
#' This function validates a dataset by comparing it against a metadata file, checking for structural mismatches and attribute inconsistencies. It reports any issues found and suggests using `set_adam_attr()` to fix missing metadata paths.
#'
#' @param dataset A tibble containing the dataset to be validated.
#'
#' @return TRUE if the dataset is validated successfully; FALSE if there are mismatches.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Validate a dataset against the 'ADSL.xlsx' metadata file
#' x <- validate_adam_dataset(dataset)
#' }
validate_adam_dataset <- function(dataset) {

  # 0. Check if path metadata is missing
  if (is.null(attr(dataset, "path"))) {
    stop('`Path` is missing. Please, use `set_adam_attr()` to fix this.')
  }

  path <- attr(dataset, 'path')

  template <- create_adam_dataset(path)

  target_name <- attr(dataset, "name")
  match_idx <- which(purrr::map_chr(template, ~ attr(.x, "name")) == target_name)
  stopifnot('Dataset name not found in metadata.' = length(match_idx) == 1)
  template <- template[[match_idx]]

  issues <- list()
  issues_attr <- list()

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

    # Compare classes
    if (!identical(class(col_dataset), class(col_template))) {
      issues[[paste0("Class mismatch: ", col)]] <- list(
        expected = class(col_template),
        found = class(col_dataset)
      )
    }

    # If factor, compare levels
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

    # # Check additional attributes
    attrs_template <- attributes(col_template)
    attrs_dataset <- attributes(col_dataset)

    for (attr_name in names(attrs_template)) {
      if (attr_name %in% c("levels", "class")) next

      val_template <- attrs_template[[attr_name]]
      val_dataset <- attrs_dataset[[attr_name]]

      if (!identical(val_template, val_dataset)) {
        issues_attr[[col]] <- c(issues_attr[[col]], attr_name)
      }
    }
  }

  if (length(issues) == 0 & length(issues_attr) == 0) {
    message("✅ No structural mismatches found between dataset and metadata")
    error_trig <- TRUE
  } else {
    message("⚠️ Structural mismatches found:\n")
    for (issue in names(issues)) {
      cat("•", issue, ":\n")
      print(issues[[issue]])
      cat("\n")
    }
    for (issue_attr in names(issues_attr)) {
      cat("• Attributes mismatch:", issue_attr, ":", paste(issues_attr[[issue_attr]], collapse = ", "), "\n")
      cat("\n")
    }
    error_trig <- FALSE
  }

  return(invisible(error_trig))
}
