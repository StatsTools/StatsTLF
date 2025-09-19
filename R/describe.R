#' Describe dataset or column
#'
#' @description
#' Prints a readable description of a dataset (data.frame/tibble)
#' or of a single column (vector), showing only attributes that are not NA.
#'
#' For datasets, common metadata such as path, name, label, structure, etc. are shown.
#' For columns, metadata such as order, label, data_type, codelist, origin,
#' and method details are shown. Value-level metadata is summarized if present.
#'
#' @param x A dataset or a column created by `create_adam_dataset`.
#'
#' @return Prints to console. Invisibly returns NULL.
#' @export
describe <- function (x)
{
  clean_str <- function(val) {
    if (is.null(val) || (length(val) == 1 && is.na(val)))
      return(NULL)
    gsub("\\\\\"", "\"", paste(val, collapse = ", "))
  }
  if (tibble::is_tibble(x)) {
    attrs <- list(path = attr(x, "path", exact = TRUE),
                  name = attr(x, "name", exact = TRUE), label = attr(x,
                                                                     "label", exact = TRUE), class = attr(x, "dataset_class",
                                                                                                          exact = TRUE), subclass = attr(x, "dataset_subclass",
                                                                                                                                         exact = TRUE), structure = attr(x, "dataset_structure",
                                                                                                                                                                         exact = TRUE), key_variables = attr(x, "key_variables",
                                                                                                                                                                                                             exact = TRUE), standard = attr(x, "standard",
                                                                                                                                                                                                                                            exact = TRUE), has_no_data = attr(x, "has_no_data",
                                                                                                                                                                                                                                                                              exact = TRUE), repeating = attr(x, "repeating",
                                                                                                                                                                                                                                                                                                              exact = TRUE), reference_data = attr(x, "reference_data",
                                                                                                                                                                                                                                                                                                                                                   exact = TRUE), comment = attr(x, "comment",
                                                                                                                                                                                                                                                                                                                                                                                 exact = TRUE))
    cli::cli_h1("📊 Dataset Description")
    cli::cli_h2("Basic information")
    for (nm in c("path", "name", "label", "class", "subclass",
                 "structure", "repeating", "key_variables", "standard",
                 "has_no_data", "reference_data", "comment")) {
      val <- clean_str(attrs[[nm]])
      if (!is.null(val))
        cli::cli_li(paste0("{.field ", nm, "}: ", val))
    }
    invisible(NULL)
  }
  else {
    nm <- deparse(substitute(x))
    attrs <- list(order = attr(x, "order", exact = TRUE),
                  dataset = attr(x, "dataset", exact = TRUE), variable = attr(x,
                                                                              "variable", exact = TRUE), label = attr(x, "label",
                                                                                                                      exact = TRUE), data_type = attr(x, "data_type",
                                                                                                                                                      exact = TRUE), length = attr(x, "length", exact = TRUE),
                  signif_dig = attr(x, "significant_digits", exact = TRUE),
                  format = attr(x, "format", exact = TRUE), mandatory = attr(x,
                                                                             "mandatory", exact = TRUE), closed = attr(x,
                                                                                                                       "closed", exact = TRUE), codelist = attr(x,
                                                                                                                                                                "codelist", exact = TRUE), origin = attr(x,
                                                                                                                                                                                                         "origin", exact = TRUE), source = attr(x, "source",
                                                                                                                                                                                                                                                exact = TRUE), predecessor = attr(x, "predecessor",
                                                                                                                                                                                                                                                                                  exact = TRUE), has_no_data = attr(x, "has_no_data",
                                                                                                                                                                                                                                                                                                                    exact = TRUE), comment = attr(x, "comment",
                                                                                                                                                                                                                                                                                                                                                  exact = TRUE))
    method_attrs <- list(name = attr(x, "method_name", exact = TRUE),
                         type = attr(x, "method_type", exact = TRUE), description = attr(x,
                                                                                         "method_description", exact = TRUE), context = attr(x,
                                                                                                                                             "method_expression_context", exact = TRUE),
                         code = attr(x, "method_expression_code", exact = TRUE))
    valuelevel_attrs <- list(where_clause = attr(x, "valuelevel_where_clause",
                                                 exact = TRUE), codelist = attr(x, "valuelevel_codelist",
                                                                                exact = TRUE), origin = attr(x, "valuelevel_origin",
                                                                                                             exact = TRUE), source = attr(x, "valuelevel_source",
                                                                                                                                          exact = TRUE), predecessor = attr(x, "valuelevel_predecessor",
                                                                                                                                                                            exact = TRUE), method_id = attr(x, "valuelevel_method_id",
                                                                                                                                                                                                            exact = TRUE), comment_id = attr(x, "valuelevel_comment_id",
                                                                                                                                                                                                                                             exact = TRUE))
    cli::cli_h1("🔹 Column Description")
    cli::cli_h2("Basic information")
    for (nm in c("order", "dataset", "variable", "label",
                 "data_type", "length", "signif_dig", "format", "mandatory",
                 "closed", "codelist", "origin", "source", "predecessor",
                 "has_no_data", "comment")) {
      val <- clean_str(attrs[[nm]])
      if (!is.null(val))
        cli::cli_li(paste0("{.field ", nm, "}: ", val))
    }
    if (!all(sapply(valuelevel_attrs, is.na))) {
      cli::cli_h2("Value Level information")
      for (nm in names(valuelevel_attrs)) {
        val <- clean_str(valuelevel_attrs[[nm]])
        if (!is.null(val))
          cli::cli_li(paste0("{.field ", nm, "}: ",
                             val))
      }
      cli::cli_alert_info("Value-level metadata available. Check spec.xlsx or define.xml for more information.")
    }
    if (!all(sapply(method_attrs, is.na))) {
      cli::cli_h2("Method information")
      for (nm in names(method_attrs)) {
        val <- clean_str(method_attrs[[nm]])
        if (!is.null(val)){


          if(nm == "description"){
            cli::cli_li(paste0("{.field ", nm, "}: "))
            cat(val)}

          if(nm != "description"){
            cli::cli_li(paste0("{.field ", nm, "}: ",
                               val))}

        }
      }
    }
    invisible(NULL)
  }
}
