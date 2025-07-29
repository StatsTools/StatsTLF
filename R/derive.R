#' Derive Variable in an ADaM Dataset with Complex Conditions
#'
#' This function derives a variable in a primary dataset (e.g., ADSL) based on complex conditions involving multiple auxiliary datasets. It supports multiple conditional assignments and a default value.
#'
#' @param .data A data frame (primary dataset) where the variable will be derived.
#' @param var Symbol. Name of the variable to be derived in `.data`.
#' @param from Named list of data frames. Auxiliary datasets used in conditional expressions. Each dataset must be named, e.g., `list(ADPD = adpd_df, ADIS = adis_df)`.
#' @param cases List of lists. Each inner list must have two elements: `condition` and `value`.
#'   - `condition`: an expression evaluated within the environment containing `.data` and `from` datasets. Use `derive_expr()` to encapsulate the condition.
#'   - `value`: expression or constant to assign to `var` when `condition` is TRUE.
#' The conditions are evaluated in order; the first matching case has precedence.
#' @param by Symbol. The subject identifier variable name common to `.data` and all datasets in `from`. Used to match rows across datasets (default is `"USUBJID"`).
#' @param default Value or expression assigned to `var` for rows where no `cases` condition matches. Defaults to `NA`.
#'
#' @return The original dataset `.data` with the variable `var` derived accordingly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Derive a variable in ADSL based on conditions from ADPD
#' adsl <- derive(adsl, var = FASFL,
#'   from = list(adpd = adpd_df),
#'   cases = list(
#'     list(
#'       condition = derive_expr(ADSL$RANDFL == "Y" & ADPD$PARAMCD == "HI - A H5N8" & ADPD$AVAL != "Vazio"),
#'       value = derive_expr("Y")
#'     )
#'   ),
#'   default = derive_expr("N")
#' )
#' }
derive <- function(.data, var, from = list(), cases = list(), by = USUBJID, default = NA) {
  stopifnot("Validation error: The dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(.data))

  var <- rlang::ensym(var)
  by_str <- purrr::map_chr(by, rlang::as_string)
  var_str <- rlang::as_string(var)

  stopifnot(
    "`var` must be a single name or symbol." = rlang::is_symbol(var) || (is.character(var) && length(var) == 1),
    "`var` must exist in .data." = var_str %in% colnames(.data)
  )

  stopifnot(
    "`from` must be a list." = is.list(from),
    "Each element of `from` must be named." = length(from) == 0 || all(nzchar(names(from)))
  )

  # if (length(from) > 0) {
  #   for (ds_name in names(from)) {
  #     ds <- from[[ds_name]]
  #     stopifnot("Validation error: Datasets in `from` does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(ds))
  #   }
  # }

  names(from) <- vapply(from, function(ds) attr(ds, "name"), character(1))

  stopifnot(
    "`cases` must be a list." = is.list(cases),
    "Each element of `cases` must be a list with `condition` and `value`." =
      length(cases) == 0 || all(
        vapply(cases, function(case) {
          is.list(case) && all(c("condition", "value") %in% names(case)) && length(case) == 2
        }, logical(1))
      )
  )

  # stopifnot(
  #   "`by` must exist in .data and all `from` datasets." = by_str %in% colnames(.data) && all(vapply(from, function(x) by_str %in% colnames(x), logical(1)))
  # )

  path <- attr(.data, 'path')
  name <- attr(.data, 'name')

  main <- name

  levels <- if (is.factor(.data[[var_str]])) {
    levels(.data[[var_str]])
  } else {
    NULL
  }

  datasets_env <- from
  datasets_env[[main]] <- .data

  .data[[var_str]][!is.na(.data[[var_str]])] <- NA

  for (case in cases) {
    cond_expr <- rlang::get_expr(case$condition)
    val_expr  <- rlang::get_expr(case$value)

    caller_env <- rlang::caller_env()

    condition_result <- rlang::eval_tidy(cond_expr, data = datasets_env, env = caller_env)

    if (!is.logical(condition_result)) {
      stop("`condition` must evaluate to a logical vector.")
    }

    if (length(condition_result) == 1 && isTRUE(condition_result)) {
      condition_result <- rep(TRUE, nrow(.data))
    }

    matched_ids <- NULL
    for (nm in names(datasets_env)) {
      dataset <- datasets_env[[nm]]
      if (length(condition_result) == nrow(dataset)) {
        if (length(by_str) == 1) {
          ids <- dataset[[by_str]][condition_result]
        } else {
          ids <- dataset[condition_result, by_str, drop = FALSE]
        }
        matched_ids <- union(matched_ids, ids)
      }
    }

    if (length(condition_result) == nrow(.data)) {
      if (length(by_str) == 1) {
        ids <- .data[[by_str]][condition_result]
      } else {
        ids <- .data[condition_result, by_str, drop = FALSE]
      }
      matched_ids <- union(matched_ids, ids)
    }

    if (is.null(matched_ids)) matched_ids <- character(0)

    value_result <- if (rlang::is_call(val_expr) || rlang::is_symbol(val_expr)) {
      rlang::eval_tidy(val_expr, data = datasets_env, env = caller_env)
    } else {
      val_expr
    }

    is_compatible <- function(existing, incoming) {
      if (existing == 'factor' & incoming == 'character') {
        return(TRUE)
      } else if (existing == 'character' & incoming == 'factor') {
        return(TRUE)
      } else if (existing == 'double' & incoming == 'integer') {
        return(TRUE)
      } else if (existing == 'integer' & incoming == 'double') {
        return(TRUE)
      } else if (existing == incoming) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    if (!all(is.na(.data[[var_str]]))) {
      existing_type <- typeof(stats::na.omit(.data[[var_str]])[1])
      value_type <- typeof(stats::na.omit(value_result)[1])
      stopifnot("`value` type mismatch in case for variable `var`." = is_compatible(existing_type, value_type))
    }

    if (!is.null(levels)) {
      values_to_check <- unique(na.omit(value_result))
      invalid_values <- setdiff(values_to_check, levels)
      stopifnot("Validation error: Invalid value(s) assigned to factor `var` — not in defined levels: " = length(invalid_values) == 0)
    }

    if (length(by_str) == 1) {
      .data <- .data |>
        dplyr::mutate(
          !!var := dplyr::if_else(
            is.na(.data[[rlang::as_string(var)]]) & .data[[by_str]] %in% matched_ids,
            value_result,
            !!var
          )
        )
    } else {
      matched_ids <- do.call(paste0, matched_ids)
      data_ids <- do.call(paste0, .data[, by_str, drop = FALSE])
      .data <- .data |>
        dplyr::mutate(
          !!var := dplyr::if_else(
            is.na(.data[[rlang::as_string(var)]]) & data_ids %in% matched_ids,
            value_result,
            !!var
          )
        )
    }
  }

  if (!identical(default, NA)) {
    default_val <- rlang::eval_tidy(rlang::get_expr(default), data = datasets_env, env = caller_env)

    if (!all(is.na(.data[[var_str]]))) {
      existing_type <- typeof(stats::na.omit(.data[[var_str]])[1])
      default_type <- typeof(stats::na.omit(default_val)[1])
      stopifnot("Validation error: Default value type mismatch for variable `var`." = is_compatible(existing_type, default_type))
    }

    if (!is.null(levels)) {
      default_values <- unique(na.omit(default_val))
      invalid_defaults <- setdiff(default_values, levels)
      stopifnot("Validation error: Default value(s) assigned to factor `var` — not in defined levels: " = length(invalid_defaults) == 0)
    }

    .data <- .data |>
      dplyr::mutate(
        !!var := dplyr::if_else(
          is.na(.data[[var_str]]),
          default_val,
          !!var
        )
      )
  }

  if (!is.null(levels)) {
    .data <- .data |>
      dplyr::mutate(
        !!var := factor(.data[[rlang::as_string(var)]], levels = levels)
      )
  }

  .data <- set_adam_attr(.data, path, name)

  stopifnot("Validation error: The final dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(.data))

  return(.data)
}
