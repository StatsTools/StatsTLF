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
  var <- rlang::ensym(var)
  by_str <- purrr::map_chr(by, rlang::as_string)
  var_str <- rlang::as_string(var)

  ### Validate Inputs ----------------------------------------------------------
  stopifnot(
    '`var` and `by` cannot be the same variable.' = by_str != var_str,
    "Validation error: The dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(.data, print = FALSE),
    "`var` must be a single name or symbol." = rlang::is_symbol(var) || (is.character(var) && length(var) == 1),
    "`var` must exist in .data." = var_str %in% colnames(.data),
    "`by` must exist in .data." = all(by_str %in% colnames(.data)),
    "`from` must be a list." = is.list(from)
  )

  if (length(from) > 0) {
    current_names <- names(from)
    names(from) <- purrr::map_chr(seq_along(from), function(i) {
      n <- current_names[i]
      if (!is.null(n) && nzchar(n)) {
        n
      } else {
        attr_name <- attr(from[[i]], "name")
        if (!is.null(attr_name) && nzchar(attr_name)) attr_name else ""
      }
    })

    stopifnot("All elements of `from` must have valid names." = all(nzchar(names(from))))
  }

  stopifnot(
    "`cases` must be a list." = is.list(cases),
    "Each element of `cases` must be a list with `condition` and `value`." =
      length(cases) == 0 || all(
        vapply(cases, function(case) {
          is.list(case) && all(c("condition", "value") %in% names(case)) && length(case) == 2
        }, logical(1))
      )
  )

  ### Setup Enviromnent --------------------------------------------------------
  cat('Deriving', var_str, '...')

  path <- attr(.data, 'path')
  name <- attr(.data, 'name')
  main <- name

  levels <- if (is.factor(.data[[var_str]])) levels(.data[[var_str]]) else NULL

  datasets_env <- from
  datasets_env[[main]] <- .data

  .data[[var_str]][!is.na(.data[[var_str]])] <- NA

  for (case in cases) {
    ### Run Condition ------------------------------------------------------------
    cond_expr <- rlang::get_expr(case$condition)
    val_expr  <- rlang::get_expr(case$value)
    caller_env <- rlang::caller_env()

    condition_result <- rlang::eval_tidy(cond_expr, data = datasets_env, env = caller_env)

    if (!is.logical(condition_result)) stop("`condition` must evaluate to a logical vector.")
    if (length(condition_result) == 1 && isTRUE(condition_result)) condition_result <- rep(TRUE, nrow(.data))

    ### Match IDs ----------------------------------------------------------------
    matched_ids <- NULL
    for (nm in names(datasets_env)) {
      dataset <- datasets_env[[nm]]
      if (attr(dataset, 'name') == main) {
        if (length(by_str) == 1) {
          ids <- dataset[[by_str]][condition_result]
        } else {
          ids <- dataset[condition_result, by_str, drop = FALSE]
        }
        matched_ids <- union(matched_ids, ids)
      }
    }

    if (is.null(matched_ids)) matched_ids <- character(0)

    ### Run Value ----------------------------------------------------------------
    value_result <- if (rlang::is_call(val_expr) || rlang::is_symbol(val_expr)) {
      rlang::eval_tidy(val_expr, data = datasets_env, env = caller_env)
    } else {
      val_expr
    }

    value_length <- length(value_result)
    source_dataset <- purrr::keep(datasets_env, ~ nrow(.x) == value_length)

    if (length(source_dataset) == 1 && !is.null(matched_ids)) {
      src <- source_dataset[[1]]
      val_tmp <- paste0(".val_", var_str)
      src[[val_tmp]] <- value_result

      if (length(by_str) == 1) {
        reduced_values <- src |>
          dplyr::filter(.data[[by_str]] %in% matched_ids) |>
          dplyr::group_by(across(all_of(by_str))) |>
          dplyr::summarise(!!var := dplyr::first(.data[[val_tmp]]), .groups = "drop")
      } else {
        reduced_values <- src |>
          dplyr::filter(
            purrr::reduce(
              purrr::map2(by_str, matched_ids, ~ src[[.x]] %in% .y),
              `&`
            )
          ) |>
          dplyr::group_by(across(all_of(by_str))) |>
          dplyr::summarise(!!var := dplyr::first(.data[[val_tmp]]), .groups = "drop")

        matched_ids <- do.call(paste0, matched_ids)
        data_ids <- do.call(paste0, .data[, by_str, drop = FALSE])
      }

      value_final <- .data |>
        dplyr::select(by_str) |>
        dplyr::left_join(reduced_values, by = by_str) |>
        dplyr::pull(!!var)

      if (length(by_str) == 1) {
        .data <- .data |>
          dplyr::mutate(
            !!var := dplyr::if_else(
              is.na(.data[[rlang::as_string(var)]]) & .data[[by_str]] %in% matched_ids,
              value_final,
              !!var
            )
          )
      } else {
        .data <- .data |>
          dplyr::mutate(
            !!var := dplyr::if_else(
              is.na(.data[[rlang::as_string(var)]]) & data_ids %in% matched_ids,
              value_final,
              !!var
            )
          )
      }
    } else {
      if (!all(is.na(.data[[var_str]]))) {
        existing_type <- typeof(stats::na.omit(.data[[var_str]])[1])
        value_type <- typeof(stats::na.omit(value_result)[1])
        is_compatible <- function(existing, incoming) {
          (existing == incoming) ||
            (existing == 'factor' & incoming == 'character') ||
            (existing == 'character' & incoming == 'factor') ||
            (existing == 'double' & incoming == 'integer') ||
            (existing == 'integer' & incoming == 'double')
        }
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
  }

  ### Add Default Value --------------------------------------------------------
  if (!identical(default, NA)) {
    default_val <- rlang::eval_tidy(rlang::get_expr(default), data = datasets_env, env = caller_env)

    if (!all(is.na(.data[[var_str]]))) {
      existing_type <- typeof(stats::na.omit(.data[[var_str]])[1])
      default_type <- typeof(stats::na.omit(default_val)[1])
      is_compatible <- function(existing, incoming) {
        (existing == incoming) ||
          (existing == 'factor' & incoming == 'character') ||
          (existing == 'character' & incoming == 'factor') ||
          (existing == 'double' & incoming == 'integer') ||
          (existing == 'integer' & incoming == 'double')
      }
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

  ### Set Attributes -----------------------------------------------------------
  if (!is.null(levels)) {
    .data <- .data |>
      dplyr::mutate(
        !!var := factor(.data[[rlang::as_string(var)]], levels = levels)
      )
  }

  .data <- set_adam_attr(.data, path, name)

  stopifnot(StatsTLF::validate_adam_dataset(.data))

  return(.data)
}
