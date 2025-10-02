#' Simplified Version of Derive Function
#'
#' This function provides a simplified version of the derive function, intended for use when converting one variable into another with the same length of levels.
#'
#' @param .data A data frame (primary dataset) where the variable will be derived.
#' @param var_target Symbol. Name of the variable to be derived in `.data`.
#' @param var_source Symbol. Name of the variable to be used as a reference to derive in `.data`.
#' @param by Symbol. The subject identifier variable name common to `.data`.
#'
#' @return The original dataset `.data` with the variable `var_target` derived accordingly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Derive variable SEXN from SEX in ADSL dataset
#' adsl <- derive2(adsl, SEX, SEXN, by = USUBJID)
#' }
derive2 <- function(.data, var_target, var_source, by = rlang::exprs(USUBJID)) {
  name <- attr(.data, 'name')

  var_target_sym <- rlang::ensym(var_target)
  var_source_sym <- rlang::ensym(var_source)

  var_target_str <- rlang::as_name(var_target_sym)
  var_source_str <- rlang::as_name(var_source_sym)

  stopifnot(var_source_str %in% names(.data))
  stopifnot(var_target_str %in% names(.data))

  src_levels <- levels(.data[[var_source_str]])
  stopifnot('Source variable must be closed.' = !is.null(src_levels))

  tgt_levels <- levels(.data[[var_target_str]])
  stopifnot('Target variable must be closed.' = !is.null(tgt_levels))

  stopifnot('Levels length of both variables must be the same.' = length(src_levels) == length(tgt_levels))

  dataset_sym <- rlang::sym(name)

  make_expr_with_dataset <- function(var_sym) {
    rlang::call2("$", rlang::sym(name), var_sym)
  }

  cases <- lapply(seq_along(src_levels), function(i) {
    list(
      condition = StatsTLF::derive_expr(!!make_expr_with_dataset(var_source_sym) == !!src_levels[[i]]),
      value = StatsTLF::derive_expr(!!tgt_levels[[i]])
    )
  })

  .data <- .data |>
    StatsTLF::derive(
      var = !!var_target_sym,
      from = list(),
      by = by,
      cases = cases,
      default = NA
    )

  return(.data)
}
