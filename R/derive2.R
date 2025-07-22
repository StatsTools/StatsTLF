#' Simplified version of derive function
#'
#' Simplified version of derive function to be used in situations where want to convert
#' one variable into another with same length of levels.
#'
#' @param dataset A data frame (primary dataset) where the variable will be derived
#' @param var_target Symbol. Name of the variable to be derived in `dataset`.
#' @param var_source Symbol. Name of the variable to be used as reference to derive in `dataset`.
#' @param by Symbol. The subject identifier variable name common to `dataset`.
#'
#' @return
#' The original dataset `dataset` with the variable `var_target` derived accordingly.
#'
#' @examples
#' \dontrun{
#' adsl <- deriveN(adsl, SEX, SEXN, by = USUBJID)
#' }
#'
#' @export
derive2 <- function(dataset, var_target, var_source, by = rlang::exprs(USUBJID)) {
  stopifnot("Validation error: The dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(dataset))

  name <- attr(dataset, 'name')

  var_target_sym <- rlang::ensym(var_target)
  var_source_sym <- rlang::ensym(var_source)

  var_target_str <- rlang::as_name(var_target_sym)
  var_source_str <- rlang::as_name(var_source_sym)

  stopifnot(var_source_str %in% names(dataset))
  stopifnot(var_target_str %in% names(dataset))

  src_levels <- levels(dataset[[var_source_str]])
  stopifnot('Source variable must be closed.' = !is.null(src_levels))

  tgt_levels <- levels(dataset[[var_target_str]])
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

  dataset <- dataset |>
    StatsTLF::derive(
      var = !!var_target_sym,
      from = list(),
      by = by,
      cases = cases,
      default = NA
    )

  return(dataset)
}
