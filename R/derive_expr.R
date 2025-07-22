#' Encapsulate an Expression in Quote for Use in `derive` Function
#'
#' The `derive_expr` function takes an expression and returns it encapsulated in `quote()`. This prevents the expression from being immediately evaluated, allowing for later manipulation.
#'
#' @param expr The expression to be encapsulated in `quote()`.
#'
#' @return The expression encapsulated in `quote()`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Encapsulating an expression
#' expr1 <- derive_expr(ADSL$RANDFL == "Y" & ADSL$SITEID %in% c("SJP01", "REC01"))
#' print(expr1)
#' }
derive_expr <- function(expr) {
  rlang::enquo(expr)
}
