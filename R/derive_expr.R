#' Encapsulate an expression in quote to be used in `derive_adam` function
#'
#' The `derive_expr` function takes an expression and returns it encapsulated in `quote()`.
#' This prevents the expression from being immediately evaluated, allowing for later manipulation.
#'
#' @param expr The expression to be encapsulated in `quote()`.
#'
#' @return Returns the expression encapsulated in `quote()`.
#'
#' @examples
#' # Encapsulating an expression
#' expr1 <- derive_expr(ADSL$RANDFL == "Y" & ADSL$SITEID %in% c("SJP01", "REC01"))
#' print(expr1)
#'
#' @export
derive_expr <- function(expr) {
  rlang::enquo(expr)
}
