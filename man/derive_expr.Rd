% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_expr.R
\name{derive_expr}
\alias{derive_expr}
\title{Encapsulate an Expression in Quote for Use in `derive` Function}
\usage{
derive_expr(expr)
}
\arguments{
\item{expr}{The expression to be encapsulated in `quote()`.}
}
\value{
The expression encapsulated in `quote()`.
}
\description{
The `derive_expr` function takes an expression and returns it encapsulated in `quote()`. This prevents the expression from being immediately evaluated, allowing for later manipulation.
}
\examples{
\dontrun{
# Example usage:
# Encapsulating an expression
expr1 <- derive_expr(ADSL$RANDFL == "Y" & ADSL$SITEID \%in\% c("SJP01", "REC01"))
print(expr1)
}
}
