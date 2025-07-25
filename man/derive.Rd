% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive.R
\name{derive}
\alias{derive}
\title{Derive Variable in an ADaM Dataset with Complex Conditions}
\usage{
derive(.data, var, from = list(), cases = list(), by = USUBJID, default = NA)
}
\arguments{
\item{.data}{A data frame (primary dataset) where the variable will be derived.}

\item{var}{Symbol. Name of the variable to be derived in `.data`.}

\item{from}{Named list of data frames. Auxiliary datasets used in conditional expressions. Each dataset must be named, e.g., `list(ADPD = adpd_df, ADIS = adis_df)`.}

\item{cases}{List of lists. Each inner list must have two elements: `condition` and `value`.
  - `condition`: an expression evaluated within the environment containing `.data` and `from` datasets. Use `derive_expr()` to encapsulate the condition.
  - `value`: expression or constant to assign to `var` when `condition` is TRUE.
The conditions are evaluated in order; the first matching case has precedence.}

\item{by}{Symbol. The subject identifier variable name common to `.data` and all datasets in `from`. Used to match rows across datasets (default is `"USUBJID"`).}

\item{default}{Value or expression assigned to `var` for rows where no `cases` condition matches. Defaults to `NA`.}
}
\value{
The original dataset `.data` with the variable `var` derived accordingly.
}
\description{
This function derives a variable in a primary dataset (e.g., ADSL) based on complex conditions involving multiple auxiliary datasets. It supports multiple conditional assignments and a default value.
}
\examples{
\dontrun{
# Example usage:
# Derive a variable in ADSL based on conditions from ADPD
adsl <- derive(adsl, var = FASFL,
  from = list(adpd = adpd_df),
  cases = list(
    list(
      condition = derive_expr(ADSL$RANDFL == "Y" & ADPD$PARAMCD == "HI - A H5N8" & ADPD$AVAL != "Vazio"),
      value = derive_expr("Y")
    )
  ),
  default = derive_expr("N")
)
}
}
