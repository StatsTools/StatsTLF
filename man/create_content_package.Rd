% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_content_package.R
\name{create_content_package}
\alias{create_content_package}
\title{Create content package}
\usage{
create_content_package(
  start_number = list(T = 1L, F = 1L, L = 1L),
  sep_subtitle = "newline",
  sep_population = "newline",
  language = "PT-BR"
)
}
\arguments{
\item{start_number}{A list of 3 elements 'T', 'F' and 'L', respectively, with 3 integer values associated.}

\item{sep_subtitle}{A character to specify the separator of title and subtitle. Use 'newline' to break a line.}

\item{sep_population}{A character to specify the separator of title or subtitle and population Use 'newline' to break a line.}

\item{language}{A character to specify the language of the titles. Use 'PT-BR' or 'EN-US'.}
}
\value{
A content package.
}
\description{
Create content package
}
\examples{
x <- create_content_package(
 start_number = list('T' = 10L, 'F' = 15L, 'L' = 1L),
 sep_subtitle = ', ',
 sep_population = ': ',
 language = 'PT-BR'
)
}
