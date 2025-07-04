% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_adam_dataset.R
\name{create_adam_dataset}
\alias{create_adam_dataset}
\title{Create an analysis dataset based on metadata file}
\usage{
create_adam_dataset(path, use_labels = FALSE)
}
\arguments{
\item{path}{A character to specify the path of the metadata file in .xlsx format based on package template.}

\item{use_labels}{A boolean to specify if factor labels should be used instead of factor levels.}
}
\value{
A tibble based on metadata file with zero rows
}
\description{
Create an analysis dataset based on metadata file
}
\examples{
x <- create_adam_dataset('ADSL.xlsx')
}
