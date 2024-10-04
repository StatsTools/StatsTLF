library(tidyverse)
library(StatsTools)
library(flextable)
library(default)
library(officer)

source_backbones()

package <- create_content_package(
 start_number = list(T = 1L, F = 1L, L = 1L),
 sep_subtitle = "newline",
 sep_population = "newline",
 language = "PT-BR"
)

package <- package

export_package(
 package = package,
 report_name = 'Results',
 template_name = "template_PT-BR.docx",
 supp = FALSE
)

rm(list = ls())
