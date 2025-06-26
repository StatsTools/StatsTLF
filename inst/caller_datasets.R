library(tidyverse)
library(StatsTLF)
library(flextable)
library(default)
library(officer)

package_init <- StatsTLF::create_content_package(
 start_number = list(T = 1L, F = 1L, L = 1L),
 sep_subtitle = "newline",
 sep_population = "newline",
 language = "PT-BR"
)

package <- StatsTLF::run_caller_contents(package_init, )

saveRDS(package@content_list, paste0('./04_Datasets/Datasets', , '.RDS'))
Sys.chmod(paste0(here::here('04_Datasets'), '/Datasets - ', , '.RDS'), mode = "0444", use_umask = FALSE)

rm(list = ls())
