if (!require(remotes)) install.packages("remotes")
if (!require(StatsTLF)) remotes::install_github("StatsTLF")
# list.of.packages <- c("StatsTLF")
# new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
# if (length(new.packages)) remotes::install_github(new.packages)
library(StatsTLF)
