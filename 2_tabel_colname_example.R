
install.packages("devtools")
library(devtools)

install_github("gluck4668/LXbatmr")

library(LXbatmr)

#-------------------------

rm(list=ls())
gc()

# devtools::load_all()

exp_data_dir <- "test"

table_colname(exp_data_dir)

