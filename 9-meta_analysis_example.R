
install.packages("devtools")
library(devtools)

install_github("gluck4668/LXbatmr")

library(LXbatmr)

rm(list=ls())
gc()


or_dir = "met"

# devtools::load_all()

#---------------------------------------------------------
meta_analysis (or_dir)
