
install.packages("devtools")
library(devtools)

install_github("gluck4668/LXbatmr")

library(LXbatmr)

rm(list=ls())
gc()

or_dir = "venn data"

# devtools::load_all()

#---------------------
venn_analysis(or_dir)

