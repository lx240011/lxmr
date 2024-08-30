
library(LXbatmr)

#devtools::load_all()

rm(list=ls())


file_list="finngen_R10_NAFLD.gz___outcome_mr_ivw_significant.xlsx"

from_dir="1400 metabolites_gwas_data (cleared result)"

to_dir="meta_finngen_R10_NAFLD"


#------------

copy_file(file_list,from_dir,to_dir)
