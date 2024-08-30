
library(openxlsx)

meta_gwas_id <- read.xlsx("1400 metabolites gwas id.xlsx")

usethis::use_data(meta_gwas_id,overwrite = T)


data(meta_gwas_id)

