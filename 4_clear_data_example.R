
library(LXbatmr)

rm(list=ls())
gc()

devtools::load_all()

data_dir <- "test"

# library(data.table)
# head(fread(dir(data_dir,full.names = T)[1]))

#-------exposure data  parameters------
{
snp_exp = "variant_id"
beta_exp = "beta"
se_exp = "standard_error"
effect_allele_exp = "effect_allele"
other_allele_exp = "other_allele"
eaf_exp = "effect_allele_frequency"
pval_exp = "p_value"
#---------------------------------

samplesize_info="The 233 circulating metabolic biomarkers buildGRCh.xlsx_gwas_info.xlsx" # 先运行：1_gwas_info_example （获取样本数量）

#--------------------------------
clum_p=1e-5
clump_kb=10000
clump_r2=0.001

}


#--------------------------------------------------------------
clear_data()






