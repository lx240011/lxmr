
install.packages("devtools")
library(devtools)

install_github("gluck4668/LXbatmr")

library(LXbatmr)


rm(list=ls())
gc()

# devtools::load_all()

#------------Batch processing MR analysis---------------------

#----exposure data----------------
{
exp_data_file="GCST90275041.tsv"  # 暴露数据

#---暴露数据主要参数------------
snp_exp = "rs_id"
beta_exp = "beta"
se_exp = "standard_error"
effect_allele_exp = "effect_allele"
other_allele_exp = "other_allele"
eaf_exp = "effect_allele_frequency"
pval_exp = "p_value"

samplesize_eqtl=265431 #暴露数据样本数

clum_p=5e-8
clump_kb=10000
clump_r2=0.001


#-----outcome data------------------
out_data_dir="meta_GCST90275041"

snp_out = "variant_id"
beta_out = "beta"
se_out = "standard_error"
effect_allele_out = "effect_allele"
other_allele_out = "other_allele"
eaf_out = "effect_allele_frequency"
pval_out = "p_value"
}


rev_bat_tsmr(exp_data_file,snp_exp,beta_exp,se_exp,effect_allele_exp,
        other_allele_exp,eaf_exp,pval_exp,clum_p,clump_kb,clump_r2,
        out_data_dir,snp_out,beta_out,se_out,effect_allele_out,
        other_allele_out,eaf_out,pval_out)


#------------------------------------------
# library(data.table)
# head(fread(exp_data_file))
# head(fread(dir(out_data_dir,full.names = T)[1]))





