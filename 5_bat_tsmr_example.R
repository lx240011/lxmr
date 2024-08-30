
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
exp_data_dir="test"  # 数据所在的文件夹

#---暴露数据主要参数，缺一不可------
snp_exp = "SNP"
beta_exp = "beta.exposure"
se_exp = "se.exposure"
effect_allele_exp = "effect_allele.exposure"
other_allele_exp = "other_allele.exposure"
eaf_exp = "eaf.exposure"
pval_exp = "pval.exposure"

# 第一步 1_gwas_info_example （获取样本数量）生成的文件。
# 如果是1400个代谢物，因系统已包含其基本数据信息，此时，samplesize_eqtl=NULL
samplesize_eqtl=NULL

#----暴露数据筛选和去除连锁不平衡的条件------
clum_p=5e-8
clump_kb=10000
clump_r2=0.001


#-----outcome data------------------
out_data_file="GCST90275044.tsv"

#---结局数据主要参数，缺一不可------
snp_out = "rs_id"
beta_out = "beta"
se_out = "standard_error"
effect_allele_out = "effect_allele"
other_allele_out = "other_allele"
eaf_out = "effect_allele_frequency"
pval_out = "p_value"
}

#----------------------------

bat_tsmr(exp_data_dir,snp_exp,beta_exp,se_exp,effect_allele_exp,
        other_allele_exp,eaf_exp,pval_exp,clum_p,clump_kb,clump_r2,
        out_data_file,snp_out,beta_out,se_out,effect_allele_out,
        other_allele_out,eaf_out,pval_out)

#----------------------------
# library(data.table)

# head(fread(dir(exp_data_dir)[1]))  # 核对暴露数据“列名称”
# head(fread(out_data_file))         #核对结局数据“列名称”



