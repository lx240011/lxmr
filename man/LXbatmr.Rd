\name{LXbatmr}
\alias{LXbatmr}
\title{LXbatmr}
\usage{
bat_tsmr(exp_data_dir,snp_exp,beta_exp,se_exp,effect_allele_exp,
        other_allele_exp,eaf_exp,pval_exp,clum_p,clump_kb,clump_r2,
        out_data_file,snp_out,beta_out,se_out,effect_allele_out,
        other_allele_out,eaf_out,pval_out)
}
\description{
LXbatmr can batch process the MR analysis.
}
\examples{

##---1_gwas_info_example---##

install.packages("devtools")
library(devtools)

install_github("gluck4668/LXbatmr")

library(LXbatmr)

rm(list=ls())

data_list = NA # "finngen_R10_NAFLD_mr_significant.xlsx" # NA

data_dir= "1400 metabolites_gwas_data (cleared result)" # "info_test" # NA


bat_get_gwasinfo(data_dir,data_list)  # the main function of "1_gwas_info_example".


##---2_tabel_colname_example---##

exp_data_dir <- "test"

table_colname(exp_data_dir)


##---3_copy_files_example---##

file_list="GCST90275041.tsv___outcome_mr_ivw_significant.xlsx"

from_dir="1400 metabolites_gwas_data (cleared result)"

to_dir="meta_GCST90275041"


copy_file(file_list,from_dir,to_dir)


##---4_clear_data_example---##

data_dir <- "test gz"

{
snp_exp = "variant_id"
beta_exp = "beta"
se_exp = "standard_error"
effect_allele_exp = "effect_allele"
other_allele_exp = "other_allele"
eaf_exp = "effect_allele_frequency"
pval_exp = "p_value"

samplesize_info="test gz___gwas_id_info.xlsx"

clum_p=1e-5
clump_kb=10000
clump_r2=0.001

}

clear_data()


##---5_bat_tsmr_example---##

{
exp_data_dir="1400 metabolites_gwas_data (cleared result)"

snp_exp = "SNP"
beta_exp = "beta.exposure"
se_exp = "se.exposure"
effect_allele_exp = "effect_allele.exposure"
other_allele_exp = "other_allele.exposure"
eaf_exp = "eaf.exposure"
pval_exp = "pval.exposure"

samplesize_eqtl="1400 metabolites_gwas_data (cleared result)___gwas_id_info.xlsx"

clum_p=5e-8
clump_kb=10000
clump_r2=0.001


#-----outcome data------------------
out_data_file="GCST90275041.tsv"

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


##---6_rev_bat_tsmr_example---##

{
exp_data_file="GCST90275041.tsv"

snp_exp = "rs_id"
beta_exp = "beta"
se_exp = "standard_error"
effect_allele_exp = "effect_allele"
other_allele_exp = "other_allele"
eaf_exp = "effect_allele_frequency"
pval_exp = "p_value"

samplesize_eqtl=265431

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


##---7-forest_example---#

or_dir ="6-5"

mr_method=c("Inverse variance weighted")

fores_plot()


##---8-cir_plot_example---##

or_file = "GCST90275041.tsv___outcome_mr_all_significant.xlsx"

mr_method=c("Inverse variance weighted","MR Egger","Weighted mode","Weighted median","Simple mode")
n_meth = length(mr_method)

cir_txt_sit = 1.3+(n_meth:1)*0.7

cir_pic (or_file,cir_txt_sit,
         gap.degree=35,
         start.degree=350,
         track.height=0.2)

##---9-meta_analysis_example---##

or_dir = "met"

meta_analysis (or_dir)


##---10-venn_analysis_example---##

or_dir = "02"

venn_analysis(or_dir)


}
