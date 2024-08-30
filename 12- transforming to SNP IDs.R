
library(LXbatmr)
library(data.table)

rm(list=ls())
gc()

#-------------
data_dir = "test"

file_lst <- dir(data_dir,full.names = T)

# head(fread(file_lst[1])) # 读取表信息，获取 chr 和 pos 的命名

chr="chromosome"
pos_location="base_pair_location"
buildGRCH="The 233 circulating metabolic biomarkers buildGRCh.xlsx"  # 一般为 buildGRCH37或buildGRCH38


#------------------------------------------
# devtools::load_all()

snp_tran(data_dir,chr,pos_location,buildGRCH)


