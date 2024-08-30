
#---------------------------------------------------------------------
snp_tran <- function(data_dir,chr,pos_location,buildGRCH){

# BiocManager::install("SNPlocs.Hsapiens.dbSNP144.GRCh37")
# BiocManager::install("SNPlocs.Hsapiens.dbSNP144.GRCh38")
# https://bioconductor.org/packages/3.19/data/annotation/src/contrib/SNPlocs.Hsapiens.dbSNP144.GRCh38_0.99.20.tar.gz
# https://bioconductor.org/packages/3.19/data/annotation/src/contrib/SNPlocs.Hsapiens.dbSNP144.GRCh37_0.99.20.tar.gz

library(SNPlocs.Hsapiens.dbSNP144.GRCh37)
library(SNPlocs.Hsapiens.dbSNP144.GRCh38)
library(data.table)
library(stringr)
library(dplyr)
library(foreach)
library(openxlsx)

file_lst <- dir(data_dir,full.names = T)

file_save = paste0(data_dir,"_new ",Sys.Date())
if(!dir.exists(file_save))
  dir.create(file_save)

#----buildGRCh------
buil <- read.xlsx(buildGRCH)
if(!any(grepl("buil",names(buil),ignore.case = T)))
  stop(paste0(" There was no buildGRCh column in the file '",buildGRCH,"' "))

meta_id <- substr(dir(data_dir),1,12)
not_id <- paste0(meta_id[!meta_id %in% buil[,1]],collapse = ", ")

if(!not_id=="")
  stop(paste0("'",not_id,"'"," was not in buildGRCH file '",buildGRCH,"' "))

#--------foreach 01 ----------
foreach(x=c(1:length(file_lst)),.errorhandling = "pass") %do% {

df <- fread(file_lst[x]) %>% data.frame()

# head(df)

print (paste0("It is number ",x, " of ",length(file_lst)))

#----获取chr信息-------
chr_sit <- grep(chr,names(df)) # Chr所在的列
chr_df <- df[,c(chr_sit)]  # 获取Chr列表
table(chr_df)
n_chr <- table(chr_df) %>% length() # 查看有多少类Chr

#-----获取buildGRCH基因版本号----
build <- read.xlsx(buildGRCH) %>% data.frame()
id <- str_extract(file_lst[x],"(?<=\\/).*?(?=\\.)")
if(grepl("_",id))
  id <- str_extract(id,".*?(?=_)")
idx <- grep(id,build[,1])

build_sit <- grep("buil",names(build),ignore.case = T)
ver <- build[,build_sit][idx] %>% as.character()

if(grepl("37",ver))
  snps <- SNPlocs.Hsapiens.dbSNP144.GRCh37 else
    snps <- SNPlocs.Hsapiens.dbSNP144.GRCh38

df_merge <- data.frame()

#----foreach 02-----------------------
foreach(y=c(1:n_chr),.errorhandling = "pass") %do% {

  message(paste0("It is Chr: ", y, " of ", n_chr))

#---获取例如y=1时chr和snp数据，这是来源于“SNPlocs.Hsapiens.dbSNP144.GRCh37”或“SNPlocs.Hsapiens.dbSNP144.GRCh38”---
  chr_snps<-snpsBySeqname(snps, as.character(y))
  pos_snp_df <- data.frame(pos = pos(chr_snps),
                           snp = mcols(chr_snps))

#----匹配df_sub的pos去筛选pos.snp.df
  df_y <- df %>% subset(chromosome==y) %>% data.frame()
  pos_sit <- grep(pos_location,names(df_y))
  my_pos <- pos_snp_df %>% subset(pos %in% df_y[,c(pos_sit)])

  colnames(my_pos) <- c(pos_location,"SNP","alleles_as_ambig")

  df_inner <- inner_join(my_pos,df_y,by = pos_location)

# head(df_inner)

  df_merge <- rbind(df_merge,df_inner)

} #-----foreach 02 end-------


fwrite(df_merge, paste0(file_save,"/", str_extract(file_lst[x],"(?<=\\/).*?(?=\\.)"),"_new.gz"))

print (paste0(" The ",x, " of ",length(file_lst), " was completed "))

} #----foreach 01 end----------------------------------

} #----snp_tran end----



