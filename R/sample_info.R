
# sample_information <- sample_info(eqtlID,samplesize_eqtl_file,outcomeID,samplesize_gwas)

exp_sample_info <- function(data_dir,samplesize_info){

#----R packages--------
# com_packages()

#---eqtl samplesize------------------------------------------------------------
x1 <- str_extract(dir(data_dir),".*?(?=\\.)")

idx <- data.frame()
for(i in x1){
  if(grepl("_",i))
    x2 <- str_extract(i,".*?(?=_)") %>% data.frame() else
      x2 <- i
 idx <- rbind(idx,x2)
 }

expid_list <- idx[,1]

#----------------------
if(!is.na(samplesize_info)){

  if(file.exists(samplesize_info)){
       exp_sample <-read.xlsx(samplesize_info)

       if(!any(grepl("samples",names(exp_sample))))
       warning(paste0(" '",samplesize_info,"' did not include the samplesize information. Please check it."))

       not_idx <- paste0(expid_list[!expid_list %in% exp_sample[,1]],collapse = ", ")
       if(!not_idx=="")
       warning (paste0("'",not_idx,"'"," was not in the samplesize file '",exp_sample,"', Please check it. "))

       not_smaple_info <- any( !file.exists(samplesize_info), !any(grepl("samples",names(exp_sample))), !not_idx=="" )

   } else {
           warning(paste0(" The smaplesize file '",samplesize_info, "' was not existed. "))
           not_smaple_info <-TRUE
         }


} else
not_smaple_info <-TRUE
#---is.na end--------

#--------------------
if(not_smaple_info){
  warning (paste0("The data samplesize was not provided, and it was obtained from GWAS Catalog online. "))
  data_list=NA
  data_dir=data_dir
  bat_get_gwasinfo(data_dir,data_list)
  sam_exp <-read.xlsx(paste0("gwas catalog information/",data_dir,"_gwas_id_info.xlsx"))
}

#--------------------
sam_exp <- sam_exp %>% data.frame()
sam_sit <- grep("sample",names(sam_exp),ignore.case = T)[1]
names(sam_exp)[sam_sit] <- "samplesize"
names(sam_exp)[1] <- "ID"
names(sam_exp)[2] <- "Trait"

#--------------------

return(sam_exp)

}
