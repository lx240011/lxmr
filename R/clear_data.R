
#--------------------------------------------------------------
clear_data <- function(){

#--------------------------------------
clear_exp_data <- function(data_dir,snp_exp,beta_exp,
                       se_exp,
                       effect_allele_exp,
                       other_allele_exp,
                       eaf_exp,pval_exp,
                       samplesize_info,
                       clum_p,clump_kb,clump_r2) {

#--------
if(!dir.exists(data_dir) | length(dir(data_dir))==0)
stop(paste0("The folder '",data_dir,"' was not existed or it was empty, please check it."))

#-----R packages-----------------------
 inst_packages()

#-------------------------------------
exp_file1 <- head(fread(dir(data_dir,full.names = T)[1]))

exp_ind <- c(snp_exp,beta_exp,se_exp,effect_allele_exp,
               other_allele_exp,eaf_exp,pval_exp)

not_exp_ind <- paste0(exp_ind[!exp_ind %in% names(exp_file1)],collapse = ", ")
if(!not_exp_ind=="")
 stop(paste0("'",not_exp_ind,"'"," was not in the colnames of expousre data. Please check the input parameters."))

rm(exp_file1)
#-----creating dir--------------------
dir_file=paste0(data_dir," (cleared result)")
 if(!dir.exists(dir_file))
  dir.create(dir_file)

#-------reading data files------------
exp_list <- dir(data_dir)
exp_list <- paste0(data_dir,"/",exp_list) %>% data.frame()
names(exp_list) <- "file_id"

#-------samplesize information ------
exp_sample_info <- exp_sample_info (data_dir,samplesize_info) %>% data.frame()

#-------foreach analysis-----------------------------------------
foreach(x=c(1:nrow(exp_list)),.errorhandling = "pass") %do% {

print(paste0("It is number ",x," of ", nrow(exp_list)))

file_type <- tolower(str_extract(exp_list[x,1],"(?<=\\.)[^\\.]+$"))

if(file_type=="txt"){
  tryCatch(exp_df <-read.table(exp_list[x,1],header = T,sep = "\t"),
             error=function(e){message("The data cann't be readed by read.table. Try read_table....")})
    exp_df <-readr::read_table(exp_list[x,1])
  } else {
    if(file_type=="gz" | file_type=="tsv")
      exp_df <- fread(exp_list[x,1]) else
        eval(str2expression(exp_df <- paste0("read.",file_type,"('",exp_list[x,1],"')")))
  }

#------exposure beta and OR transformation---------
  if(!grepl("log",beta_exp,ignore.case = T) | !grepl("ln",beta_exp,ignore.case = T))
  {
    if(grepl("odd",beta_exp,ignore.case = T) | grepl("OR",beta_exp,ignore.case = T)){
      exp_df <- exp_df %>% as.data.frame()
      p <- grep(beta_exp,names(exp_df)) %>% as.numeric()
      exp_df[,p] <- log(exp_df[,p])}
  }

#-----format data----------------------------------
exp_df <- data.frame(exp_df)
p_sit <- grep(pval_exp,names(exp_df))
exp_df[,p_sit] <- as.numeric(exp_df[,p_sit])

exp_df <- eval(str2expression(paste0("subset(exp_df,",pval_exp,"<clum_p)")))

exp_df <- format_exposure_data(filename = exp_df,
                               sep = ",",
                               snp_col = snp_exp,
                               beta_col = beta_exp,
                               se_col = se_exp,
                               effect_allele_col = effect_allele_exp,
                               other_allele_col = other_allele_exp,
                               eaf_col = eaf_exp,
                               pval_col = pval_exp)

#-------Linkage Disequilibrium (LD) test--------
exp_df <- exp_ld_clum(exp_df,clump_kb=clump_kb,clump_r2=clump_r2)
exp_df <- exp_df$exp_clum
data_id <- str_extract(exp_list[x,1],"(?<=/).*(?=_)")
exp_df$id.exposure <- data_id

#------adding samplesize and Trait information-----
exp_sit <- grep(data_id,exp_sample_info$ID)

exp_df$samplesize.exposure <- exp_sample_info$samplesize[exp_sit]
exp_df$Trait <- exp_sample_info$Trait[exp_sit]

#----calculating F value-------------
b <- exp_df$beta.exposure
eaf <- exp_df$eaf.exposure
se <- exp_df$se.exposure
sam <- sample_size

exp_df$R2<-(2*(b^2)*eaf*(1-eaf)/(2*(b^2)*eaf*(1-eaf)+2*sam*(se^2)*eaf*(1-eaf)))
exp_df$F<-exp_df$R2*(exp_df$sam-2)/(1-exp_df$R2)

write.csv(exp_df,paste0(dir_file,"/",str_extract(exp_list[x,1],"(?<=/).*(?=\\.)"),".csv"))

p = round(100*x/nrow(exp_list),4)
message(paste0("......",p,"% was completed......"))
#----the end of foreach--------------
}

print(paste0("The cleared data can be found in the folder of '", dir_file,"', good luck!"))
#------the end-----
}

#---------Runing clear_exp_data------------
data_dir=data_dir;snp_exp=snp_exp;beta_exp=beta_exp;
se_exp=se_exp;
effect_allele_exp=effect_allele_exp;
other_allele_exp=other_allele_exp;
eaf_exp=eaf_exp;pval_exp=pval_exp;
samplesize_info=samplesize_info;
clum_p=clum_p;clump_kb=clump_kb;clump_r2=clump_r2


clear_exp_data (data_dir=data_dir,snp_exp=snp_exp,beta_exp=beta_exp,
               se_exp=se_exp,
               effect_allele_exp=effect_allele_exp,
               other_allele_exp=other_allele_exp,
               eaf_exp=eaf_exp,pval_exp=pval_exp,
               samplesize_info=samplesize_info,
               clum_p=clum_p,clump_kb=clump_kb,clump_r2=clump_r2)

#--------clear_data end--------------
}


