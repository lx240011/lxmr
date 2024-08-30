

bat_tsmr <- function(exp_data_dir,snp_exp,beta_exp,se_exp,effect_allele_exp,
                   other_allele_exp,eaf_exp,pval_exp,clum_p,clump_kb,clump_r2,
                   out_data_file,snp_out,beta_out,se_out,effect_allele_out,
                   other_allele_out,eaf_out,pval_out){

#--------------------------------------
if(!dir.exists(exp_data_dir) | length(dir(exp_data_dir))==0)
  warning(paste0("The '",exp_data_dir,"' was not existed, or it was empty."))

#-----R packages-----------------------
inst_packages()

#-------------------------------------
exp_file1 <- fread(dir(exp_data_dir,full.names = T)[1])

exp_ind <- c(snp_exp,beta_exp,se_exp,effect_allele_exp,
              other_allele_exp,eaf_exp,pval_exp)

not_exp_ind <- paste0(exp_ind[!exp_ind %in% names(exp_file1)],collapse = ", ")
if(!not_exp_ind=="")
  stop(paste0("'",not_exp_ind,"'"," was not in the colnames of expousre data. Please check the exposure data."))

#-----creating dir--------------------
dir_file=paste0(out_data_file," (bat_mr_results)_",Sys.Date())
  if(!dir.exists(dir_file))
    dir.create(dir_file)

#------reading exposure data list----
if(grepl("\\.",exp_data_dir))
  exp_list <- read.xlsx(exp_data_dir) else
   exp_list <- dir(exp_data_dir) %>% as.data.frame()
#exp_list <- exp_list[c(1:2),1]  %>% as.data.frame()
colnames(exp_list)[1] <- "id"

#-------reading data ----------------
out_data <- fread(out_data_file) # read outcome data

#------outcome beta and OR transformation---------
if(!grepl("log",beta_out,ignore.case = T) | !grepl("ln",beta_out,ignore.case = T))
{
if(grepl("odd",beta_out,ignore.case = T) | grepl("OR",beta_out,ignore.case = T)){
out_data <- out_data %>% as.data.frame()
p <- grep(beta_out,names(out_data)) %>% as.numeric()
out_data[,p] <- log(out_data[,p])}
}

#-----------------
out_ind <- c(snp_out,beta_out,se_out,effect_allele_out,
             other_allele_out,eaf_out,pval_out)

not_out_ind <- paste0(out_ind[!out_ind %in% names(out_data)],collapse = ", ")
if(!not_out_ind=="")
  stop(paste0("'",not_out_ind,"'"," was not in the colnames of outcome data. Please check the outcome data."))

#------creating empty data frames----
mr_all <- data.frame()
mr_all_significant <- data.frame()
mr_ivw_significant <- data.frame()
heterogeneity_test <- data.frame()
pleiotropy_test <- data.frame()

#-------foreach analysis------------
foreach(x=c(1:nrow(exp_list)), .errorhandling = "pass") %do% {

print(paste0("It is number ",x, " of ",nrow(exp_list)))

file_type <- tolower(str_extract(exp_list[x,1],"(?<=\\.)[^\\.]+$"))

exp_data_file <- paste0(exp_data_dir,"/",exp_list[x,1])

# str_extract("exp_list.vcf.gz.txt","(?<=\\.)[^\\.]+$")
if(file_type=="txt"){
  tryCatch(exp_df <-read.table(exp_data_file,header = T,sep = "\t"),
           error=function(e){message("The exposure data cann't be readed by read.table. Try read_table function....")})
           if(!exists("exp_df") | nrow(exp_df)==0){
           exp_df <-readr::read_table(exp_data_file,show_col_types = F)}
  } else {
     if(file_type=="gz" | file_type=="tsv")
        exp_df <- fread(exp_data_file) else
        exp_df <- eval(str2expression(paste0("read.",file_type,"('",exp_data_file,"')")))
        }

#------exposure beta and OR transformation---------
if(!grepl("log",beta_exp,ignore.case = T) | !grepl("ln",beta_exp,ignore.case = T))
{
  if(grepl("odd",beta_exp,ignore.case = T) | grepl("OR",beta_exp,ignore.case = T)){
    exp_df <- exp_df %>% as.data.frame()
    p <- grep(beta_exp,names(exp_df)) %>% as.numeric()
    exp_df[,p] <- log(exp_df[,p])}
}


#--------------format exposure data---------------
exp_df <- eval(str2expression(paste0("subset(exp_df,",pval_exp,"<clum_p)")))

if(!any(grepl("id.exposure", names(exp_df))) & !any(grepl("se.exposure", names(exp_df))) ){
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
}

#----------------format outcome data---------------
out_df <- format_outcome_data (filename = out_data,
                               snps = exp_df$SNP,
                               sep = ",",
                               snp_col = snp_out,
                               beta_col = beta_out,
                               se_col = se_out,
                               effect_allele_col = effect_allele_out,
                               other_allele_col = other_allele_out,
                               eaf_col = eaf_out,
                               pval_col = pval_out)

#--------harmonise data--------------------------
harm_df <- harmonise_data(exposure_dat = exp_df,
                          outcome_dat = out_df,
                          action=2)

if(exists("harm_df")){
  harm_df$id.outcome <- str_extract(out_data_file,".*?(?=\\.)")
  }
#-------calculating R2, F,meanF------------------
#harm_df$R2 <- (2 * (harm_df$beta.exposure^2) * harm_df$eaf.exposure * (1 - harm_df$eaf.exposure)) /
#    (2 * (harm_df$beta.exposure^2) * harm_df$eaf.exposure * (1 - harm_df$eaf.exposure) +
#     2 * harm_df$samplesize.exposure*harm_df$eaf.exposure * (1 - harm_df$eaf.exposure) * harm_df$se.exposure^2)

#harm_df$f <- harm_df$R2 * (harm_df$samplesize.exposure - 2) / (1 - harm_df$R2)

#harm_df$meanf<- mean(harm_df$f)

#------screen out the data with F>10------------
#harm_res <- harm_df %>% subset(f>10)

#------MR analysis-----------------------------
#res <- mr(harm_res)
res <- mr(harm_df)
res$id.outcome <- str_extract(out_data_file,".*?(?=\\.)")

res_or_all <- generate_odds_ratios(res) %>% data.frame()
ivw_sit <- grep("Inverse variance weighted",res_or_all$method,ignore.case = T)
res_or <- res_or_all[ivw_sit,]

# mr_result=rbind(mr_result,cbind(id=x, method=res_or$method, pvalue= round(res_or$pval,5),
#                                beta=res_or$b,lo_ci=res_or$lo_ci,up_ci=res_or$up_ci,
#                                or=res_or$or,or_lci95=res_or$or_lci95,or_uci95=res_or$or_uci95))


mr_all <- rbind(mr_all,res_or_all)

if(res_or$pval<0.05){
#-------------------------------------
  mr_all_significant <- rbind(mr_all_significant,res_or_all)
  mr_ivw_significant <- rbind(mr_ivw_significant,res_or)

#-------------------------------------
  #异质性检验，没有异质性的。
  mr_hete <- mr_heterogeneity(harm_df,method_list=c("mr_ivw"))
  heterogeneity_test <- rbind(heterogeneity_test,mr_hete)


  #多水平校验，这里是没有多水平效应的
  mr_pleio <- mr_pleiotropy_test(harm_df)
  pleiotropy_test <- rbind(pleiotropy_test,mr_pleio)

#-------------------------------------
 is.het = all(!is.na(mr_hete$Q_pval),mr_hete$Q_pval<0.05)
 is.ple = all(!is.na(mr_pleio$pval),mr_pleio$pval<0.05)

  if(is.het | is.ple)
   mr_dir <- paste0(dir_file,"/",str_extract(exp_list[x,1],".*(?=_)")," (excluded)") else
       mr_dir <- paste0(dir_file,"/",str_extract(exp_list[x,1],".*(?=_)"))

  if(!dir.exists(mr_dir))
      dir.create(mr_dir)

  mr_save_file <-paste0(mr_dir,"/",str_extract(exp_list[x,1],".*(?=_)"))

#-----------save files---------------
  write.xlsx(harm_df, paste0(mr_save_file,"_harmonise_data.xlsx"))
  write.xlsx(mr_hete,  paste0(mr_save_file,"__heterogeneity_test.xlsx"))
  write.xlsx(mr_pleio, paste0(mr_save_file,"_pleiotropy_test.xlsx"))

#进行了一个（MR-PRESSO）检验，这个也是多水平效应检验，P值应该要大于0.05
  tryCatch(
  pres <- mr_presso(BetaOutcome="beta.outcome",
                    BetaExposure ="beta.exposure",
                    SdOutcome ="se.outcome",
                    SdExposure = "se.exposure",
                    OUTLIERtest =TRUE,DISTORTIONtest = TRUE,
                    data =harm_df, NbDistribution = 1000,
                    SignifThreshold = 0.05),
  error=function(e){message("There is no enough intrumental variables for PRESSO analysis")}
       )

  if(exists("pres")){
    capture.output(pres,file = paste0(mr_save_file,"_presso_test.txt"))
    write.xlsx(pres, paste0(mr_save_file,"_presso_test.xlsx"))
    }

  # 查看离群值
  #Outlier <- pres$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`

  #if(!is.null(Outlier) & is.numeric(Outlier) )
  #  harm_df <- mr_data[-Outlier,] # 剔除离群值61和70


  #Leave-one-out analysis是指逐步剔除SNP后观察剩余的稳定性，理想的是剔除后变化不大
  while (!is.null(dev.list()))  dev.off()#关闭Plots
  par(cex = 0.6)
  par(mar = c(0,4,2,0))
  png(filename = paste0(mr_save_file,"_leaveoneout_plot.png"),
      width=800, height=600,units = "px",res = 100)
  par(mai = c(1, 1, 1, 1)) # 边距 c(bottom, left, top, right)

  single<- mr_leaveoneout(harm_df)
  p1<- mr_leaveoneout_plot(single)
  print(p1)

  dev.off()

  #散点图
  while (!is.null(dev.list()))  dev.off()#关闭Plots
  par(cex = 0.6);
  par(mar = c(0,4,2,0))
  png(filename = paste0(mr_save_file,"_scatter_plot.png"),
      width=800, height=600,units = "px",res = 100)
  par(mai = c(1, 1, 1, 1)) # 边距 c(bottom, left, top, right)

  p2 <- mr_scatter_plot(res,harm_df)
  print(p2)

  dev.off()


  #绘制森林图
  while (!is.null(dev.list()))  dev.off()#关闭Plots
  par(cex = 0.6);
  par(mar = c(0,4,2,0))
  png(filename = paste0(mr_save_file,"_forest_plot.png"),
      width=800, height=600,units = "px",res = 100)
  par(mai = c(1, 1, 1, 1)) # 边距 c(bottom, left, top, right)

  res_single<- mr_singlesnp(harm_df)
  p3 <- mr_forest_plot(res_single)
  print(p3)

  dev.off()

  #绘制漏斗图，主要是看蓝线周围的散点是否对称
  while (!is.null(dev.list()))  dev.off()#关闭Plots
  par(cex = 0.6);
  par(mar = c(0,4,2,0))
  png(filename = paste0(mr_save_file,"_funnel_plot.png"),
      width=800, height=600,units = "px",res = 100)
  par(mai = c(1, 1, 1, 1)) # 边距 c(bottom, left, top, right)

  p4 <- mr_funnel_plot(res_single)
  print(p4)

  dev.off()#关闭Plots


#-------------------------------------
#----if end---------------------------
  }


#print(paste0("......Processing ",x, "is done......",  ))

p = round(100*x/nrow(exp_list),4)
print(paste0("......",p,"% was completed......"))


}#-----foreach end----------

write.xlsx(mr_all,paste0(dir_file,"/",out_data_file,"___outcome_mr_all.xlsx"))
write.xlsx(heterogeneity_test,paste0(dir_file,"/",out_data_file,"___heterogeneity_test.xlsx"))
write.xlsx(pleiotropy_test,paste0(dir_file,"/",out_data_file,"___pleiotropy_test.xlsx"))

if(nrow(mr_all_significant)>0){

  is.hete <- all(!is.na(heterogeneity_test$Q_pval),any(heterogeneity_test$Q_pval<0.05))
  if(is.hete)
     het_id <- heterogeneity_test %>% filter(Q_pval<0.05) %>% .$id.exposure else
       het_id <- NA

  is.plei <- all(!is.na(pleiotropy_test$pval),any(pleiotropy_test$pval<0.05))
  if(is.plei)
    ple_id <- pleiotropy_test %>% filter(pval<0.05) %>% .$id.exposure else
      ple_id <- NA

  exclud_id <- c(het_id,ple_id) %>% unique() %>% na.omit()

  if(length(exclud_id)>0){
    mr_all_significant <- mr_all_significant[!(mr_all_significant$id.exposure %in% exclud_id),]
    mr_ivw_significant <- mr_ivw_significant[!(mr_ivw_significant$id.exposure %in% exclud_id),]
  }


  write.xlsx(mr_all_significant,paste0(dir_file,"/",out_data_file,"___outcome_mr_all_significant.xlsx"))
  write.xlsx(mr_ivw_significant,paste0(dir_file,"/",out_data_file,"___outcome_mr_ivw_significant.xlsx"))


  }



print(paste0("The MR analysis result can be found in the folder of '",dir_file,"' "))
#-------bat_tsmr end--------
}










