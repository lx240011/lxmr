#---------------------------------------------------------
meta_analysis <- function(or_dir){

#---R packages--------
com_packages()

#----------------------
dir_file <- paste0("meta_analysis ",Sys.Date())
if(!dir.exists(dir_file))
    dir.create(dir_file)

#------reading data----------------
or_file_list <- dir(or_dir,full.names = T)

#-----merge data------------------
or_data <- data.frame()

for(x in or_file_list){
df <- read.xlsx(x)
or_data <- rbind(or_data,df)
}

#-------meta analysis---------------------------------------
meta_analysis <- function(or_df,save_name){
log_or <- log(or_df$or)
log_uci<-log(or_df$or_uci95)
log_lci<-log(or_df$or_lci95)
se_log_or <-(log_uci-log_lci)/(2*1.96)

meta_df <- data.frame(id= paste0 (or_df$id.exposure,"-",or_df$id.outcome),
                      or= or_df$or,
                      or_uci95=or_df$or_uci95,
                      or_lci95=or_df$or_lci95
                      )

meta_analysis <- meta:: metagen(log_or,se_log_or,sm="OR",data=meta_df, studlab=meta_df$id)

summary(meta_analysis)


plot_height <- case_when(nrow(meta_df)<30 ~ 1500,
                        nrow(meta_df)<40 ~ 1600,
                        nrow(meta_df)<50 ~ 1800,
                        nrow(meta_df)<60 ~ 2000,
                        nrow(meta_df)<70 ~ 2200,
                        nrow(meta_df)<150 ~ 2400,
                        nrow(meta_df)>150 ~ 2600,
                        )



while (!is.null(dev.list()))   dev.off()
png(filename = save_name,
    width=1600, height=plot_height,units = "px",res = 150)
par(mai = c(0.2, 0.2, 0.2, 0.2)) # 边距 c(bottom, left, top, right)

meta::forest(meta_analysis,plotwidth = "10cm",fontsize = 8)

while (!is.null(dev.list()))   dev.off()

} #----meta_analysis function end-------------------------------------

#------------------------------------------
or_df <- or_data %>% arrange (-or)
save_name = paste0(dir_file,"/meta_plot_all ",Sys.Date(),".png")

meta_analysis(or_df,save_name)
write.xlsx(or_df,paste0(dir_file,"/meta_plot_all ",Sys.Date(),".xlsx"))

#------positive correlation---------------
or_df <- or_data %>% filter (or>1)
if(nrow(or_df)>0){
save_name = paste0(dir_file,"/meta_plot (positive correlation) ",Sys.Date(),".png")
meta_analysis(or_df,save_name)
write.xlsx(or_df,paste0(dir_file,"/meta_plot (positive correlation) ",Sys.Date(),".xlsx"))
}
#-------negative correlation--------------
or_df <- or_data %>% filter (or<1)
if(nrow(or_df)>0){
save_name = paste0(dir_file,"/meta_plot (negative correlation) ",Sys.Date(),".png")
meta_analysis(or_df,save_name)
write.xlsx(or_df,paste0(dir_file,"/meta_plot (negative correlation) ",Sys.Date(),".xlsx"))
}


}# --the end--


