
forest_one_by_one <- function(or_dir,mr_method){


#---R packages--------
  com_packages()

#------------
if(!dir.exists(or_dir) | length(dir(or_dir))==0)
  stop(paste0("The folder '",or_dir, "' may be not existed or it may be empty.Pleas check it."))

dir_one <- "forest result/forest_one_by_one"
if(!dir.exists(dir_one))
  dir.create(dir_one)


files_list = dir(or_dir,full.names = T)                         #获取目录下所有文件
#or_data_merg <- data.frame()

#-----------------------------------------------------------
foreach(xi=files_list,.errorhandling = "pass") %do% {

file_type <- tolower(str_extract(xi,"(?<=\\.)[^\\.]+$"))

if(file_type=="txt"){
    tryCatch(or_data <-read.table(xi,header = T,sep = "\t"),
             error=function(e){message("The OR data file cann't be readed by read.table. Try read_table function....")})
    if(!exists("or_df") | nrow(or_df)==0){
      or_data <-readr::read_table(xi,show_col_types = F)}
  } else {
    if(file_type=="gz" | file_type=="tsv")
      or_data <- fread(xi) else
        or_data <- eval(str2expression(paste0("read.",file_type,"('",xi,"')")))
  }


#-----------------------------------
or_df <- or_data[(or_data$method %in% mr_method),] %>% arrange(-or)
or_df$method[(or_df$method %in% "Inverse variance weighted")] <- "IVW"
or_df$pval <- as.numeric(or_df$pval)

#对数据进行整理
or_df$' ' <- paste(rep(" ", 10), collapse = " ")
or_df$'OR(95% CI)'=ifelse(is.na(or_df$or), "", sprintf("%.3f (%.3f to %.3f)", or_df$or, or_df$or_lci95, or_df$or_uci95))
or_df$pval = ifelse(or_df$pval<0.001, "<0.001", sprintf("%.3f", or_df$pval))
or_df$id.exposure = ifelse(is.na(or_df$id.exposure), "", or_df$id.exposure)
or_df$nsnp = ifelse(is.na(or_df$nsnp), "", or_df$nsnp)
#or_df[duplicated(or_df$id.exposure),]$id.exposure=""
#or_df[duplicated(or_df$id.outcome),]$id.outcome=""

#准备图形参数
tm <- forest_theme(base_size = 18,   #图形整体的大小
                   #可信区间的形状、线条类型、宽度、颜色、两端竖线高度
                   ci_pch = 16, ci_lty = 1, ci_lwd = 1.5, ci_col = "blue", ci_Theight = 0.3,
                   #参考线条的形状、宽度、颜色
                   refline_gp = gpar(lwd = 1.5, lty = "dashed", col = "red"), #"grey20"
                   #x轴刻度字体的大小
                   xaxis_gp = gpar(fontsize = 12, lwd = 0.6, cex = 1),
                   #脚注大小、颜色
                   footnote_gp = gpar(fontsize = 12, cex = 1, fontface = "plain", col = "black")
                   )

#绘制图形

or_df00 <- or_df

plot_forest <- function(or_df,or_id){

plot <- forestploter::forest(or_df[, c("id.exposure","id.outcome","nsnp","method","pval"," ","OR(95% CI)")],
                             est = or_df$or,
                             lower = or_df$or_lci95,
                             upper = or_df$or_uci95,
                             ci_column = 6,     #可信区间所在的列
                             ref_line = 1,      #参考线条的位置
                             xlim = c(0.001, 3),    #X轴的范围
                             theme = tm,        #图形的参数
                             )


#修改图形中可信区间的颜色
boxcolor = c("#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F","#8491B4","#91D1C2","#DC0000","#7E6148")
boxcolor = boxcolor[as.numeric(as.factor(or_df$method))]

for(y in 1:nrow(or_df)){
  plot <- edit_plot(plot, col=5,row = y, which = "ci", gp = gpar(fill = boxcolor[y],fontsize=25)) # 改col，box的列
}

#设置pvalue的字体
pos_bold_pval = which(as.numeric(gsub('<',"",or_df$pval))<0.05)
if(length(pos_bold_pval)>0){
  for(z in pos_bold_pval){
    plot <- edit_plot(plot, col=4,row = z, which = "text", gp = gpar(fontface="bold"))  # 改col pvalue的列
  }
}

#在图形中增加线段
plot <- add_border(plot, part = "header", row =1,where = "top",gp = gpar(lwd =2))
plot <- add_border(plot, part = "header", row =1,gp = gpar(lwd =2))

if(any(or_df$or>1)){
or_p <- table(or_df$or>1)["TRUE"] %>% as.numeric()+1
plot <- add_border(plot, part = "header", row = or_p, gp = gpar(lty =2))
}

or_end <- nrow(or_df)+1
plot <- add_border(plot, part = "body", row = c(or_end), gp = gpar(lwd =2))

#设置字体大小和让文字居中
plot <- edit_plot(plot, col=1:ncol(or_df),row = 1:nrow(or_df), which = "text", gp = gpar(fontsize=12))
plot <- edit_plot(plot, col = 1:ncol(or_df), which = "text",hjust = unit(0.5, "npc"),part="header",
                  x = unit(0.5, "npc"))
plot <- edit_plot(plot, col = 1:ncol(or_df), which = "text",hjust = unit(0.5, "npc"),
                  x = unit(0.5, "npc"))


#plot
#输出图形

plot_file <- paste0(dir_one, "/",or_id,"_forest_plot ",Sys.Date(),".png")

plot_height <- dplyr::case_when(nrow(or_df) < 10 ~ 1000,
                         nrow(or_df) < 20 ~ 1200,
                         nrow(or_df) < 30 ~ 1500,
                         nrow(or_df) < 40 ~ 1800,
                         nrow(or_df) < 50 ~ 2200,
                         nrow(or_df) < 60 ~ 2500,
                         nrow(or_df) < 70 ~ 2700,
                         nrow(or_df) >= 70 ~ 3000,
                         nrow(or_df) >= 100 ~ 3200
                       )

ggsave(filename = plot_file, plot =plot,
       width = 1800,height = plot_height,units = "px",dpi = 150 )

dev.off()

#-----plot_forest end--------
}


#--------拆分表格，每个表nrow=50--------------------
nor <- -floor(-nrow(or_df00)/50) # 向上取整

df_nor <-data.frame(v1=c(50*(0:(nor-1))+1),
                    v2=c(50*c(1:nor))
                   )

df_list <- list()

for(m in 1:nor){
  df_list[[m]] <- or_df00[c(df_nor[m,1]:df_nor[m,2]),]
  df_list[[m]] <- na.omit(df_list[[m]])
  }

if(nrow(df_list[[m]])>1 & nrow(df_list[[m]])<5)
{df_list[[m-1]] <-  rbind(df_list[[m-1]],df_list[[m]])
df_list <- df_list[-m]
}

#--------拆分表格，end------------------------------

or_id_list <- str_extract(xi,"(?<=/).*?(?=\\.)")

if(nrow(df_list[[m]])>1 & nrow(df_list[[m]])<5)
   nn = nor-1 else
    nn = nor

for( k in c(1:nn)){
    id <- paste0(or_id_list,"_0",k,"_")
    or_df= df_list[[k]]
    or_id= id
    plot_forest(or_df,or_id)
    }

#----foreach end--------
}


#-----the end-------


#print(paste0("The forest plot can be found in the folder '",dir_one,"'"))

}



#---------------------------------------------------------------------------------

forest_all <- function(or_dir,mr_method){

#------------
com_packages()

#-----------
if(!dir.exists(or_dir) | length(dir(or_dir))==0)
 stop(paste0("The folder '",or_dir, "' may be not existed or it may be empty.Pleas check it."))

dir_all <- "forest result/forest_merge_data"
 if(!dir.exists(dir_all))
  dir.create(dir_all)


files_list = dir(or_dir,full.names = T)  #获取目录下所有文件
or_data_merg <- data.frame()

#-----------------------------------------------------------
foreach(xi=files_list,.errorhandling = "pass") %do% {

 file_type <- tolower(str_extract(xi,"(?<=\\.)[^\\.]+$"))

  if(file_type=="txt"){
    tryCatch(or_data <-read.table(xi,header = T,sep = "\t"),
        error=function(e){message("The OR data file cann't be readed by read.table. Try read_table function....")})
      if(!exists("or_df") | nrow(or_df)==0){
          or_data <-readr::read_table(xi,show_col_types = F)}
      } else {
        if(file_type=="gz" | file_type=="tsv")
          or_data <- fread(xi) else
            or_data <- eval(str2expression(paste0("read.",file_type,"('",xi,"')")))
      }

      or_data_merg <- rbind(or_data_merg,or_data)

      #----foreach end--------
    }


#-----------------------------------
  or_data <- or_data_merg[(or_data_merg$method %in% mr_method),]
  or_p1 <- or_data %>% subset(or>1) %>% arrange(id.outcome)
  or_p2 <- or_data %>% subset(or<1) %>% arrange(id.outcome)
  or_df <- rbind(or_p1,or_p2)
  or_df$method[(or_df$method %in% "Inverse variance weighted")] <- "IVW"
  or_df$pval <- as.numeric(or_df$pval)
  write.xlsx(or_df, paste0(dir_all, "/forest_merge_data ",Sys.Date(),".xlsx"))

#对数据进行整理
  or_df$' ' <- paste(rep(" ", 10), collapse = " ")
  or_df$'OR(95% CI)'=ifelse(is.na(or_df$or), "", sprintf("%.3f (%.3f to %.3f)", or_df$or, or_df$or_lci95, or_df$or_uci95))
  or_df$pval = ifelse(or_df$pval<0.001, "<0.001", sprintf("%.3f", or_df$pval))
  or_df$id.exposure = ifelse(is.na(or_df$id.exposure), "", or_df$id.exposure)
  or_df$nsnp = ifelse(is.na(or_df$nsnp), "", or_df$nsnp)
  #or_df[duplicated(or_df$id.exposure),]$id.exposure=""
  #or_df[duplicated(or_df$id.outcome),]$id.outcome=""

  #准备图形参数
  tm <- forest_theme(base_size = 18,   #图形整体的大小
                     #可信区间的形状、线条类型、宽度、颜色、两端竖线高度
                     ci_pch = 16, ci_lty = 1, ci_lwd = 1.5, ci_col = "blue", ci_Theight = 0.3,
                     #参考线条的形状、宽度、颜色
                     refline_gp = gpar(lwd = 1.5, lty = "dashed", col = "red"), #"grey20"
                     #x轴刻度字体的大小
                     xaxis_gp = gpar(fontsize = 12, lwd = 0.6, cex = 1),
                     #脚注大小、颜色
                     footnote_gp = gpar(fontsize = 12, cex = 1, fontface = "plain", col = "black")
                     )

  #绘制图形

  or_df00 <- or_df

  plot_forest <- function(or_df,or_id){

  plot <- forestploter::forest(or_df[, c("id.exposure","id.outcome","nsnp","method","pval"," ","OR(95% CI)")],
                                     est = or_df$or,
                                     lower = or_df$or_lci95,
                                     upper = or_df$or_uci95,
                                     ci_column = 6,     #可信区间所在的列
                                     ref_line = 1,      #参考线条的位置
                                     xlim = c(0.1, 3),    #X轴的范围
                                     theme = tm,        #图形的参数
                                 )


  #修改图形中可信区间的颜色
  boxcolor = c("#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F","#8491B4","#91D1C2","#DC0000","#7E6148")
  boxcolor = boxcolor[as.numeric(as.factor(or_df$method))]

  for(y in 1:nrow(or_df)){
      plot <- edit_plot(plot, col=5,row = y, which = "ci", gp = gpar(fill = boxcolor[y],fontsize=25)) # 改col，box的列
        }

  #设置pvalue的字体
  pos_bold_pval = which(as.numeric(gsub('<',"",or_df$pval))<0.05)
  if(length(pos_bold_pval)>0){
    for(z in pos_bold_pval){
      plot <- edit_plot(plot, col=4,row = z, which = "text", gp = gpar(fontface="bold"))  # 改col pvalue的列
      }
    }


  #在图形中增加线段
  plot <- add_border(plot, part = "header", row =1,where = "top",gp = gpar(lwd =2))
  plot <- add_border(plot, part = "header", row =1,gp = gpar(lwd =2))

  if(any(or_df$or>1)){
    or_p <- table(or_df$or>1)["TRUE"] %>% as.numeric()+1
    plot <- add_border(plot, part = "header", row = or_p, gp = gpar(lty =2))
  }

  or_end <- nrow(or_df)+1
  plot <- add_border(plot, part = "body", row = c(or_end), gp = gpar(lwd =2))

  #设置字体大小和让文字居中
  plot <- edit_plot(plot, col=1:ncol(or_df),row = 1:nrow(or_df), which = "text", gp = gpar(fontsize=12))
  plot <- edit_plot(plot, col = 1:ncol(or_df), which = "text",hjust = unit(0.5, "npc"),part="header",x = unit(0.5, "npc"))
  plot <- edit_plot(plot, col = 1:ncol(or_df), which = "text",hjust = unit(0.5, "npc"),x = unit(0.5, "npc"))


  #plot
  #输出图形

  plot_file <- paste0(dir_all, "/",or_id,"_forest_merge_plot ",Sys.Date(),".png")

  plot_height <- dplyr::case_when(nrow(or_df) < 10 ~ 1000,
                                        nrow(or_df) < 20 ~ 1200,
                                        nrow(or_df) < 30 ~ 1500,
                                        nrow(or_df) < 40 ~ 1800,
                                        nrow(or_df) < 50 ~ 2200,
                                        nrow(or_df) < 60 ~ 2500,
                                        nrow(or_df) < 70 ~ 2700,
                                        nrow(or_df) >= 70 ~ 3000,
                                        nrow(or_df) >= 100 ~ 3200
                                     )

  ggsave(filename = plot_file, plot =plot,
        width = 1800,height = plot_height,units = "px",dpi = 150 )

  dev.off()

  #-----plot_forest end--------
  }

  #--------拆分表格，每个表nrow=50--------------------
  nor <- -floor(-nrow(or_df00)/50) # 向上取整

  df_nor <-data.frame(v1=c(50*(0:(nor-1))+1),
                      v2=c(50*c(1:nor))
  )

  df_list <- list()

  for(m in 1:nor){
    df_list[[m]] <- or_df00[c(df_nor[m,1]:df_nor[m,2]),]
    df_list[[m]] <- na.omit(df_list[[m]])
  }

  if( m >1 & nrow(df_list[[m]])<5 )
  {df_list[[m-1]] <-  rbind(df_list[[m-1]],df_list[[m]])
  df_list <- df_list[-m]
  }

#--------拆分表格，ned------------------------------

  or_id_list <- ("or_merge_data")

  if(m >1 & nrow(df_list[[m]])<5 )
    nn = nor-1 else
      nn = nor

  for( k in c(1:nn)){
    id <- paste0(or_id_list,"_0",k,"_")
    or_df= df_list[[k]]
    or_id= id
    plot_forest(or_df,or_id)
  }


  #-----the end-------


# print(paste0("The forest plot can be found in the folder '",dir_all,"'"))

}



#----------------------------------------------

fores_plot <- function(){

if(!dir.exists("forest result"))
    dir.create("forest result")

or_dir=or_dir
mr_method=mr_method

#---------------------------------
forest_one_by_one (or_dir,mr_method)  # 逐个生成森林图

forest_all (or_dir,mr_method)   # 整合多个OR文件后，再生成森林图

print("The forest plot can be found in the folder 'forest result'")

}

