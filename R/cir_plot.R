
#----------------------------------------------------------#
cir_pic <- function(or_file,cir_txt_sit,
                    gap.degree,
                    start.degree,
                    track.height){
#-----R packages-----------------------
com_packages()

#-------------------------------------
dir_or <- "circos_plot_result"
if(!dir.exists(dir_or))
   dir.create(dir_or)

#-----reading mr or data---------------
file_type <- tolower(str_extract(or_file,"(?<=\\.)[^\\.]+$"))

if(file_type=="txt"){
  tryCatch(or_data <-read.table(or_file,header = T,sep = "\t"),
           error=function(e){message("The OR data file cann't be readed by read.table. Try read_table function....")})
  if(!exists("or_df") | nrow(or_df)==0){
    or_data <-readr::read_table(or_file,show_col_types = F)}
} else {
  if(file_type=="gz" | file_type=="tsv")
    or_data <- fread(or_file) else
      or_data <- eval(str2expression(paste0("read.",file_type,"('",or_file,"')")))
}


or_df <- or_data[(or_data$method %in% mr_method),]
or_df$method[or_df$method=="Inverse variance weighted"]="IVW"

mat=acast(or_df, id.exposure~method, value.var="pval")
#mat <- na.omit(mat)

col_color = colorRamp2(c(0, 0.01, 0.1, 0.5, 1), c("#ff0000", "#ff3300","white", "skyblue", "blue"))


#while (!is.null(dev.list()))   dev.off()
#circos.clear()
#circos.par(gap.after=c(20))
#pdf(file =paste0(out_data_file,"_circos_plot.pdf"),
#    width=8, height=8)


#--------
while (!is.null(dev.list()))   dev.off()
{circos.clear()
circos.par(gap.after=c(20))
png(filename = paste0(dir_or,"/",str_extract(or_file,".*?(?=\\.)"),"_circos_plot.png"),
    width=1200, height=1200,units = "px",res = 150)
#par(mai = c(0.5, 0.5, 0.5, 0.5)) # 边距 c(bottom, left, top, right)

# 设置布局参数
circos.par(
  gap.degree = gap.degree,  # 圆环之间的角度间隔
 # cell.padding = c("cell", 0.02, "mm"),  # 单元填充
  start.degree = start.degree  # 起始角度
)

circos.heatmap(mat, # 数据矩阵
               col = col_color,
               rownames.cex = 0.8,
               dend.side = "inside",
               rownames.side = "outside",
               track.height = track.height,  # 扇形高度
               bg.border = "black")

}

# while (!is.null(dev.list()))   dev.off()

cn = colnames(mat)
n = length(cn)
#cir_txt_sit = 0.35+(n:1)*0.2 # methods 的位置和行距

circos.text(rep(CELL_META$cell.xlim[2], n) +
              convert_x(1, "mm"),
              cir_txt_sit,
             # 0.35+(n:1)*0.2, # methods 的位置和行距
              cn,
              cex =0.8,  # 字体大小
              adj = c(0, 0.5),
              facing = "inside")

# circos.text(rep(CELL_META$cell.xlim[2], n) +
#    convert_x(1, "mm"),
#    0.35+(n:1)*0.2, # methods 的位置和行距
#	  cn, cex =0.8,  # 字体大小
#	  adj = c(0, 0.5),
#	  facing = "inside")

circos.clear()

lgd = Legend(title="Pvalue", col_fun=col_color)
grid.draw(lgd)


while (!is.null(dev.list()))   dev.off()

print(paste0("The circos plot can be found in the folder of '",dir_or,"' "))

#---------the end-------------
}
