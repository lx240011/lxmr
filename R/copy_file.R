
#-----------------------------------

copy_file <- function(file_list,from_dir,to_dir){

#---R packages--------
  com_packages()

#------------

file.list <- read.xlsx(file_list) %>% .[,1]
from.list <- dir(from_dir)

if(!dir.exists(to_dir))
   dir.create(to_dir)

nonfile <-data.frame()

foreach(x=file.list,.errorhandling = "pass") %do% {

print(paste0(x, " is being copied......"))
if(!any(grepl(x,from.list,ignore.case = T))){
  message (paste0(x, " can not be found in the '",from_dir,"' folder"))
  nonfile <- rbind(nonfile,x)
  }


copy.file <- grep(x,from.list,ignore.case = T,value = T)

from.file <- paste0(from_dir,"/",copy.file)
to.file <- paste0(to_dir,"/",copy.file)

file.copy(from = from.file,to = to.file, overwrite = T,copy.mode = TRUE )

#-----foreach end--------
}

if(nrow(nonfile)>0){
colnames(nonfile)[1] <- "mis_files"
write.xlsx(nonfile,paste0(to_dir,"/mis_files.xlsx"))
}

print(paste0("The result can be found in '",to_dir,"' folder"))

}
