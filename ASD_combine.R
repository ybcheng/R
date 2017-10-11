# ******************************************* #
# This is an R program designed to preprocess #
# spectral measurement taken on soil core     #
# samples using the ASD FieldSpec4 for the    #
# IID Salton Sea  Project                     #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, October 2016                 #
# email: ybcheng@formatinoenv.com 	          #
# ******************************************* #
#
# depends on files generated from ASD_preProc.R
# simple script designed to combine multiple resDB.csv and derDB.csv
# for additional processing

rm(list=ls()) # clean start

# EDIT the following parameters, where the files are 
fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Original"

setwd(fd)
fn <- list.files(fd, pattern = '*resDB.csv',include.dirs = 'True', recursive = 'True')
der_fn <- list.files(fd, pattern = '*derDB.csv',include.dirs = 'True', recursive = 'True')
if (length(fn)==0  | length(der_fn)==0) {
  stop("NO files found!!!")
}

resDB <- data.frame()
for (f in fn){
  tmp <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  resDB <- rbind.data.frame(resDB, tmp)
  print(paste0("processed: ", f))
}

derDB <- data.frame()
for (d in der_fn){
  tmp <- read.table(d, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  derDB <- rbind.data.frame(derDB, tmp)
  print(paste0("processed: ", d))
}

write.csv(resDB,"resDB.csv")
resDB_sub <- cbind(subset=NA, sand=NA, silt=NA, clay=NA, resDB)
write.csv(resDB_sub,"resDB_sub.csv")
print(paste0(length(fn), " resDB files processed"))

write.csv(derDB,"derDB.csv")
print(paste0(length(der_fn), " derDB files processed"))
