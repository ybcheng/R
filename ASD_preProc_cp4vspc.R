# ******************************************* #
# This is an R program designed to preprocess #
# spectral measurement taken on soil core     #
# samples using the ASD FieldSpec4 for the    #
# IID Salton Sea  Project                     #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, January 2017                 #
# email: ybcheng@formatinoenv.com 	          #
# ******************************************* #

rm(list=ls()) # clean start
# This program is designed to copy all the ASD raw files to
# the "workspace" folder to be further processed with ViewSpecPro
# assuming ASD files are separated in different folders 
# for different cores. The program will read everything 
# in and put them together                              
# 
#library(asdreader)

# user input --------------------------------------------------------------

# EDIT this: set up where the calibration files are
cal_fd <- "C:/Users/ybcheng/Documents/ASD/calibration_files"

# EDIT this: set up working directory and where the files are
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_2/water"
#fd <- "K:/IID_SaltonSea/Tasks/AQ_Monitoring/ASD/WE20170422"
#fd <- "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Original/20171099"
#fd <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Original/20171028"
#fd <- "K:/IID_SaltonSea/Tasks/PotentialPilotStudies/Clubhouse/ASD/Original/20180225"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Original/20190311"

# EDIT the expected number of files per core
rawPerCore <- 64
#rawPerCore <- 7
#
# EDIT whether to output filenames and fileinfo to csv files or not
filenamesYN <- 1  #usually 0, output filenames to a csv file
fileinfoYN <- 0   #usually 0, but 1 for PiSWERL sites
#



# checking raw data -------------------------------------------------------
#
print("reading in raw data....")

setwd(fd)
fn <- list.files(fd, pattern = '\\.asd$',include.dirs = 'True', recursive = 'True')
if (length(fn)==0) {
  stop("NO files found!!!")
} else if (length(fn)%%rawPerCore != 0) {
  stop("WARNING!!! check database!!!")
} else {
  print("database Looks Good!!!")
}
print("")


# assemble workspace ------------------------------------------------------

# write.csv(fn, "filenames.csv")
ws_dir <- paste0(fd, "/workspace/")
ws_fn <- paste0(ws_dir, formatC(1:length(fn), width=5, flag=0), ".asd")

if (file.exists(ws_dir)){
  file.copy(fn, ws_fn)
}else{
  dir.create(ws_dir, showWarnings = FALSE)
  file.copy(fn, ws_fn)
}

# copy all the calibration files to workspace
print("copy calibration files...")
cal_fn <- list.files(cal_fd, full.names = TRUE)
file.copy(cal_fn, ws_dir)

# output filenames to a CSV file
if (filenamesYN==1){
  write.csv(fn, paste0(ws_dir, "filenames.csv"))
}

# output filenames and modified time to a CSV file
if (fileinfoYN == 1){
  df_fn <- data.frame(fn)
  df_fn$mtime <- file.mtime(fn)
  write.csv(df_fn, paste0(ws_dir, "fileinfo.csv"))
}

print("workspace assembled")