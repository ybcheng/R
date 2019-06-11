# ******************************************** #
# This script is designed to transfer files    #
# using BITS command                           #
#                                              #
# Formation Environmental                      #
# Yen-Ben Cheng, March 2019                    #
# email: ybcheng@formatinoenv.com 	           #
# ******************************************** #


# generate file transfer list ---------------------------------------------

# this section is designed particularly for Windows PowerShell
# Background Intelligent Transfer Service
# it will generate the file list needed for BITS
rm(list=ls())

#fd <- "X:/Task3c/PhotoStation/20190314"
fd <- "E:/photos/20190404"
#file_pattern <- "_SL.jpg$"
file_pattern <- "JPG$"

out_csv <- "filelist.txt"

#td <- "k:/IID_SaltonSea/Tasks/Soil mapping/PhotoDocumentation/Processing/"
#td <- "V:/temp/"
td <- "k:/IID_SaltonSea/Tasks/Soil mapping/PhotoDocumentation/Original/20190404/"
dir.create(td, showWarnings = FALSE)

setwd(fd)

if (dir.exists(fd) != TRUE){
  stop("Directory does not exist!!!")
} else{
  setwd(fd)
  fn <- list.files(fd, pattern = file_pattern, include.dirs = TRUE, recursive = TRUE, full.names=TRUE)
  
  if (length(fn) == 0){
    stop("NO files found")
  } else{
    tn <- paste0(td, basename(fn))
    YN <- file.exists(tn) #checking if the file is already in destination folder
    fn <- gsub("/", "\\\\", fn)
    tn <- gsub("/", "\\\\", tn)
    
    tmp_df <- data.frame(fn)
    colnames(tmp_df) <- "Source"
    tmp_df$Destination <- tn
    tmp_df$YN <- YN
    
    out_df <- subset(tmp_df, YN==FALSE, select=c(Source,Destination)) #only transfer non-existing files
    
    write.csv(out_df, out_csv, quote = FALSE, row.names = FALSE)
    
    print(paste0("generated: ", out_csv))
  }
}

# open windows powerShell
# navigate to today's photo folder
# cd x:\Task3c\PhotoStation\
# BITS cmd:
# Import-CSV filelist.txt | Start-BitsTransfer -TransferType Upload