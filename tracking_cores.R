# ******************************************* #
# This is an R program designed to track      #
# progress of lab work for                    #
# IID Salton Sea Project                      #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, Oct 2016                     #
# email: ybcheng@formatinoenv.com 	          #
# ******************************************* #
#
# Depends on files generated from ASD_perProc.R

rm(list=ls())

# EDIT this: set up working directory and where the files are
# set up files to search for
fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Original/"
setwd(fd)
fpattern <- "coreNames.csv"
output_csv <- "TrackingCoreNames.csv"

fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)

if (length(fn)==0) {
  stop("NO files found!!!")
}

mydata <- data.frame()
for (f in fn){
  tmp_data <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  tmp_data$lab_date <- rep(dirname(f), nrow(tmp_data))
  mydata <- rbind.data.frame(mydata, tmp_data)
}

mydata <- mydata[c(2,1)]
write.csv(mydata, output_csv)
print(paste0("Updated: ", output_csv))
