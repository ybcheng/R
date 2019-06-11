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
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Original/"
#fd <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Original/"
#fd <- "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Original/"
#fd <- "K:/IID_SaltonSea/Tasks/PotentialPilotStudies/PoeRoad/ASD/Original/"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Original/"
pd <- "K:/IID_SaltonSea/Tasks/Soil mapping/PhotoDocumentation/Processing/"

setwd(fd)
#fpattern <- "coreNames.csv"
fpattern <- "core_names.txt"
output_csv <- "TrackingCoreNames.csv"
ppattern <- "jpg$"


fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)
pn <- list.files(pd, pattern = ppattern, include.dirs = FALSE, recursive = FALSE)

if (length(fn)==0) {
  stop("NO files found!!!")
}

mydata <- data.frame()
for (f in fn){
  #tmp_data <- read.table(f, header=FALSE, row.names=NULL, sep=",", check.names=FALSE)
  tmp_data <- read.table(f, header=FALSE, row.names=1, sep=",", check.names=FALSE)
  tmp_data$lab_date <- rep(dirname(f), nrow(tmp_data))
  tmp_data$lab_count <- rep(nrow(tmp_data), nrow(tmp_data))
  mydata <- rbind.data.frame(mydata, tmp_data)
}

mydata$core_date <- as.character(substring(row.names(mydata),4,11))
mydata$core_count <- -9999
mydata$site <- as.character(substring(row.names(mydata),13,14))
mydata$site_count <- 9999

for (d in unique(mydata$core_date)){
  mydata$core_count[mydata$core_date == d] <- nrow(subset(mydata, core_date == d))
}

for (s in unique(mydata$site)){
  mydata$site_count[mydata$site == s] <- nrow(subset(mydata, site == s))
}

mydata <- mydata[c(2,1,4,3,5,6)]
fst_col <- as.data.frame(rownames(mydata))
colnames(fst_col) <- c(paste0("Updated: ", Sys.time()))
mydata <- cbind(fst_col, mydata)

if (length(pn) < nrow(mydata)){
  print("less photos than ASDs")
  length(pn) <- nrow(mydata)
  proc_photos <- pn
  mydata <- cbind.data.frame(mydata, proc_photos)
} 

write.csv(mydata, output_csv, quote=FALSE, row.names = FALSE)
print(paste0("Updated: ", output_csv))
print(Sys.time())