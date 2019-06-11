# ********************************************  #
# This is an R program designed to post-process #
# results from PLS regression analysis          #
# focus on correcting outliers (>=100 or <=0)   #
# and balance the three components              #
# for the IID Salton Sea  Project               #
#                                               #
# Formation Environmental                       #
# Yen-Ben Cheng, Jan 2017                       #
# email: ybcheng@formatinoenv.com 	            #
# ********************************************  #
#
# Depends on files generated from ASD_analysis.R

rm(list=ls())

# EDIT following parameters
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/CH_S2/"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash_N2_N1/"
fpattern <- "*raw_2.csv"
specs_per_core = 30
replace <- FALSE

setwd(fd)

fn <- list.files(fd, pattern = fpattern, full.names = TRUE, recursive = TRUE)
if (length(fn)==0) {
  stop("NO files found!!!")
}

for (f in fn){
  setwd(dirname(f))
  mydata <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  
  #copy raw data and bring outliers back to range
  mydata$re_sand <- mydata$sand
  num_s1 <- length(mydata$re_sand[mydata$sand >= 100])
  num_s2 <- length(mydata$re_sand[mydata$sand <= 0])
  mydata$re_sand[mydata$sand >= 100] <- round(runif(num_s1,92,97))
  mydata$re_sand[mydata$sand <= 0] <- round(runif(num_s2,0,7))
  mydata$re_silt <- 0
  mydata$re_clay <- mydata[,ncol(mydata)-2]
  num_c1 <- length(mydata$re_clay[mydata$re_clay >= 100])
  num_c2 <- length(mydata$re_clay[mydata$re_clay <= 0])
  mydata$re_clay[mydata$re_clay <= 0] <- round(runif(num_c2,0,4))
  mydata$re_clay[mydata$re_clay >= 100] <- round(runif(num_c1,87,94))

  mydata$re_silt <- 100 - mydata$re_sand - mydata$re_clay

  #if derived silt is negative, take it out of sand and clay
  mydata$re_sand[mydata$re_silt < 0] <- floor(mydata$re_sand[mydata$re_silt < 0] + mydata$re_silt[mydata$re_silt < 0]/2)
  mydata$re_clay[mydata$re_silt < 0] <- ceiling(mydata$re_clay[mydata$re_silt < 0] + mydata$re_silt[mydata$re_silt < 0]/2)
  mydata$re_silt <- 100 - mydata$re_sand - mydata$re_clay
  
  #output refined estimates
  re_filename <- sub("raw", "refined", f)
  write.csv(mydata, re_filename) #row.names = FALSE to rid of rownames
  print(paste0("generated: ", re_filename))
}