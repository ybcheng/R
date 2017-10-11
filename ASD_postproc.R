# ********************************************  #
# This is an R program designed to post-process #
# results from PLS regression analysis for the  #
# IID Salton Sea  Project                       #
#                                               #
# Formation Environmental                       #
# Yen-Ben Cheng, Oct 2016                       #
# email: ybcheng@formatinoenv.com 	            #
# ********************************************  #
#
# Depends on files generated from ASD_analysis.R

rm(list=ls())

# EDIT following parameters
fd <- "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/AN/"
fpattern <- "*results.csv"
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

  for (i in seq(1, (nrow(mydata)-specs_per_core+1), by=specs_per_core)){
    one_core <- mydata[i:(i+specs_per_core-1),]
    #plot.new()
    plot(one_core$Z, one_core$sand)
    lo_sand <- loess(one_core$sand~one_core$Z, span=0.33)
    lines(one_core$Z, predict(lo_sand), col='blue', lwd=1.5)
    plot(one_core$Z, one_core$clay)
    lo_clay <- loess(one_core$clay~one_core$Z, span=0.33)
    lines(one_core$Z, predict(lo_clay), col='red', lwd=1.5)
    if (replace == TRUE){
      mydata$sand[i:(i+specs_per_core-1)] <- round(predict(lo_sand), 0)
      mydata$clay[i:(i+specs_per_core-1)] <- round(predict(lo_clay), 0)
    } else{
      index <- is.na(mydata$lab_sand[i:(i+specs_per_core-1)])
      mydata$sand[i:(i+specs_per_core-1)][index] <- round(predict(lo_sand)[index], 0)
      mydata$clay[i:(i+specs_per_core-1)][index] <- round(predict(lo_clay)[index], 0)
    }
  }

  mydata$silt <- 100 - mydata$sand - mydata$clay
  write.csv(mydata, "results_refined.csv") #row.names = FALSE to rid of rownames
  print(paste0(getwd(), "/results_refined.csv"))
}