# ******************************************* #
# This is an R program designed to perform    #
# partial least squre regression analysis on  #
# samples using the ASD FieldSpec4 for the    #
# IID Salton Sea  Project                     #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, July 2016                    #
# email: ybcheng@formatinoenv.com 	          #
# ******************************************* #
#
# Depends on files generated from ASD_perProc.R
#
# This section is designed to read in resDB_sub.csv
# then do the average on spectral data for subsets that's sent for lab analysis
# and generate resDB_pls.csv and derDB_pls.csv
# 
# resDB_sub.csv should have an extra column named "subset" than resDB.csv
# This version also expects lab analysis results following the subset column 
#
# EDIT this: set up working directory and where the files are
# also set up what filename pattern to search for
# also set up expected number of spectra per core
# also set up how many soil properties are in the resDB_sub.csv file

rm(list=ls())

#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_SMGM/AS_S1_C1_ver2"
#fd <- "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/4PLS/SW_ALL - Copy"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/PR_TS_V4/PoeRoad_V4"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/VailDrain_V2"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/BB_y_PA"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/Pacific_B1"

fpattern <- "*_sub.csv"
specPerCore <- 30
sProp <- 3
#
#
setwd(fd)
fn <- list.files(fd, pattern = fpattern, include.dirs = FALSE, recursive = FALSE, full.names=TRUE)
if (length(fn)==0) {
  stop("NO files found!!!")
} 

for (f in fn){
  subDB <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  if (nrow(subDB)%%specPerCore != 0) {
    stop("WARNING!!! check database!!!")
  }
  
  # first create depth vector and add it to the DF
  dp <- double()
  for (l in seq(nrow(subDB))){
    if (is.na(subDB$subset[l])){
      dp[l] <- NA
    }else {
      dp_label <- rownames(subDB)[l]
      dp[l] <- as.numeric(substr(dp_label, nchar(dp_label)-3+1, nchar(dp_label))) #last three letter is depth
    }
  }
  subDB <- cbind(depth=dp, subDB)
  
  plsDB <- double()
  for (j in seq(1, (nrow(subDB)-specPerCore+1), by=specPerCore)){
    sub <- subDB[j:(j+specPerCore-1),]
    if (sum(sub$subset, na.rm = TRUE) != 0){
      subLabel <- rownames(sub)[1]
      out <- aggregate(x = sub, by=list(sub$subset), FUN="mean", na.action="na.exclude")
      #orgLabel <- strsplit(subLabel,"-")[[1]]
      orgLabel <- substr(subLabel, 1, nchar(subLabel)-3)
      newLabel <- character()
      for (k in seq(1, dim(out)[1])){
        dLabel <- formatC(round(out$depth[k]), width=3, flag="0")
        tmpLabel <- paste0(orgLabel, dLabel)
        newLabel <- append(newLabel, tmpLabel)
      }
      out <- out[, 4:ncol(out)]
      rownames(out) <- newLabel
      plsDB <- rbind(plsDB, out)
    }else {
      next()
    }
  }
  plsDB <- as.data.frame(plsDB)
  plsDB_fname <- gsub("_sub", "_pls", f)
  write.csv(plsDB, plsDB_fname)
  
  # After the subDB has been subset and averaged, calculate derivative spectra from it
  derDBpls <- plsDB[,1:sProp]
  derNames <- colnames(plsDB[1:sProp])
  for (k in (2+sProp):(dim(plsDB)[2]-1)){
    tmpDer <- (plsDB[k+1]-plsDB[k-1])/(as.integer(colnames(plsDB[k+1]))-as.integer(colnames(plsDB[k-1])))
    tmpName <- colnames(plsDB[k])
  #  if (k==2){
  #    derDBpls <- tmpDer
  #    derNames <- tmpName
  #  } else{
      derDBpls <- cbind(derDBpls, tmpDer)
      derNames <- append(derNames, tmpName)
  #  }
  }
  colnames(derDBpls) <- derNames
  derDBpls_fname <- gsub("res", "der", plsDB_fname)
  write.csv(derDBpls, derDBpls_fname)
  
  # also calculate continuum removal from plsDB
  library(prospectr)
  
  crDBpls <- continuumRemoval(plsDB[,(sProp+1):ncol(plsDB)])
  crDBpls <- as.data.frame(crDBpls)
  colnames(crDBpls) <- colnames(plsDB[(sProp+1):ncol(plsDB)])
  crDBpls <- cbind(plsDB[1:sProp], crDBpls)
  crDBpls_fname <- gsub("res", "cr", plsDB_fname)
  write.csv(crDBpls, crDBpls_fname)
  
  print(paste0("processed: ", f))
}

