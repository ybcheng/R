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

# EDIT this: set up working directory and where the files are
fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_SMGM/CP_NE_NW_SE1_SC1_2_SW/"
setwd(fd)
out_fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_SMGM/CP_NE_NW_SE1_SC1_2_SW/indv_spec/"


##################################################
# Use this section to read in multiple csv files
# and put them in a big dataframe
# then output each spectrum into a single file for
# SMGM processing
#
# EDIT this: set up what filename pattern to search for
fpattern <- "*absDB.csv"
#
print("reading in csv files....")

fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)
if (length(fn)==0) {
  stop("NO files found!!!")
}  
#print(fn)

if (file.exists(out_fd) == FALSE){
  dir.create(out_fd, showWarnings = FALSE)
}

mydata <- data.frame()
for (f in fn){
  tmp <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  mydata <- rbind.data.frame(mydata, tmp)
}

#mydata <- data.frame(t(mydata))
#mydata$wvl <- as.integer(rownames(mydata))

for (i in seq(nrow(mydata))){
  tmp <- mydata[i,]
  out_fn <- paste0(out_fd, rownames(tmp)[1], ".csv")
  tmp <- data.frame(t(tmp))*100
  tmp$wvl <- as.integer(rownames(tmp))
  tmp <- tmp[c(2,1)]
  write.table(tmp, file=out_fn, row.names = FALSE, col.names = FALSE, sep=",")
  print (paste0("processed:  ", out_fn))
}




##################################################
# Use this section to read in SMGM results and 
# average them based on subset data       
#
# EDIT this: set up what filename pattern to search for
rm(list=ls())

fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_SMGM/CP_NE_NW_SE1_SC1_2_SW"
fpattern <- "*SMGMdb_sub.csv"
specPerCore <- 30
sProp <- 3
#
#
setwd(fd)
fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE, full.names=TRUE)
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
}