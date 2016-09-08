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
#
# EDIT this: set up working directory and where the files are
# also set up what filename pattern to search for
# also set up expected number of spectra per core
fd <- "C:/Users/ybcheng/Documents/data2016/20160907ASD/"
fpattern <- "*_sub.csv"
specPerCore <- 28
#
setwd(fd)
fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE, full.names=TRUE)
if (length(fn)==0) {
  stop("NO files found!!!")
} 

for (i in seq(length(fn))){
  subDB <- read.table(fn[i], header=TRUE, row.names=1, sep=",", check.names=FALSE)
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
  }
  plsDB <- as.data.frame(plsDB)
  plsDB_name <- gsub("_sub", "_pls", fn[i])
  write.csv(plsDB, plsDB_name)
  
  # After the subDB has been subset and averaged, calculate derivative spectra from it
  for (k in 2:(dim(plsDB)[2]-1)){
    tmpDer <- (plsDB[k+1]-plsDB[k-1])/(as.integer(colnames(plsDB[k+1]))-as.integer(colnames(plsDB[k-1])))
    tmpName <- colnames(plsDB[k])
    if (k==2){
      derDB <- tmpDer
      derNames <- tmpName
    } else{
      derDB <- cbind(derDB, tmpDer)
      derNames <- append(derNames, tmpName)
    }
  }
  colnames(derDB) <- derNames
  derDB_name <- gsub("res", "der", plsDB_name)
  write.csv(derDB,derDB_name)
  
  print(paste0("processed: ", fn[i]))
}


# This section is to read in multiple csv file and  #
# csv files and put them in a big dataframe         #
#
# EDIT this: set up working directory and where the files are
# also set up what filename pattern to search for
fd <- "C:/Users/ybcheng/Documents/data2016/20160801Rcode/"
fpattern <- "*22.csv"
#
print("reading in csv files....")

setwd(fd)
fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)
if (length(fn)==0) {
  stop("NO files found!!!")
} 
print(fn)

for (i in seq(length(fn))){
  tmp <- read.table(fn[i], header=TRUE, row.names=1, sep=",", check.names=FALSE)
  if (i == 1){
    mydata <- tmp
  } else{
    mydata <- rbind.data.frame(mydata, tmp)
  }
}

print("dataframe assembled for analysis")
print("")




# the following part is to randomly select part of the dataframe as training data and
# the rest as testing data
#
# EDIT this: the percentage of the data to be used as training data
perc_train <- 0.7

vect_train <- sort(sample(nrow(mydata), round(nrow(mydata)*perc_train)))
mydata_train <- mydata[vect_train,]
mydata_test <- mydata[-vect_train,]




# this following are some examples to perform
# partial least square regression on the big dataframe
#
library(pls)
#

fn <- "C:/Users/ybcheng/Documents/data2016/20160907ASD/resDB_pls.csv"
mydata <- read.table(fn, header=TRUE, row.names=1, sep=",", check.names=FALSE)

# first prepare the variables from the database 
clay <- mydata[,1]
silt <- mydata[,2]
specs <- as.matrix(mydata[,3:ncol(mydata)])

# then put them together in a dataframe
mydf <- data.frame(clay=clay, silt=silt, specs=I(specs))
# or
mydf <- data.frame(clay=clay, silt=silt)
mydf$specs <- specs
# will generate the same dataframe


clay.pls <- plsr(clay~specs, 5, data=mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(clay.pls)
clay.pls$validation$PRESS #showing PRESS values
plot(RMSEP(clay.pls), legendpos="topright")
plot(clay.pls, ncomp=15, asp=1, line=TRUE)
plot(clay.pls, plottype = "scores", comps = 5:10)
plot(clay.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

predict(clay.pls, ncomp=10, newdata=clayTest) #this requires setting up a test dataset clayTest
RMSEP(clay.pls, newdata = clayTest)

# this part is to read resDB back in and use the pls model to 
# estimate clay content for every depth along the soil core
fn <- list.files("C:/Users/ybcheng/Documents/data2016/20160907ASD/", pattern="*resDB.csv")
resDB <- read.table(fn[1], header=TRUE, row.names=1, sep=",", check.names=FALSE)
new_specs <- as.matrix(resDB)
newData <- data.frame(specs=I(new_specs))
results <- predict(clay.pls, ncomp=5, newdata=newData) # this will generate estimated clay content for every depth


# this part is to prepare the results for output to csv
results <- as.data.frame(results)
rn <- rownames(results)
depth <- double()
for (i in seq(nrow(results))){
  d <- strsplit(rn[i],"_")[[1]]
  depth <- append(depth, as.numeric(d[6]))
}
results$Z <- depth


# EDIT the following parameters
# requires a database of core names and lat lon
specPerCore <- 28
loc_DB_file <- "C:/Users/ybcheng/Documents/data2016/20160907ASD/location.csv"

loc_DB <- read.table(loc_DB_file, header=TRUE, row.names = 1, sep = ",", check.names = FALSE)

lat <- double()
lon <- double()

for (i in seq(1,(nrow(results)-specPerCore+1), by=specPerCore)){
  core_name <- substr(rn[i], 1, nchar(rn[i])-4) #three digits for depth plus "_"
  tmp_lat <- rep(loc_DB[core_name, "Y"], times = specPerCore)
  tmp_lon <- rep(loc_DB[core_name, "X"], times = specPerCore)
  lat <- append(lat, tmp_lat)
  lon <- append(lon, tmp_lon)
}

#lat <- rep(-115.85, times = dim(results)[1])
#lon <- rep(33.35, times = dim(results)[1])

results$Y <- lat
results$X <- lon

results <- results[c(4,3,2,1)]
write.csv(results, "results.csv") #row.names = FALSE to rid of rownames


# use this if they want results per soil core
# EDIT the following parameters
# requires a database of core names and lat lon
specPerCore <- 28
loc_DB_file <- "C:/Users/ybcheng/Documents/data2016/20160907ASD/location.csv"

loc_DB <- read.table(loc_DB_file, header=TRUE, row.names = 1, sep = ",", check.names = FALSE)

lat <- double()
lon <- double()

for (i in seq(1,(nrow(results)-specPerCore+1), by=specPerCore)){
  core_name <- substr(rn[i], 1, nchar(rn[i])-4) #three digits for depth plus "_"
  sub_lat <- rep(loc_DB[core_name, "Y"], times=specPerCore)
  sub_lon <- rep(loc_DB[core_name, "X"], times=specPerCore)
  sub_results <- results[i:(i+specPerCore-1),]
  
  sub_results$Y <- sub_lat
  sub_results$X <- sub_lon
  
  sub_results <- sub_results[c(4,3,2,1)]
  sub_filename <- paste0(core_name, ".csv")
  write.csv(sub_results, sub_filename) #row.names = FALSE to rid of rownames
}










# the following part is a little tricky, works only if all props are non-NA
# but saves time for multiple target variables
props <- as.matrix(mydata[,1:2])
sum(complete.cases(props))  #counts if all props are non-NA
specs <- as.matrix(mydata[,3:ncol(mydata)])
mydf <- data.frame(props=I(props),specs=I(specs))