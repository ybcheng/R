# ******************************************* #
# This is an R program designed to perform    #
# partial least squre regression analysis on  #
# samples using the ASD FieldSpec4 for the    #
# IID Salton Sea  Project                     #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, July 2016                    #
# email: ybcheng@formationenv.com 	          #
# ******************************************* #
#
# Depends on files generated from ASD_perProc.R

rm(list=ls())

# EDIT this: set up working directory and where the files are
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_2_sub/AS_S1_C2_ver2/"
fd <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/AS_S1_C2_ver2/"
setwd(fd)


##################################################
# Use this section to read in multiple csv files
# and put them in a big dataframe       
#
# EDIT this: set up what filename pattern to search for
fpattern <- "*resDB_pls.csv"
der_fpattern <- "*derDB_pls.csv"
#
print("reading in csv files....")

fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)
der_fn <- list.files(fd, pattern = der_fpattern, include.dirs = TRUE, recursive = TRUE)
if (length(fn)==0 | length(der_fn)==0) {
  stop("NO files found!!!")
} else if (length(fn) != length(der_fn)) {
  stop("check database")
} 
print(fn)

mydata <- data.frame()
der_mydata <- data.frame()
for (f in fn){
  tmp <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  mydata <- rbind.data.frame(mydata, tmp)
}
for (d in der_fn){
  der_tmp <- read.table(d, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  der_mydata <- rbind.data.frame(der_mydata, der_tmp)
}  

print("dataframe assembled for analysis")
print("")


######################################################
# this following are some examples to perform
# partial least square regression on the big dataframe
#
library(pls)

# EDIT this part if neccessary
# first prepare the variables from the database 
clay <- mydata[,3]
silt <- mydata[,2]
sand <- mydata[,1]
specs <- as.matrix(mydata[,4:ncol(mydata)])

# then put them together in a dataframe
mydf <- data.frame(clay=clay, silt=silt, sand=sand, specs=I(specs))
# or
#mydf <- data.frame(clay=clay, silt=silt)
#mydf$specs <- specs
# will generate the same dataframe


##########################################
# the following part is to randomly select part of the dataframe as training data and
# the rest as testing data
#
# EDIT this: the percentage of the data to be used as training data
perc_train <- 0.7

vect_train <- sort(sample(nrow(mydf), round(nrow(mydf)*perc_train)))
mydf_train <- mydf[vect_train,]
mydf_test <- mydf[-vect_train,]


######
# CLAY
clay.pls.list <- rep(list(),10)
for (i in seq(10)){
  clay.pls.list[[i]] <- plsr(clay~specs, 20, data=mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(clay.pls.list[[i]])
  print("")
}

clay.pls <- plsr(clay~specs, 20, data=mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(clay.pls)
clay.pls$validation$PRESS #showing PRESS values
plot(RMSEP(clay.pls), legendpos="topright")
plot(clay.pls, ncomp=11, asp=1, line=TRUE)
write.csv(clay.pls$fitted.values, "clay_pls_fit_values.csv")
plot(clay.pls, plottype = "scores", comps = 1:3)
plot(clay.pls, "loadings", comps = 1:5, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)


#########
# setting up new test data and test the pls model with new data
fn_test = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/AN_ST/resDB_pls.csv"
mydata_test <- read.table(fn_test, header=TRUE, row.names=1, sep=",", check.names=FALSE)
clay_test <- mydata_test[,3]
silt_test <- mydata_test[,2]
sand_test <- mydata_test[,1]
specs_test <- as.matrix(mydata_test[,4:ncol(mydata_test)])
mydf_test <- data.frame(clay=clay_test, silt=silt_test, sand=sand_test, specs=I(specs_test))

predict(clay.pls, ncomp=11, newdata=mydf_test) #this requires setting up a test dataset clayTest
RMSEP(clay.pls, newdata = mydf_test)


######
# SAND
sand.pls.list <- rep(list(),10)
for (i in seq(10)){
  sand.pls.list[[i]] <- plsr(sand~specs, 20, data=mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(sand.pls.list[[i]])
  print("")
}

sand.pls <- plsr(sand~specs, 20, data=mydf, na.action=na.omit, validation="LOO")
summary(sand.pls)
sand.pls$validation$PRESS #showing PRESS values
plot(RMSEP(sand.pls), legendpos="topright")
plot(sand.pls, ncomp=9, asp=1, line=TRUE)
write.csv(sand.pls$fitted.values, "sand_pls_fit_values.csv")
plot(sand.pls, plottype = "scores", comps = 1:3)
plot(sand.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

predict(sand.pls, ncomp=10, newdata=sandTest) #this requires setting up a test dataset clayTest
RMSEP(sand.pls, newdata = sandTest)


######
# SILT
silt.pls.list <- rep(list(),10)
for (i in seq(10)){
  silt.pls.list[[i]] <- plsr(silt~specs, 20, data=mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(silt.pls.list[[i]])
  print("")
}

silt.pls <- plsr(silt~specs, 20, data=mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(silt.pls)
silt.pls$validation$PRESS #showing PRESS values
plot(RMSEP(silt.pls), legendpos="topright")
plot(sand.pls, ncomp=40, asp=1, line=TRUE)
plot(sand.pls, plottype = "scores", comps = 5:10)
plot(sand.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

predict(silt.pls, ncomp=10, newdata=siltTest) #this requires setting up a test dataset clayTest
RMSEP(silt.pls, newdata = siltTest)




######################################################
# this following are some examples to perform
# partial least square regression on the big dataframe
# mostly the save as above BUT using DERIVATIVE spectra instead
#
library(pls)

# EDIT this part if neccessary
# first prepare the variables from the database 
der_clay <- der_mydata[,3]
der_silt <- der_mydata[,2]
der_sand <- der_mydata[,1]
der_specs <- as.matrix(der_mydata[,4:ncol(der_mydata)])

# then put them together in a dataframe
der_mydf <- data.frame(der_clay=der_clay, der_silt=der_silt, der_sand=der_sand, der_specs=I(der_specs))
# or
#der_mydf <- data.frame(der_clay=der_clay, der_sand=der_sand)
#der_mydf$der_specs <- der_specs
# will generate the same dataframe


##########################################
# the following part is to randomly select part of the dataframe as training data and
# the rest as testing data
#
# EDIT this: the percentage of the data to be used as training data
perc_train <- 0.7

der_vect_train <- sort(sample(nrow(der_mydf), round(nrow(der_mydf)*perc_train)))
der_mydf_train <- der_mydf[der_vect_train,]
der_mydf_test <- der_mydf[-der_vect_train,]


##########
# DER_CLAY
der_clay.pls.list <- rep(list(),10)
for (i in seq(10)){
  der_clay.pls.list[[i]] <- plsr(der_clay~der_specs, 20, data=der_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(der_clay.pls.list[[i]])
  print("")
}

der_clay.pls <- plsr(der_clay~der_specs, 20, data=der_mydf, na.action=na.omit, validation="LOO") 
summary(der_clay.pls)
der_clay.pls$validation$PRESS #showing PRESS values
plot(RMSEP(der_clay.pls), legendpos="topright")
plot(der_clay.pls.list[[2]], ncomp=12, asp=1, line=TRUE)
plot(der_clay.pls, plottype = "scores", comps = 5:10)
plot(der_clay.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)


#########
# setting up new test data and test the pls model with new data
der_fn_test = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/AN_ST/derDB_pls.csv"
der_mydata_test <- read.table(der_fn_test, header=TRUE, row.names=1, sep=",", check.names=FALSE)
der_clay_test <- der_mydata_test[,3]
der_silt_test <- der_mydata_test[,2]
der_sand_test <- der_mydata_test[,1]
der_specs_test <- as.matrix(der_mydata_test[,4:ncol(der_mydata_test)])
der_mydf_test <- data.frame(der_clay=der_clay_test, der_silt=der_silt_test, der_sand=der_sand_test, der_specs=I(der_specs_test))

predict(der_clay.pls.list[[1]], ncomp=11, newdata=der_mydf_test) #this requires setting up a test dataset clayTest
RMSEP(der_clay.pls.list[[1]], newdata = der_mydf_test)


##########
# DER_SAND
der_sand.pls.list <- rep(list(),10)
for (i in seq(10)){
  der_sand.pls.list[[i]] <- plsr(der_sand~der_specs, 20, data=der_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(der_sand.pls.list[[i]])
  print("")
}

der_sand.pls <- plsr(der_sand~der_specs, 20, data=der_mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(der_sand.pls)
der_sand.pls$validation$PRESS #showing PRESS values
plot(RMSEP(der_sand.pls), legendpos="topright")
plot(der_sand.pls.list[[1]], ncomp=12, asp=1, line=TRUE)
plot(der_sand.pls, plottype = "scores", comps = 5:10)
plot(der_sand.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

#this requires setting up a test dataset sandTest
predict(der_sand.pls.list[[5]], ncomp=12, newdata=der_mydf_test) 
RMSEP(der_sand.pls.list[[4]], newdata = der_mydf_test)


##########
# DER_SILT
der_silt.pls.list <- rep(list(),10)
for (i in seq(10)){
  der_silt.pls.list[[i]] <- plsr(der_silt~der_specs, 20, data=der_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(der_silt.pls.list[[i]])
  print("")
}

der_silt.pls <- plsr(der_silt~der_specs, 20, data=der_mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(der_silt.pls)
der_silt.pls$validation$PRESS #showing PRESS values
plot(RMSEP(der_silt.pls), legendpos="topright")
plot(der_sand.pls, ncomp=20, asp=1, line=TRUE)
plot(der_sand.pls, plottype = "scores", comps = 5:10)
plot(der_sand.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

predict(der_silt.pls, ncomp=20, newdata=der_siltTest) #this requires setting up a test dataset clayTest
RMSEP(der_silt.pls, newdata = der_siltTest)




#############################################################
# this part is to read resDB (or derDB) back in and use the pls model to 
# estimate clay/slit/sand content for every depth along the soil core
# EDIT filenames and etc
#fn <- list.files("K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/AS_S1_C1_ver2/", pattern="*derDB.csv")
fn <- list.files("K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/VD/", pattern="*derDB.csv")
newDB <- read.table(fn[1], header=TRUE, row.names=1, sep=",", check.names=FALSE)
new_specs <- as.matrix(newDB)
newData <- data.frame(der_specs=I(new_specs))

library(pls)

# der_model AS_S1_C1_NR_N1_C1_S1_PR_N1_S1_CH_S2 (ALL data)
clay_results <- predict(der_clay.pls.list[[10]], ncomp=11, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[4]], ncomp=13, newdata=newData)

# der_model AS_S1_C1_NR_N1_C1_S1_PR_N1_S1 (ALL data)
clay_results <- predict(der_clay.pls.list[[3]], ncomp=11, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[9]], ncomp=13, newdata=newData)

# der_model AS_S1_C1_NR_N1_C1_S1_v2 (ALL data)
clay_results <- predict(der_clay.pls.list[[9]], ncomp=9, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[8]], ncomp=13, newdata=newData)

# der_model CP_NE_NW_SE1_SC1_2_SW
clay_results <- predict(der_clay.pls.list[[5]], ncomp=9, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[9]], ncomp=10, newdata=newData)

# der_model CP_NE_NW_SE1_SC1_2_SW_v2
clay_results <- predict(der_clay.pls.list[[5]], ncomp=7, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[4]], ncomp=9, newdata=newData)

# der_model AS_S1_C1_NR_N1_C1_S1
clay_results <- predict(der_clay.pls.list[[8]], ncomp=11, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[1]], ncomp=11, newdata=newData)

# der_model NR_N1_C1_S1
clay_results <- predict(der_clay.pls.list[[10]], ncomp=11, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[5]], ncomp=9, newdata=newData)

# der_model AS_S1_C1_ver2
clay_results <- predict(der_clay.pls.list[[6]], ncomp=12, newdata=newData) # this will generate estimated clay content for every depth
sand_results <- predict(der_sand.pls.list[[10]], ncomp=10, newdata=newData)

# der_model AS_S1_C1_ver1
clay_results <- predict(der_clay.pls.list[[9]], ncomp=12, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[9]], ncomp=10, newdata=newData)

# der_model CH_S2_refined
clay_results <- predict(der_clay.pls.list[[5]], ncomp=10, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[7]], ncomp=10, newdata=newData)




# re-arrange results for output to csv files ------------------------------

results <- as.data.frame(clay_results)
rn <- rownames(results)
depth <- double()
for (i in seq(nrow(results))){
  d <- strsplit(rn[i],"_")[[1]]
  depth <- append(depth, as.numeric(d[6]))
}
results$Z <- depth
results$sand <- sand_results
results <- round(results[c(2,3,1)], 0)
#write.csv(results, "results.csv")

# USE this part if Lat/Lon X/Y is desired in the results.csv
# EDIT the following parameters
# requires a database of core names and lat lon
specPerCore <- 30
loc_DB_file <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/All_Soil_Core_Actual_Locations.csv"

loc_DB <- read.table(loc_DB_file, header=TRUE, row.names = 2, sep = ",", check.names = FALSE)

lat <- double()
lon <- double()

for (i in seq(1,(nrow(results)-specPerCore+1), by=specPerCore)){
  core_name <- substr(rn[i], 1, nchar(rn[i])-4) #three digits for depth plus "_"
  tmp_lat <- rep(loc_DB[core_name, "Y"], times = specPerCore)
  tmp_lon <- rep(loc_DB[core_name, "X"], times = specPerCore)
  lat <- append(lat, tmp_lat)
  lon <- append(lon, tmp_lon)
}

results$Y <- lat
results$X <- lon

results <- results[c(5,4,1,2,3)]
write.csv(results, "results_AS_C23_003.csv") #row.names = FALSE to rid of rownames




# alternative output format -----------------------------------------------

# use this if they want results per soil core
# EDIT the following parameters
# requires a database of core names and lat lon
specPerCore <- 30
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




# testing area ------------------------------------------------------------

# the following part is a little tricky, works only if all props are non-NA
# but saves time for multiple target variables
props <- as.matrix(mydata[,1:2])
sum(complete.cases(props))  #counts if all props are non-NA
specs <- as.matrix(mydata[,3:ncol(mydata)])
mydf <- data.frame(props=I(props),specs=I(specs))