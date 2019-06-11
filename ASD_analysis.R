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
# Depends on files generated from ASD_sub.R

rm(list=ls())




# load previsouly developed workspace -------------------------------------
#load previsouly developed workspace
# "model #1" :
#load("K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_2/AS_S1_C1_NR_N1_C1_S1_PR_N1_S1_CH_S2/AS_S1_C1_NR_N1_C1_S1_PR_N1_S1_CH_S2.RData")
# "model #2" : 
#load("K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_2/AS_S1_C1_NR_N1_C1_S1_PR_N1_S1/AS_S1_C1_NR_N1_C1_S1_PR_N1_S1.RData")
# "San Felipe Wash S and C blocks" : 
load("K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash_V2/SFW_S1_S2_C1_C2_v2.RData")





# EDIT the parameters -----------------------------------------------------

# EDIT this: set up working directory and where the files are
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_2/NR_N1_C1_S1/"
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_2/AS_S1_C1_ver2/"
#fd <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/VD_117_122/"
#fd <- "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/4PLS/SW_ALL/"
#fd <- "K:/IID_SaltonSea/Tasks/PotentialPilotStudies/PoeRoad/ASD/Processing/4PLS/PR_025_027/"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/PoeRoad/"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash/SFW_S2_013_021/"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/PR_TS_V4/TrifoliumStorm_V4"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/PR_TS/PoeRoad_V2"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/TrifoliumStorm_V2"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/AlamoRiverSouth"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash/SFW_C1_C2_V2"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash_N2_N1"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/WisterFrink_B1"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/VailDrain_V2"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/BB_y_PA"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/AS_S1_S2_C1_C3"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/AS_LR_OB"

setwd(fd)


##################################################
# Use this section to read in multiple csv files
# and put them in a big dataframe       
#
# EDIT this: set up what filename pattern to search for
fpattern <- "*resDB_pls.csv"
der_fpattern <- "*derDB_pls.csv"
cr_fpattern <- "*crDB_pls.csv"
#
print("reading in csv files....")

fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)
der_fn <- list.files(fd, pattern = der_fpattern, include.dirs = TRUE, recursive = TRUE)
cr_fn <- list.files(fd, pattern = cr_fpattern, include.dirs = TRUE, recursive = TRUE)

if (length(fn)==0 | length(der_fn)==0 | length(cr_fn)==0) {
  stop("NO files found!!!")
} else if (length(fn) != length(der_fn)) {
  stop("check database")
} 
print(fn)
print(der_fn)
print(cr_fn)

mydata <- data.frame()
der_mydata <- data.frame()
cr_mydata <- data.frame()

for (f in fn){
  tmp <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  mydata <- rbind.data.frame(mydata, tmp)
}
for (d in der_fn){
  der_tmp <- read.table(d, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  der_mydata <- rbind.data.frame(der_mydata, der_tmp)
}
for (c in cr_fn){
  cr_tmp <- read.table(c, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  cr_mydata <- rbind.data.frame(cr_mydata, cr_tmp)
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
  clay.pls.list[[i]] <- plsr(clay~specs, 15, data=mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(clay.pls.list[[i]])
  print("")
}

clay.pls <- plsr(clay~specs, 20, data=mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(clay.pls)
clay.pls$validation$PRESS #showing PRESS values
plot(RMSEP(clay.pls), legendpos="topright")
plot(clay.pls, ncomp=4, asp=1, line=TRUE)
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
  sand.pls.list[[i]] <- plsr(sand~specs, 15, data=mydf, na.action=na.omit, validation="CV")
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
# mostly the same as above BUT using DERIVATIVE spectra instead
#
library(pls)

#for (i in 2:(ncol(der_mydata)-1)){
#  tmp <- (der_mydata[i+1]-der_mydata[i-1])/(as.integer(colnames(der_mydata[i+1]))-as.integer(colnames(der_mydata[i-1])))
#  tmpName <- paste0(colnames(der_mydata[i]),"0")
#  if (i==2){
#    derDB2 <- tmp
#    derNames2 <- tmpName
#  } else{
#    derDB2 <- cbind(derDB2, tmp)
#    derNames2 <- append(derNames2, tmpName)
#  }
#}
#colnames(derDB2) <- derNames2
#derDB2 <- derDB2[,5:211]
#der_mydata <- cbind.data.frame(der_mydata, derDB2)

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
plot(der_clay.pls.list[[5]], ncomp=6, asp=1, line=TRUE)
plot(der_clay.pls, plottype = "scores", comps = 5:10)
plot(der_clay.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)
write.csv(der_clay.pls.list[[8]]$fitted.values, "clay_pls_fit_values.csv")


#########
# setting up new test data and test the pls model with new data
der_fn_test = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/AN_ST/derDB_pls.csv"
der_mydata_test <- read.table(der_fn_test, header=TRUE, row.names=1, sep=",", check.names=FALSE)
der_clay_test <- der_mydata_test[,3]
der_silt_test <- der_mydata_test[,2]
der_sand_test <- der_mydata_test[,1]
der_specs_test <- as.matrix(der_mydata_test[,4:ncol(der_mydata_test)])
der_mydf_test <- data.frame(der_clay=der_clay_test, der_silt=der_silt_test, der_sand=der_sand_test, der_specs=I(der_specs_test))

predict(der_clay.pls.list[[6]], ncomp=4, newdata=der_mydf_test) #this requires setting up a test dataset clayTest
RMSEP(der_clay.pls.list[[6]], newdata = der_mydf_test)


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
plot(RMSEP(der_sand.pls.list[[4]]), legendpos="topright")
plot(der_sand.pls.list[[4]], ncomp=7, asp=1, line=TRUE)
plot(der_sand.pls, plottype = "scores", comps = 5:10)
plot(der_sand.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

write.csv(der_sand.pls.list[[5]]$fitted.values, "sand_pls_fit_values.csv")

#this requires setting up a test dataset sandTest
predict(der_sand.pls.list[[5]], ncomp=12, newdata=der_mydf_test) 
RMSEP(der_sand.pls.list[[4]], newdata = der_mydf_test)


##########
# DER_SILT
der_silt.pls.list <- rep(list(),10)
for (i in seq(10)){
  der_silt.pls.list[[i]] <- plsr(der_silt~der_specs, 30, data=der_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(der_silt.pls.list[[i]])
  print("")
}

der_silt.pls <- plsr(der_silt~der_specs, 15, data=der_mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(der_silt.pls)
der_silt.pls$validation$PRESS #showing PRESS values
plot(RMSEP(der_silt.pls), legendpos="topright")
plot(der_silt.pls.list[[10]], ncomp=20, asp=1, line=TRUE)
plot(der_silt.pls, plottype = "scores", comps = 5:10)
plot(der_silt.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)

predict(der_silt.pls, ncomp=20, newdata=der_siltTest) #this requires setting up a test dataset clayTest
RMSEP(der_silt.pls, newdata = der_siltTest)




# -------------------------------------------------------------------------
# this following are some examples to perform
# partial least square regression on the big dataframe
# mostly the same as above BUT using Continuum Removal spectra instead
#
library(pls)

# EDIT this part if neccessary
# first prepare the variables from the database 
cr_clay <- cr_mydata[,3]
cr_silt <- cr_mydata[,2]
cr_sand <- cr_mydata[,1]
cr_specs <- as.matrix(cr_mydata[,4:ncol(cr_mydata)])

# then put them together in a dataframe
cr_mydf <- data.frame(cr_clay=cr_clay, cr_silt=cr_silt, cr_sand=cr_sand, cr_specs=I(cr_specs))
# or
#der_mydf <- data.frame(der_clay=der_clay, der_sand=der_sand)
#der_mydf$der_specs <- der_specs
# will generate the same dataframe


##########################################
# the following part is to randomly select part of the dataframe as training data and
# the rest as testing data
#
# EDIT this: the percentage of the data to be used as training data
#perc_train <- 0.7
#
#cr_vect_train <- sort(sample(nrow(cr_mydf), round(nrow(cr_mydf)*perc_train)))
#cr_mydf_train <- cr_mydf[cr_vect_train,]
#cr_mydf_test <- cr_mydf[-cr_vect_train,]


##########
# CR_CLAY
cr_clay.pls.list <- rep(list(),10)
for (i in seq(10)){
  cr_clay.pls.list[[i]] <- plsr(cr_clay~cr_specs, 20, data=cr_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(cr_clay.pls.list[[i]])
  print("")
}

cr_clay.pls <- plsr(cr_clay~cr_specs, 20, data=cr_mydf, na.action=na.omit, validation="LOO") 
summary(cr_clay.pls)
cr_clay.pls$validation$PRESS #showing PRESS values
plot(RMSEP(cr_clay.pls), legendpos="topright")
plot(cr_clay.pls.list[[10]], ncomp=7, asp=1, line=TRUE)
plot(cr_clay.pls, plottype = "scores", comps = 5:10)
plot(cr_clay.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)


##########
# CR_SAND
cr_sand.pls.list <- rep(list(),10)
for (i in seq(10)){
  cr_sand.pls.list[[i]] <- plsr(cr_sand~cr_specs, 20, data=cr_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(cr_sand.pls.list[[i]])
  print("")
}

cr_sand.pls <- plsr(cr_sand~cr_specs, 20, data=cr_mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(cr_sand.pls)
cr_sand.pls$validation$PRESS #showing PRESS values
plot(RMSEP(cr_sand.pls), legendpos="topright")
plot(cr_sand.pls.list[[6]], ncomp=17, asp=1, line=TRUE)
plot(cr_sand.pls, plottype = "scores", comps = 5:10)
plot(cr_sand.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)


##########
# CR_SILT
cr_silt.pls.list <- rep(list(),10)
for (i in seq(10)){
  cr_silt.pls.list[[i]] <- plsr(cr_silt~cr_specs, 30, data=cr_mydf, na.action=na.omit, validation="CV")
  print(paste0(i, " run"))
  summary(cr_silt.pls.list[[i]])
  print("")
}

cr_silt.pls <- plsr(cr_silt~cr_specs, 30, data=cr_mydf, na.action=na.omit, validation="LOO") #validation="CV"
summary(cr_silt.pls)
cr_silt.pls$validation$PRESS #showing PRESS values
plot(RMSEP(cr_silt.pls), legendpos="topright")
plot(cr_silt.pls.list[[7]], ncomp=16, asp=1, line=TRUE)
plot(cr_silt.pls, plottype = "scores", comps = 5:10)
plot(cr_silt.pls, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)




#############################################################
# this part is to read resDB (or derDB or crDB) back in and use the pls model to 
# estimate clay/slit/sand content for every depth along the soil core
# EDIT filenames and etc
#fn <- list.files("K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/AS_S1_C1_ver2/", pattern="*derDB.csv")
#fn <- list.files("K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/VD/", pattern="*derDB.csv")
#fn <- list.files("K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/4PLS/SW_10_24/", pattern="*derDB.csv")
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/BombayBeach/20190501"
setwd(fd)

#############################################################
# using dervative spetra
fn <- list.files(fd, pattern="*derDB.csv")

if (length(fn)==0) {
  stop("NO files found!!!")
}

newDB <- read.table(fn[1], header=TRUE, row.names=1, sep=",", check.names=FALSE)
new_specs <- as.matrix(newDB)
newData <- data.frame(der_specs=I(new_specs))


#############################################################
# using continuum removal spectra
fn <- list.files(fd, pattern="*crDB.csv")
if (length(fn)==0) {
  stop("NO files found!!!")
}

newDB <- read.table(fn[1], header=TRUE, row.names=1, sep=",", check.names=FALSE)
new_specs <- as.matrix(newDB)
newData <- data.frame(cr_specs=I(new_specs))


# apply selected model to derive estimated values
library(pls)


# der_model_BB_PA 
clay_results <- predict(der_clay.pls.list[[5]], ncomp=6, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[4]], ncomp=7, newdata=newData)

# der_model_AS_LR_OB 
clay_results <- predict(der_clay.pls.list[[6]], ncomp=7, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[2]], ncomp=8, newdata=newData)

# der_model_SFW_N2_N1_V2 
clay_results <- predict(der_clay.pls.list[[5]], ncomp=9, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[6]], ncomp=5, newdata=newData)

# der_model_SFW_N2_N1 
clay_results <- predict(der_clay.pls.list[[2]], ncomp=12, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[7]], ncomp=5, newdata=newData)

# der_model_VD_2017y2019 
clay_results <- predict(der_clay.pls.list[[6]], ncomp=4, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[2]], ncomp=3, newdata=newData)

# der_model_WF_B1 
clay_results <- predict(der_clay.pls.list[[8]], ncomp=6, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[10]], ncomp=7, newdata=newData)

# der_model_SFW_S1_S2_C1_C2 
clay_results <- predict(der_clay.pls.list[[5]], ncomp=10, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[4]], ncomp=5, newdata=newData)

# der_model_TS (201811xx data)
clay_results <- predict(der_clay.pls.list[[9]], ncomp=7, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[8]], ncomp=9, newdata=newData)

# der_model_PR (201811xx data)
clay_results <- predict(der_clay.pls.list[[7]], ncomp=4, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[1]], ncomp=4, newdata=newData)

# der_model_SW (ALL data)
clay_results <- predict(der_clay.pls.list[[8]], ncomp=14, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[9]], ncomp=6, newdata=newData)

# der_model AS_S1_C1_NR_N1_C1_S1_PR_N1_S1_CH_S2 (ALL data) "model #1"
clay_results <- predict(der_clay.pls.list[[10]], ncomp=11, newdata=newData) 
sand_results <- predict(der_sand.pls.list[[4]], ncomp=13, newdata=newData)

# der_model AS_S1_C1_NR_N1_C1_S1_PR_N1_S1 (ALL data)       "model #2"
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
clay_results <- predict(der_clay.pls.list[[4]], ncomp=12, newdata=newData) # this will generate estimated clay content for every depth
sand_results <- predict(der_sand.pls.list[[5]], ncomp=10, newdata=newData)
# der_model AS_S1_C1_ver2
#clay_results <- predict(der_clay.pls.list[[6]], ncomp=12, newdata=newData) # this will generate estimated clay content for every depth
#sand_results <- predict(der_sand.pls.list[[10]], ncomp=10, newdata=newData)

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

#optional adding temp X Y in the results
#results$X <- 1111
#results$Y <- 9999
#results <- round(results[c(4,5,2,3,1)], 0)

write.csv(results, "results_raw.csv")

# USE this part if Lat/Lon X/Y is desired in the results.csv
# EDIT the following parameters
# requires a database of core names and lat lon
specPerCore <- 30
#loc_DB_file <- "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/4PLS/SW_exact_core_locations.csv"
#loc_DB_file <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/VD_planned_locations.csv"
#loc_DB_file <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/geodata/20190510_SFW_coring_locations.csv"
#loc_DB_file <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/geodata/20190409_WF_coring_locations.csv"
#loc_DB_file <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/geodata/20190515_VD17y19_coring_locations.csv"
#loc_DB_file <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/geodata/20190510_All_core_locations.csv"
loc_DB_file <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/geodata/20190510_BB_core_locations.csv"


##check if row.names matches where soil core ID is
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
write.csv(results, "results_raw_2.csv") #row.names = FALSE to rid of rownames





# update exact field location ---------------------------------------------
rm(list=ls())

fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash/SFW_S2_007_012/"
setwd(fd)
fn <- "SFW_S2_007_012_wavg60_prelim.csv"
specPerCore <- 30

loc_DB_path <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash/"
loc_DB_file <- "SFW_coring_locations.csv"
loc_DB_file <- paste0(loc_DB_path, loc_DB_file)


results <- read.table(fn, header = TRUE, sep = ",", check.names = FALSE, row.names = 1)

rn <- rownames(results)
depth <- double()
for (i in seq(nrow(results))){
  d <- strsplit(rn[i],"_")[[1]]
  depth <- append(depth, as.numeric(d[6]))
}

loc_DB <- read.table(loc_DB_file, header=TRUE, row.names = 3, sep = ",", check.names = FALSE)
### make sure the core names are stored in the 2nd column ####

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

write.csv(results, gsub(".csv","_update.csv", fn), na="") #row.names = FALSE to rid of rownames

# alternative output format -----------------------------------------------

# use this if they want results per soil core
# EDIT the following parameters
# requires a database of core names and lat lon
specPerCore <- 30
c <- "C:/Users/ybcheng/Documents/data2016/20160907ASD/location.csv"

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





# this part is designed to compute and visualize lab data only ------------

library(ggplot2)

fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/SanFelipeWash_N2_N1"
setwd(fd)

fpattern <- "*resDB_pls.csv"
fn <- list.files(fd, pattern = fpattern, include.dirs = TRUE, recursive = TRUE)

if (length(fn)==0 | length(der_fn)==0 | length(cr_fn)==0) {
  stop("NO files found!!!")
} else if (length(fn) != length(der_fn)) {
  stop("check database")
}

print(fn)

mydata <- data.frame()
for (f in fn){
  tmp <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  mydata <- rbind.data.frame(mydata, tmp)
}

lab_data <- mydata[,1:3]
lab_data$fine <- lab_data$silt + lab_data$clay
lab_data$Z <- as.integer(substr(rownames(lab_data), nchar(rownames(lab_data))-3+1, nchar(rownames(lab_data))))

lab_data2 <- as.data.frame(subset(lab_data, select=c(fine)))
lab_data2$depth <- "All"
lab_data3 <- as.data.frame(subset(lab_data, Z<=60, select=c(fine)))
lab_data3$depth <- "Top60cm"

lab_plot <- rbind.data.frame(lab_data2, lab_data3)

plot <- ggplot(lab_plot, aes(x=fine, fill=depth))+
          #geom_histogram(data=lab_plot, color="white", alpha=0.6, binwidth=5, position="identity")+
          geom_histogram(data=lab_plot, color="white", alpha=0.6, binwidth=5, position="dodge")+
          scale_x_continuous(name="%Fine", breaks=seq(0,100,by=20))+
          geom_vline(xintercept=35.0, linetype="dashed", color="black", size=1)+
          geom_text(aes(x=33.0, y=5, label="35% Threshold"), colour="black", angle=90)
plot

# testing area ------------------------------------------------------------

# the following part is a little tricky, works only if all props are non-NA
# but saves time for multiple target variables
props <- as.matrix(mydata[,1:2])
sum(complete.cases(props))  #counts if all props are non-NA
specs <- as.matrix(mydata[,3:ncol(mydata)])
mydf <- data.frame(props=I(props),specs=I(specs))