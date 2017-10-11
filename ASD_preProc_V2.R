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

# This version utilizes parabolically corrected radiance files
# from ViewSpec pro as raw input
#
# assuming ASD files are separated in different folders 
# for different cores. The program will read everything 
# in and put them together                              
# 
#library(asdreader)

# set up parameters and etc -----------------------------------------------

# EDIT this: set up working directory and where the files are
#fd <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/VD"
fd <- "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/4PLS/SW_1_9"
#fd <- "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/AN_ST"
#fd <- "C:/Users/ybcheng/Documents/data2017/20170116_ASD_pco"


# search and read in files ------------------------------------------------

print("reading in raw data....")

setwd(fd)
fn <- list.files(fd, pattern = '\\.asd$',include.dirs = 'True', recursive = 'True')
if (length(fn)==0) {
  stop("NO files found!!!")
} 
# write.csv(fn, "filenames.csv")

# read in file modified time for further processing
details <- file.info(fn)
ftime <- as.numeric(details$mtime)

# this section reads in parabolically corrected radiance files from Viewspec pro
# hence it depends on the output named "rawDB.csv"
rawDB_fn <- paste0(fd, "/rawDB.csv")
rawDB <- read.table(rawDB_fn, header=TRUE, row.names=1, sep=",", check.names=FALSE)
rawDB <- rawDB[ , colSums(is.na(rawDB)) == 0]
#rawDB[ , apply(rawDB, 2, function(x) !any(is.na(x)))]

#for (f in fn) {
#  tmp <- get_spectra(f)
#  rawDB <- rbind(rawDB, tmp)
  #print(paste("processed: ",fn[i]))
#}
#write.csv(rawDB, "rawDB.csv")
print("raw database read in")
print("")


# checking database -------------------------------------------------------

# EDIT the expected number of files per core
rawPerCore <- 64
#
if (nrow(rawDB)%%rawPerCore != 0) {
  stop("WARNING!!! check database!!!")
} else {
  print("database Looks Good!!!")
}
print("")


# create database for soil core names -------------------------------------

# may be useful for random sample selection
coreNames <- character()
for (i in seq(1,(length(fn)-rawPerCore+1), by=rawPerCore)){
  tmp <- dirname(fn[i])
  coreNames <- append(coreNames,tmp)
}
write.csv(coreNames, "coreNames.csv")


# take average of spectra and create a new database------------------------

# EDIT the expected number of files to average
numAvg <- 2
#
print("calculating average spectra database....")

avgPerCore <- rawPerCore/numAvg
avgDB <- double()
avgFTime <- double()
for (i in seq(1,(nrow(rawDB)-numAvg+1),by=numAvg)) {
  tmp <- colMeans(rawDB[i:(i+numAvg-1),])
  avgDB <- rbind(avgDB, tmp)
  tmpT <- mean(ftime[i],ftime[i+numAvg-1])
  avgFTime <- append(avgFTime, tmpT)
  #print(paste("processed: rawDB #",i))
}
write.csv(avgDB, "avgDB.csv")
print("calculating average spectra database finished")
print("")


# calculate relative reflectance ------------------------------------------

# first and last spectrum for each core is white reference
# a linear interpolation will be performed to calculate
# white reference for each of the measurements
# this version utilizes file acquisition Time for interpolation
#
print("calculating relative reflectance....")

relDB <- double()
for (i in seq(1,(nrow(avgDB)-avgPerCore+1),by=avgPerCore)) {
  for (j in 1:(avgPerCore-2)) {
    # below is simple linear interpolation
    #tmp <- avgDB[i+j,]/(avgDB[i,]*((avgPerCore-1-j)/(avgPerCore-1))+avgDB[i+avgPerCore-1,]*(j/(avgPerCore-1)))
    #print(paste(((avgPerCore-1-j)/(avgPerCore-1))," , ",(j/(avgPerCore-1))))
    #
    # this part uses file acquisition time to improve accuracy
    refT <- avgDB[i,] + (avgDB[i+avgPerCore-1,]-avgDB[i,])*(avgFTime[i+j]-avgFTime[i])/(avgFTime[i+avgPerCore-1]-avgFTime[i])
    tmp <- avgDB[i+j,]/refT
    relDB <- rbind(relDB, tmp)
  }
  #print(paste("processed: avgDB #",i))
}
write.csv(relDB, "relDB.csv")
print("calculating relative reflectance finished")
print("")


# convert relative reflectance to absolute reflectance --------------------

# first read in the coefficient vector that converts relative relfectance to absolute reflectance
#
# EDIT where to read in the coefficient to calculate absolute reflectance
absFile <- "C:/Users/ybcheng/Documents/R/SaltonSea/abs_refl.csv"
#
print("calculating absolute reflectance....")

absCoeff <- read.csv(absFile, header=FALSE)
colnames(absCoeff) <- colnames(relDB)

absDB <- double()
for (i in 1:nrow(relDB)) {
  tmp <- relDB[i,]*absCoeff
  absDB <- rbind(absDB, tmp)
  #print(paste("processed: relDB #", i))
}

print("calculating absolute reflectance finished")
print("")


# creating labels for each spectrum ---------------------------------------

# EDIT the start and steps of the sequence
absPerCore <- avgPerCore-2  #means first and last measurements are white panels
seqStart <- 5
seqEnd <- seqStart * absPerCore
seqStep <- 5
dLabels <- seq(seqStart, seqEnd, by=seqStep)
# convert integers to three charecters string
dLabels <- formatC(dLabels, width=3, flag="0")

print("creating labels for spectra....")

specLabels <- character()
for (i in 1:nrow(absDB)){
  fileNumb <- 1+ ((i-1)%%absPerCore+1)*numAvg + ((i-1)%/%absPerCore)*rawPerCore
  #print(fileNumb)
  #orgFn <- strsplit(dirname(fn[fileNumb]),"-")[[1]]
  tmp <- paste0(dirname(fn[fileNumb]), "_", dLabels[((i-1)%%absPerCore+1)])
  specLabels <- append(specLabels, tmp)
}
#print(specLabels)

rownames(absDB) <- specLabels
write.csv(absDB,"absDB.csv")

print("creating labels for spectra finsihed")
print("")



# spctral resampling on absolute reflectance ------------------------------

# This section is to perform spectral average on absolute reflectance database
# It utilizes the "resample2" function in the "prospectr" library
# FWHM is assumed to be the same as sampling interval
#
library(prospectr)
#
# EDIT this part to specify the new resampled wavelength
resWav <- seq(370, 2480, by=10)
#
print("spectral resampling begins....")

wav <- as.numeric(colnames(absDB))
resDB <- resample2(absDB, wav, resWav)
resDB <- as.data.frame(resDB)
write.csv(resDB,"resDB.csv")

resDB_sub <- cbind(subset=NA, sand=NA, silt=NA, clay=NA, resDB)
write.csv(resDB_sub,"resDB_sub.csv")

print("spectral resampling finished")
print("")



# calculate first derivative spectra --------------------------------------

# from the resampled 10nm resolution database
#
print("calculating 1st derivative spectra begin....")

for (i in 2:(ncol(resDB)-1)){
  tmp <- (resDB[i+1]-resDB[i-1])/(as.integer(colnames(resDB[i+1]))-as.integer(colnames(resDB[i-1])))
  tmpName <- colnames(resDB[i])
  if (i==2){
    derDB <- tmp
    derNames <- tmpName
  } else{
    derDB <- cbind(derDB, tmp)
    derNames <- append(derNames, tmpName)
  }
}
colnames(derDB) <- derNames
write.csv(derDB,"derDB.csv")

print("calculating derivative spectra finished")
print("")


# calculating continuum removal -------------------------------------------

# on resampled database
#
library(prospectr)
#
print("calculating continuum removal begins")

crDB <- continuumRemoval(resDB)
crDB <- as.data.frame(crDB)
colnames(crDB) <- colnames(resDB)
write.csv(crDB, "crDB.csv")

print("calculating continuum removal finished")
print("")
print("ALL DONE!!!")
