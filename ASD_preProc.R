# ******************************************* #
# This is an R program designed to preprocess #
# spectral measurement taken on soil core     #
# samples using the ASD FieldSpec4 for the    #
# IID Salton Sea  Project                     #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, July 2016                    #
# email: ybcheng@formatinoenv.com 	          #
# ******************************************* #

rm(list=ls()) # clean start

# This section utilizes the ASD reader library  
# to read in raw ASD files                              
# assuming ASD files are separated in different folders 
# for different cores. The program will read everything 
# in and put them together                              
# 
library(asdreader)
#
# EDIT this: set up working directory and where the files are
fd <- "C:/Users/ybcheng/Documents/data2016/20160908ASD/"
#
print("reading in raw data....")

setwd(fd)
fn <- list.files(fd, pattern = '\\.asd$',include.dirs = 'True', recursive = 'True')
if (length(fn)==0) {
  stop("NO files found!!!")
} 
# write.csv(fn, "filenames.csv")

rawDB <- double()
for (i in 1:length(fn)) {
  tmp <- get_spectra(fn[i])
  rawDB <- rbind(rawDB, tmp)
  #print(paste("processed: ",fn[i]))
}
write.csv(rawDB, "rawDB.csv")
print("raw database compilation complete")
print("")


# check if the database is what we expected
#
# EDIT the expected number of files per core
rawPerCore <- 120
#
if (nrow(rawDB)%%rawPerCore != 0) {
  print("WARNING!!! check database!!!")
} else {
  print("database Looks Good!!!")
}
print("")


# create a database for soil core names
# may be useful for random sample selection
coreNames <- character()
for (i in seq(1,(length(fn)-rawPerCore+1), by=rawPerCore)){
  tmp <- dirname(fn[i])
  coreNames <- append(coreNames,tmp)
}
write.csv(coreNames, "coreNames.csv")


# this section is to take average of a given number 
# of spectra and create a new database
#
# EDIT the expected number of files to average
numAvg <- 4
#
print("calculating average spectra database....")

avgPerCore <- rawPerCore/numAvg
avgDB <- double()
for (i in seq(1,(nrow(rawDB)-numAvg+1),by=numAvg)) {
  tmp <- colMeans(rawDB[i:(i+numAvg-1),])
  avgDB <- rbind(avgDB, tmp)
  #print(paste("processed: rawDB #",i))
}
write.csv(avgDB, "avgDB.csv")
print("calculating average spectra database finished")
print("")


# this section is to calculate relative reflectance
# first and last spectrum for each core is white reference
# a linear interpolation will be performed to calculate
# white reference for each of the measurements
#
print("calculating relative reflectance....")

relDB <- double()
for (i in seq(1,(nrow(avgDB)-avgPerCore+1),by=avgPerCore)) {
  for (j in 1:(avgPerCore-2)) {
    tmp <- avgDB[i+j,]/(avgDB[i,]*((avgPerCore-1-j)/(avgPerCore-1))+avgDB[i+avgPerCore-1,]*(j/(avgPerCore-1)))
    #print(paste(((avgPerCore-1-j)/(avgPerCore-1))," , ",(j/(avgPerCore-1))))
    relDB <- rbind(relDB, tmp)
  }
  #print(paste("processed: avgDB #",i))
}
write.csv(relDB, "relDB.csv")
print("calculating relative reflectance finished")
print("")


# This section is to convert relative reflectance to absolute reflectance
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


# This section is to create labels for each of the spectral measurements
#
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
  fileNumb <- 1+ ((i-1)%%absPerCore+1)*4 + ((i-1)%/%absPerCore)*rawPerCore
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

print("spectral resampling finished")
print("")


# This section is to calculate first derivative spectra from
# the resampled 10nm resolution database
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


# This section utilizes prospecr library to calculate continuum removal on
# resampled database
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

