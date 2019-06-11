# ******************************************* #
# This is an R program designed to preprocess #
# spectral measurement taken on soil core     #
# samples using the ASD FieldSpec4 for the    #
# IID Salton Sea  Project                     #
#                                             #
# Formation Environmental                     #
# Yen-Ben Cheng, October 2016                 #
# email: ybcheng@formatinoenv.com 	          #
# ******************************************* #
#
# depends on files generated from ASD_preProc.R
# simple script designed to combine multiple resDB.csv and derDB.csv
# for additional processing

rm(list=ls()) # clean start

# EDIT the following parameters, where the files are 
#fd <- "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/BombayBeach/"
#fd <- "K:/IID_SaltonSea/Tasks/PotentialPilotStudies/PoeRoad/ASD/Processing/4PLS"
#fd <- "K:/IID_SaltonSea/Tasks/PilotStudies/VailDrain/ASD/Processing/4PLS"
fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/BB_y_PA/"
#fd <- "K:/IID_SaltonSea/Tasks/Soil mapping/ASD/Processing/4PLS/AS_S1_S2_C1_C3/"


# set up environment ------------------------------------------------------

setwd(fd)
fn <- list.files(fd, pattern = '*resDB.csv',include.dirs = 'True', recursive = 'True')
der_fn <- list.files(fd, pattern = '*derDB.csv',include.dirs = 'True', recursive = 'True')
cr_fn <- list.files(fd, pattern = '*crDB.csv',include.dirs = 'True', recursive = 'True')
abs_fn <- list.files(fd, pattern = '*absDB.csv',include.dirs = 'True', recursive = 'True')
if (length(fn)==0  | length(der_fn)==0 | length(cr_fn)==0) {
  stop("NO files found!!!")
}


# reflectance DB ----------------------------------------------------------

resDB <- data.frame()
for (f in fn){
  tmp <- read.table(f, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  resDB <- rbind.data.frame(resDB, tmp)
  print(paste0("processed: ", f))
}

write.csv(resDB,"resDB.csv")
resDB_sub <- cbind(subset=NA, sand=NA, silt=NA, clay=NA, resDB)
write.csv(resDB_sub,"resDB_sub.csv")
print(paste0(length(fn), " resDB files processed"))


# derivative DB -----------------------------------------------------------

derDB <- data.frame()
for (d in der_fn){
  tmp <- read.table(d, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  derDB <- rbind.data.frame(derDB, tmp)
  print(paste0("processed: ", d))
}

write.csv(derDB,"derDB.csv")
print(paste0(length(der_fn), " derDB files processed"))


# continuum removal DB ----------------------------------------------------

crDB <- data.frame()
for (c in cr_fn){
  tmp <- read.table(c, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  crDB <- rbind.data.frame(crDB, tmp)
  print(paste0("processed: ", c))
}

write.csv(crDB,"crDB.csv")
print(paste0(length(cr_fn), " crDB files processed"))


# absolute reflectance DB -------------------------------------------------

absDB <- data.frame()
for (a in abs_fn){
  tmp <- read.table(a, header=TRUE, row.names=1, sep=",", check.names=FALSE)
  absDB <- rbind.data.frame(absDB, tmp)
  print(paste0("processed: ", a))
}

write.csv(absDB,"absDB.csv")
print(paste0(length(abs_fn), " absDB files processed"))
