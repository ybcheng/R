# ******************************************** #
# This script is designed to organize photos   #
# taken at the station according to core names #
# a csv file that has the core label in order  #
# is needed                                    #
#                                              #
# Formation Environmental                      #
# Yen-Ben Cheng, July 2016                     #
# email: ybcheng@formatinoenv.com 	           #
# ******************************************** #

rm(list=ls())

# EDIT the following parameters
fd <- "C:/Users/ybcheng/Documents/data2016/20160906/"
file_pattern <- "\\.jpg$"
files_per_core <- 16
core_name_file <- "core_names.txt"
#

if (dir.exists(fd) != TRUE){
  stop("Directory does not exist")
}

setwd(fd)

if (file.exists(core_name_file) != TRUE){
  stop("core name file does not exist")
}

# search files and sort by mtime
fn <- list.files(fd, pattern = file_pattern)
details <- file.info(fn)
details <- details[with(details, order(as.POSIXct(mtime))), ]
fn <- rownames(details)

core_names <- read.csv(core_name_file, header = FALSE)
core_names <- as.matrix(core_names)

if ((length(fn)/files_per_core) != length(core_names)){
  stop("Check photo files!!!")
} else{
  k <- 1
  for (i in seq(length(core_names))){
    if (file.exists(core_names[i]) == TRUE){
      print(paste0("Folder exists, skipping: ", core_names[i]))
    } else{
      dir.create(core_names[i])
      for (j in seq(k,(k+files_per_core-1))){
        move_to <- paste0("./", core_names[i], "/", fn[j])
        file.rename(from=fn[j], to=move_to)
      }
      k <- k + files_per_core
      print(paste0("processed: ", core_names[i]))
    }
  }
}


# This section is to rename the stitched photo
# so the filename matches the folder name (core name)
rm(list=ls())

# EDIT the following parameters
fd <- "C:/Users/ybcheng/Documents/data2016/20160906/"
file_pattern <- "stitch.jpg$"

setwd(fd)

if (dir.exists(fd) != TRUE){
  stop("Directory does not exist!!!")
} else{
  setwd(fd)
  fn <- list.files(fd, pattern = file_pattern, include.dirs = TRUE, recursive = TRUE)
  
  if (length(fn) == 0){
    stop("NO files found")
  } else{
    for (i in seq(length(fn))){
      from_name <- paste0("./", fn[i])
      to_name <- paste0("./", dirname(fn[i]), "/", dirname(fn[i]), ".jpg")
      file.rename(from = from_name, to = to_name)
      print(paste0("processed: ", to_name))
    }
  }
}

