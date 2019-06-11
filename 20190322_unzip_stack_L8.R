rm(list=ls())

library("raster")

in_dir <- "I:/Landsat/CDR_zip/p039r037/"
tmp_dir <- "I:/Landsat/CDR_unzip/p039r037/YBC/"
cmd_dir <- "I:\\Landsat\\CDR_unzip\\p039r037\\YBC"
out_dir <- "I:/Landsat/CDR_unzip/p039r037/Landsat8/"
#out_dir <- "I:/GOES/20180416_IID/07_sandbox/"

winrarPath  = "C:/Program Files/7-Zip/7z.exe"


setwd(in_dir)

zip_list <- list.files(path=in_dir, pattern="LC8.*.gz$", recursive=FALSE)
for (z in zip_list){
  command  <- paste('"', winrarPath,'" x -aoa -y ',z, sep="")
  system(command, wait=TRUE, show.output.on.console=FALSE)
}

tar_list <- list.files(path=in_dir, pattern="LC8.*.tar$", recursive=FALSE)
for (t in tar_list){
  command  <- paste('"', winrarPath,'" e -aoa -y ',t, ' -o', cmd_dir, sep="")
  system(command, wait=TRUE, show.output.on.console=FALSE)
  
  tif_list <- list.files(path=tmp_dir, pattern=".*.sr_band.*.tif", full.names=TRUE, recursive=FALSE)
  if (length(tif_list) != 7){
    print(paste0("check: ", t))
    break
  }
  
  out_filename <- paste0(out_dir, strsplit(basename(tif_list[1]),"_")[[1]][4],"_l8_multi_cdr.tif")
  if (file.exists(out_filename) == TRUE){
    unlink(paste0(tmp_dir,"*"))
    file.remove(t)
    print(paste0(out_filename, " already exists!!"))
    next
  }
  
  sr_b1 <- raster(tif_list[1]) * 0.0001
  sr_b2 <- raster(tif_list[2]) * 0.0001
  sr_b3 <- raster(tif_list[3]) * 0.0001
  sr_b4 <- raster(tif_list[4]) * 0.0001
  sr_b5 <- raster(tif_list[5]) * 0.0001
  sr_b6 <- raster(tif_list[6]) * 0.0001
  sr_b7 <- raster(tif_list[7]) * 0.0001
  out_data <- stack(x=c(sr_b1, sr_b2, sr_b3, sr_b4, sr_b5, sr_b6, sr_b7))
  writeRaster(out_data, filename=out_filename, format="GTiff", overwrite=TRUE, options=c("COMPRESS=NONE"), datatype="FLT4S")
  print(paste0("generated: ", out_filename))
  
  unlink(paste0(tmp_dir,"*"))
  file.remove(t)
}  
  
  
  
