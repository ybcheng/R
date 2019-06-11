rm(list=ls())

# User Inputs
########################################################################################################################

FGDB       = "K:/IID_SaltonSea/Tasks/Task1b_EmissionsInventory/geodata/vector/20190325_L8sub_4optram.gdb"
FC_name    = "surfM"
shp_mask   = "K:/IID_SaltonSea/Tasks/Task1b_EmissionsInventory/geodata/vector/20181223_surfM2.shp"
#mask out 0)Developed; 4)Cobbles (Alluvial); 11)Sandstone; 12)Bedrock; 18)Open Water

#in_Folder  = "K:/IID_SaltonSea/Tasks/Task1b_EmissionsInventory/geodata/raster"
#in_Folder = "I:/Landsat/CDR_unzip/p039r037/Landsat8/dev_1819"
in_Folder = "I:/Landsat/CDR_unzip/p041r035/OWens_OPTRAM_L8"
img_list = "optram_list_1718.txt"
#Output_Folder  = "K:/CDFW/Analysis/Sentinel/20181128_CDFW_AOIs_MemoryTest" # contains pre-downloaded compressed sentinel imagery
#WetlandID      = "WetIndID"
#script.dir     = "K:/CDFW/Deliver/DesktopVersion/Software"
crop=FALSE
res =TRUE
mask=TRUE

#######################################################################################################################
# Package Checking Function
########################################################################################################################

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) {
      print("Package not found")
      stop("Package not found")
    }
  }
}

########################################################################################################################
# Loading libraries
########################################################################################################################

pkgTest("rgdal")
pkgTest("sp")
pkgTest("ggplot2")
pkgTest("raster")
pkgTest("downloader")
pkgTest("spatialEco")
pkgTest("sf")
pkgTest("h2o")
pkgTest("stats")
pkgTest("data.table")
pkgTest("multiplex")
pkgTest("shinyjs")
pkgTest("velox")
pkgTest("gdalUtils")
pkgTest("rgeos")
pkgTest("MASS")
pkgTest("viridis")

########################################################################################################################
# Defining variables
########################################################################################################################

#FC_name = FeatureName
#workspace = in_Folder
setwd(in_Folder)
getwd()

#fn_list <- read.csv(img_list, header = FALSE, col.names=c("fname"))
fn_list <- list.files(path=in_Folder, pattern="2016.*.cdr.tif$")
print(fn_list)

########################################################################################################################
# if cropping is desired, read in the feature class
########################################################################################################################

if (crop){
  fc <- readOGR(dsn=FGDB,layer=FC_name)
}

########################################################################################################################
# read data in and preprocessing
########################################################################################################################
mydf <- data.frame()
#for (fn in fn_list$fname){
for (fn in fn_list){  
  red  <- raster(fn, band=4)
  nir  <- raster(fn, band=5)
  swir <- raster(fn, band=7)

  if(crop){
    sub_red  <- crop(red,  extent(fc))
    sub_nir  <- crop(nir,  extent(fc))
    sub_swir <- crop(swir, extent(fc))
  } else{
    sub_red  <- red
    sub_nir  <- nir
    sub_swir <- swir
  }

  if(mask){
    if(file.exists(paste0(in_Folder,"/mask.tif"))){
      msk_rast <- raster(paste0(in_Folder,"/mask.tif"))
    }else {
      e  <- extent(sub_red)
      e2 <- paste0(paste0(paste0(paste0(paste0(paste0(paste0(e[1], " "), e[3]), " "), e[2]), " "), e[4]), " ")
      print("Rasterizing surface map ...")
    
      #command <- paste0(paste0(paste0(paste0(paste0("gdal_rasterize -a IDx -ot Byte -tr 10 10  -te ", e2), paste(workspace,folders[f],sep="/")),"/wetlands.shp "),paste(workspace,folders[f],sep="/")),"/wetlands.tif")
      command <- paste0("gdal_rasterize -a IDx -ot Byte -tr 30 30  -te ", e2, shp_mask, " ", paste0(in_Folder,"/mask.tif"))
      system("cmd.exe",input=command, wait=TRUE, show.output.on.console=FALSE)
      msk_rast <- raster(paste0(in_Folder,"/mask.tif"))
    }
    msk_red  <- mask(x=sub_red,  mask=msk_rast, maskvalue=0) 
    msk_nir  <- mask(x=sub_nir,  mask=msk_rast, maskvalue=0)
    msk_swir <- mask(x=sub_swir, mask=msk_rast, maskvalue=0)
  } else{
    msk_red  <- sub_red 
    msk_nir  <- sub_nir
    msk_swir <- sub_swir
  }
  
  if(res){
    res_red  <- aggregate(msk_red,  fact=3, FUN=mean())
    res_nir  <- aggregate(msk_nir,  fact=3, FUN=mean())
    res_swir <- aggregate(msk_swir, fact=3, FUN=mean())
  } else{
    res_red  <- msk_red
    res_nir  <- msk_nir
    res_swir <- msk_swir
  }

  ndvi <- (res_nir-res_red)/(res_nir+res_red)
  STR <- ((1-res_swir)^2)/(2*res_swir)
  
  tmp_ndvi <- as.data.frame(rasterToPoints(ndvi))
  tmp_str <- as.data.frame(rasterToPoints(STR))

  tmp_df <- as.data.frame(cbind(tmp_ndvi$layer, tmp_str$layer))
  colnames(tmp_df) <- c("ndvi", "str")
  tmp_df$date <- rep(substr(fn,1,8), nrow(tmp_df))
  mydf <- rbind.data.frame(mydf, tmp_df)
  print(fn)
}


########################################################################################################################
# 
########################################################################################################################

dim(mydf)
plot(mydf$ndvi, mydf$str)

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


# ggplot 1 ------------------------------------------------------------------

y_cutoff <- 25
mydf2 <- subset(mydf, str<=y_cutoff)
dim(mydf2)
#plot(mydf2$ndvi, mydf2$str)

mydf2$density <- get_density(mydf2$ndvi, mydf2$str, n = 100)
#ggplot(mydf2) + geom_point(aes(ndvi, str))


# ggplot 2 ------------------------------------------------------------------

myplot <- ggplot(mydf2)+
  geom_point(aes(ndvi, str, color = density), size=0.5)+
  scale_color_viridis()
myplot

myplot <- myplot + coord_cartesian(ylim = c(0,20))+
  geom_hline(yintercept=0, linetype="dashed", color="grey", size=1)+
  geom_vline(xintercept=0, linetype="dashed", color="grey", size=1)
myplot

# model parameterization
# L8 17_18 results:
#int_d = 0.0
#slp_d = 1.3
#int_w = 2.5
#slp_w = 10.4

# L8 18_19 results:
# Owen's 20190514 results
#int_d <- -0.2
#slp_d <- 4.5
#int_w <- 5.0
#slp_w <- 9.2

myplot <- myplot + geom_abline(intercept=int_d, slope=slp_d, linetype="dashed", color="red", size=1)
myplot

myplot <- myplot + geom_abline(intercept=int_w, slope=slp_w, linetype="dashed", color="green3", size=1)
myplot

txt_d <- paste0("OLint_D:", int_d, "  ;  ", "OLslp_D:", slp_d)
txt_w <- paste0("OLint_W:", int_w, "  ;  ", "OLslp_W:", slp_w)
myplot <- myplot + annotate("text", x=0.6, y=6.0, label=txt_d, color="red",    fontface="bold")
myplot <- myplot + annotate("text", x=0.6, y=9.0, label=txt_w, color="green3", fontface="bold")
myplot






# post-analysis -- model implementation --------------------------------
# -- derive moisture from L8
in_dir = "I:/Landsat/CDR_unzip/p039r037/Landsat8/"
img_list = "optram_list_1819.txt"

FGDB       = "K:/IID_SaltonSea/Tasks/Task1b_EmissionsInventory/geodata/vector/20190325_L8sub_4optram.gdb"
FC_name    = "surfM"
shp_mask   = "K:/IID_SaltonSea/Tasks/Task1b_EmissionsInventory/geodata/vector/20181223_surfM2.shp"
tif_mask   = "I:/Landsat/CDR_unzip/p041r035/Owens_OPTRAM_L8//mask.tif"

crop=FALSE

#IID SS sand ######################
th_d <- 0.05
th_w <- 0.55

#1718 model
#int_d <- 0
#slp_d <- 1.3
#int_w <- 2.5
#slp_w <- 10.4

#1819 model
int_d <- 0
slp_d <- 1.3
int_w <- 2.5
slp_w <- 12
###################################


# Owens silty clay ################
#th_d <- 0.056
#th_w <- 0.423

#int_d <- 0
#slp_d <- 3.2
#int_w <- 8
#slp_w <- 9.2
##################################

paras <- data.frame(paras=c(th_d,th_w,int_d,slp_d,int_w,slp_w),row.names=c("th_d","th_w","int_d","slp_d","int_w","slp_w"))


#out_dir = "K:/IID_SaltonSea/Tasks/Task1b_EmissionsInventory/geodata/raster/L8_OPTRAM_2017_2018/"
out_dir = "I:/Landsat/CDR_unzip/p039r037/Landsat8/dev_1819/"
#out_dir = "I:/Landsat/CDR_unzip/p041r035/Owens_OPTRAM_L8/"

setwd(in_dir)
getwd()

fn_list <- read.csv(img_list, header = FALSE, col.names=c("fname"))
#fn_list <- list.files(path=in_Folder, pattern="2018.*.tif$")

########################################################################################################################
# if cropping is desired, read in the feature class

if (crop){
  fc <- readOGR(dsn=FGDB,layer=FC_name)
}

########################################################################################################################
# read data in and preprocessing

for (fn in fn_list$fname[5]){
  out_filename1 <- paste0(out_dir, gsub("multi_cdr.tif", "optram_W3.tif", fn))
  out_filename2 <- paste0(out_dir, gsub("multi_cdr.tif", "optram_th3.tif", fn))
  out_paraname  <- paste0(out_dir, gsub("multi_cdr.tif", "optram_para3.csv", fn))
  
  if(file.exists(out_filename1)){
    print(paste0("file exists, skipping: ", out_filename1))
    next
  }
  
  red <- raster(fn, band=4)
  nir <- raster(fn, band=5)
  swir <- raster(fn, band=7)
  ndvi <- (nir-red)/(nir+red)
  STR <- ((1-swir)^2)/(2*swir)
  
  w <- (int_d + slp_d*ndvi - STR) / (int_d - int_w + (slp_d - slp_w)*ndvi)
  lo <- w<0
  w[lo] <- NA
  hi <- w>1
  w[hi] <- NA
  
  if(crop){
    w_sub  <- crop(w,  extent(fc))
  }else {
    w_sub <- w
  }
  writeRaster(w_sub, filename=out_filename1, format="GTiff", overwrite=TRUE, options=c("COMPRESS=NONE"), datatype="FLT4S")
  
  th <- (th_w-th_d) * w_sub + th_d
  writeRaster(th,    filename=out_filename2, format="GTiff", overwrite=TRUE, options=c("COMPRESS=NONE"), datatype="FLT4S")
  
  write.table(paras, out_paraname, sep="\t")
  
  print(paste0("processed: ", out_filename2))
}
