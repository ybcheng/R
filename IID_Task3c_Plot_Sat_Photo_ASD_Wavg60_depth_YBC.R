
# the script will plot ONLY satellite imagery on top half
# core photo and ASD results on the bottom
# sand, silt, clay, fine and 60cm average of sand and fine
# the script put everything into one PDF file
# example input: IID_Task3c_Plot_Photo_ASD_fine_avg60_YBC.csv
# example output: IID_Task3c_Plot_Sat_Photo_ASD_avg60_YBC.pdf

##################################################
# This script is to create 2D soil variation map at XY Plane for Depth Range
# Use Original Collection Points as inputs
#
##################################################

rm(list=ls())

#Photo_Folder = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/PhotoDocumentation/Final"
#Photo_Folder = "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/PhotoDocumentation/Final"
Photo_Folder = "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/PhotoDocumentation/Final"

#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/AlamoRiverSouth/AS_S1_fine_avg.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/AlamoRiverSouth/AS_C1_C2_C3_fine_avg.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/AlamoRiverSouth/AS_N1_N2_fine_avg.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/NewRiver/NR_N1_C1_S1_v2_w_water_avg60.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/AlamoRiverNorth/AN_A1_fine_water_avg.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/AlamoRiverNorth/AN_ST_prelim_fine_water_avg.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/CoachellaPlaya/CP_fine_water_avg_V2.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS_results/ClubHouse/CH_refined_test.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/4PLS_results/AN_fine_water_avg60.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/4PLS_results/BB_fine_water_avg60.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/4PLS_results/SC_fine_water_avg60.csv"
#csv_file = "C:/Users/ybcheng/Documents/R/SaltonSea/examples_plotting/IID_Task3c_Plot_Photo_ASD_fine_avg60_YBC.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/4PLS/VD_ALL_prelim_results.csv"
csv_file = "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/4PLS/SW_ALL/SW_ALL_results_w60avg.csv"

#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverSouth/AS_S1_Charts_fine_avg.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverSouth/AS_C1_C2_C3_Charts_fine_avg.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverSouth/AS_N1_N2_Charts_fine_avg.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverNorth/AN_ST_Charts_fine_water_avg_prelim.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/NewRiver/NR_N1_C1_S1_Charts_V2.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/Coachella/CP_Charts_fine_water_avg_V2.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/ClubHouse/CH_Charts_test.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/Charts/AN_Charts_fine_water_avg.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/Charts/BB_Charts_fine_water_avg.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/Charts/SC_Charts_fine_water_avg.pdf"
#SavePDF = "C:/Users/ybcheng/Documents/R/SaltonSea/examples_plotting/IID_Task3c_Plot_Sat_Photo_ASD_avg60_YBC.pdf"
SavePDF = "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/Charts/SW_ALL_w60avg_Charts.pdf"
#SavePDF = "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/Charts/VD_ALL_Charts.pdf"

if (file.exists(SavePDF) == TRUE){
  file.remove(SavePDF)
}

SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/Charts/SaltonWash.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/Charts/VailDrain.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverSouth/AlamoRiverSouth.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverNorth/AlamoRiverNorth.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/NewRiver/NewRiver.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/Coachella/Coachella.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/ClubHouse/ClubHouse.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/Charts/BombayBeach.tif"
#SatelliteImage = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/ASD/Processing/Charts/SaltonCity.tif"

#EMImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/DualEM/Processing/ZVariation2D/AlamoRiverSouth/Raster2/InterpolatedRaster_Depth_From_0_To_1.6.tif"
#EMImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverNorth/EM_Dummy.tif"
#EMImage = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/DualEM/Processing/ZVariation2D/NewRiver/Raster/InterpolatedRaster_Depth_From_0_To_1.6.tif"

#Shpfilefolder = "K:/IID_SaltonSea/Tasks/Task3e_VailDrainFSPS/ASD/Processing/Charts"
Shpfilefolder = "K:/IID_SaltonSea/Tasks/Task3f_SaltonWashFieldStudy/ASD/Processing/Charts"
#Shpfilefolder = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/DualEM/Processing/SGEMS/AlamoRiverSouth/AOI"
#Shpfilefolder = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/AlamoRiverNorth"
#Shpfilefolder = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/DualEM/Processing/SGEMS/NewRiver/AOI"
#Shpfilefolder = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts/Coachella"

#Shpfile = "VD_new_aoi"
Shpfile = "SW_aoi"
#Shpfile = "AS_All"
#Shpfile = "AS_North"
#Shpfile= "NR_All"
#Shpfile = "Coachella_AOI"

##################################################

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(rgdal)
library(raster)
library(fields)
library(gridExtra)
library(jpeg)
library(rasterVis)
library(extrafont)
#library(colorschemes)
library(plotrix)

# Importing Data
iData <- read.csv(csv_file,header=T)
temp <- data.frame(do.call('rbind', strsplit(as.character(iData$FEID),'_',fixed=TRUE)))
Zmax <- max(iData$Z)

FEIDjpg <- paste(paste(paste(paste(paste(temp[,1],temp[,2], sep="_"),temp[,3], sep="_"),temp[,4], sep="_"),temp[,5], sep="_"),".jpg",sep="")
iData2 <- data.frame(iData,jpgName=FEIDjpg)

FEIDjpg_Unique <- unique(FEIDjpg)
JPGList <- Sys.glob("*.jpg")

pdf(file=SavePDF,width=8,height=11)

for (jpgfile in FEIDjpg_Unique) {
  
  #jpgpath = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/PhotoDocumentation/Final/IID20160913_AS_S1_001_SL.jpg"
  jpgpath <- paste(Photo_Folder,jpgfile,sep="/")
  jpgfilename <- basename(jpgpath)
  
  # Subset Data
  iData2_subset <- iData2[which(iData2$jpgName == jpgfilename),]
  ASD_subset <- data.frame(x=iData2_subset$Z, sand=iData2_subset$sand, silt=iData2_subset$silt, clay=iData2_subset$clay, fine=iData2_subset$fine) #, water=iData2_subset$water)
  #iData2_subset2 <- na.omit(iData2_subset)
  avg_subset <- data.frame(x=iData2_subset$Z, sand=iData2_subset$avg_sand, silt=iData2_subset$avg_silt, clay=iData2_subset$avg_clay, fine=iData2_subset$avg_fine) #, water=iData2_subset2$avg_water)
  avg_subset <- na.omit(avg_subset)
  wavg_subset <- data.frame(x=iData2_subset$Z, fine=iData2_subset$w_avg_fine)
  wavg_subset <- na.omit(wavg_subset)
  dpth_subset <- data.frame(x=iData2_subset$Z, dpth=iData2_subset$X35)
  dpth_subset <- na.omit(dpth_subset)
  #Lab_subset <- data.frame(x=iData2_subset2$Z, sand=iData2_subset2$lab_sand, silt=iData2_subset2$lab_silt, clay=iData2_subset2$lab_clay, fine=iData2_subset2$lab_fine)
  #samp_subset <- data.frame(x=iData2_subset$Z, sample=iData2_subset$sample)
  #samp_subset <- na.omit(samp_subset)
       
  # Setup Plot
  #par(mfrow = c(1,3))
  #par(oma = c(6,4,8,4))
  #par(mar = c(0,0,0,0))
  
  # Setup Layout
  #Lmat <- matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE)
  Lmat <- matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE)
  Lwidth <- c(5,5)
  Lheight <- c(7,1,7)
  #layout(Lmat)
  layout(Lmat,Lwidth,Lheight)
  par(oma = c(5,3,7,5))
  
  # Read Mosaic
  Photo.rast <- stack(jpgpath)
  
  # Read and Plot Satellite Imagery
  par(mar = c(0,0,0,2))
  SatelliteImage.rast <- stack(SatelliteImage)
  plotRGB(SatelliteImage.rast, 1, 2, 3, axes = F)

  # Read and Plot Boundary Shapefile
  AOI <- readOGR(dsn = Shpfilefolder, layer = Shpfile)
  lines(AOI, col="yellow", lwd=2, lty=1)

  loc <- data.frame(x=iData2_subset$X[1],y=iData2_subset$Y[1],value=1)
  coordinates(loc) <- ~x + y
  points(loc, pch='*', col="purple", cex=4.5)
  #box(col="black")
  
  # Read and Plot EM Imagery  ###this version does not plot EM
  #par(mar = c(0,0,0,4))
  #EMImage.rast <- stack(EMImage)
  #EMImage.rast2 <- projectRaster(EMImage.rast, crs = projection(SatelliteImage.rast))
  #redyellowgreen <- colorRampPalette(c("darkgreen","yellow","red"))(256)
  #image(EMImage.rast2, col=redyellowgreen, xaxt="n", yaxt="n", bty = "n")
  #mtext("Z Variation from 0 - 1.6m", side=3, line=-0.5, cex=0.7)
  #mtext("(Number of Classes)", side=3, line=-1.5, cex=0.7)
  #points(loc, pch='*', col="black", cex=4.5)
  #plot(1, type="n", axes=F, xlab="", ylab="") #this is a blank plot
  
  #Plot the legend    ###this version does not plot EM
  #pardat <- par()
  #xmin <- 0
  #xmax <- round(maxValue(EMImage.rast2))
  #lableg <- c(formatC(xmin, format="f", digits=1), formatC(1*(xmax-xmin)/5, format="f", digits=1), formatC(2*(xmax-xmin)/5, format="f", digits=1), formatC(3*(xmax-xmin)/5, format="f", digits=1), formatC(4*(xmax-xmin)/5, format="f", digits=1), formatC(xmax, format="f", digits=1))
  #pardat <- par()
  #Xincrement <- 0.05 * (pardat$usr[2]-pardat$usr[1])
  #Yincrement <- 0.1 * (pardat$usr[4]-pardat$usr[3])
  #color.legend(pardat$usr[2], pardat$usr[3]+Yincrement, pardat$usr[2]+Xincrement, pardat$usr[4]-Yincrement, paste(" ", lableg, sep=""), redyellowgreen, align="rb", gradient="y", cex=0.8)
  
  #plot two blank charts first for pretty layout
  #par(fig = c(0, 1, 0, 1), oma = c(0, 0, 2, 0), mar = c(0, 10, 0, 0), new = TRUE)
  par(mar = c(0,0,0,0))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  par(mar = c(0,0,0,0))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
  # Plot Legend
  legend('bottomright', c("Sand (ASD)","Silt (ASD)","Clay (ASD)","Fine (ASD)"), inset = c(0,0.2), horiz = TRUE, lty=c(1,1,1,1,1), lwd=2, bty='n', cex=0.6, col=c("Red","Blue","Purple","Green"))
  #legend('bottomright', c("Sand (avg)","Silt (avg)","Clay (avg)","Fine (avg)"), inset = c(0,0), horiz = TRUE, lty=c(2,2,2,2,2), lwd=2,  bty='n', cex=0.6, col=c("Red","Blue", "Purple","Green"))
  legend('bottomright', c("Fine (w-avg)","Fine>=35"), inset = c(0,0), horiz = TRUE, lty=c(2,2), lwd=2,  bty='n', cex=0.6, col=c("Green","cornflowerblue"))
  #legend('bottomright', c("Sand (Lab)","Silt (Lab)","Clay (Lab)","Fine (Lab)"), inset = c(0,0.2), horiz = TRUE, pch=c(19,19,19,19), bty='n', cex=1.0, col=c("Red","Blue","Purple","Green"))
  #legend('topright', c("Sand (ASD)","Silt (ASD)","Clay (ASD)"), xpd = TRUE, horiz = TRUE, inset = c(0,0.03), lty=c(1,1,1), lwd=2, bty='n', cex=0.9, col=c("Red","Blue","Purple"))
  #legend('topright', c("Sand (Lab)","Silt (Lab)","Clay (Lab)"), xpd = TRUE, horiz = TRUE, inset = c(0,0.04), pch=c(19,19,19), bty='n', cex=0.9, col=c("Red","Blue","Purple"))
  
  # Plot Photo Mosaic
  par(mar = c(0,0,0,0))
  plotRGB(Photo.rast, 1, 2, 3, asp=0.5) # higher number, thinner the figure
  
  # Preplot for Data
  par(mar = c(0,0,0,0))
  plot(cbind(c(0,100),c(0,Zmax)), axes=FALSE,  ylim = rev(range(0:Zmax)), xaxs="i", yaxs="i")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
  box(col = 'grey')
  axis(1, seq(0, 100, 10), col = 'grey', col.axis = 'black', col.ticks = 'black', cex.axis = 1, mgp = c(0.3, 0.3, 0), tck=-0.005) #font = 3, 
  axis(1, seq(0, 100,  5), labels=rep("", 21), tck=-0.005)
  axis(2, col = 'grey', col.axis = 'black', col.ticks = 'black', cex.axis = 1, mgp = c(0.3, 0.3, 0), tck=-0.005) #font = 3, 
  grid(lwd=1, lty=1, col="white")

  # Create a dashed black line box focused on 0-60 cm area
  #abline(v=45, lty=2, lwd=1.5)
  arrows(0.1, 0.1, 99.9, 0.1, length=0, lwd=1.5, lty=2)
  arrows(0.1, 0.1, 0.1, 59.9, length=0, lwd=1.5, lty=2)
  #abline(h=25, lty=2, lwd=1.5)
  arrows(0.1, 59.9, 99.9, 59.9, length=0, lwd=1.5, lty=2)
  arrows(99.9, 0.1, 99.9, 59.9, length=0, lwd=1.5, lty=2)
  
  
  # Plot Data
  x <- ASD_subset$x
  y <- ASD_subset$sand
  xnew <- seq(5, Zmax, 1)
  ynew <- splint(x, y, xnew)
  lines(ynew,xnew, col="Red", lwd=2, lty=1) #, df= 7 or df=length(ASD_subset$sand)-2

  x <- ASD_subset$x
  y <- ASD_subset$silt
  xnew <- seq(5, Zmax, 1)
  ynew <- splint(x, y, xnew)
  lines(ynew,xnew, col="Blue", lwd=1, lty=1)

  x <- ASD_subset$x
  y <- ASD_subset$clay
  xnew <- seq(5, Zmax, 1)
  ynew <- splint(x, y, xnew)
  lines(ynew,xnew, col="Purple", lwd=1, lty=1)
  
  x <- ASD_subset$x
  y <- ASD_subset$fine
  xnew <- seq(5, Zmax, 1)
  ynew <- splint(x, y, xnew)
  lines(ynew,xnew, col="Green", lwd=2, lty=1)
  
  #x <- ASD_subset$x
  #y <- ASD_subset$water
  #xnew <- seq(5, Zmax, 1)
  #ynew <- splint(x, y, xnew)
  #lines(ynew,xnew, col="Cornflowerblue", lwd=1.5, lty=1)
  
  #x <- c(0,avg_subset$x)
  #y <- c(avg_subset$sand,avg_subset$sand)
  #x <- avg_subset$x
  #y <- avg_subset$sand
  #xnew <- seq(5, zmax, 1)
  #ynew <- splint(x, y, xnew)
  #lines(y,x, col="Red", lwd=2, lty=2)
  
  #x <- avg_subset$x
  #y <- avg_subset$silt
  #xnew <- seq(5, zmax, 1)
  #ynew <- splint(x, y, xnew)
  #lines(y,x, col="Coral", lwd=1.5, lty=2)
  
  #x <- avg_subset$x
  #y <- avg_subset$clay
  #xnew <- seq(5, zmax, 1)
  #ynew <- splint(x, y, xnew)
  #lines(y,x, col="Purple", lwd=1.5, lty=2)
  
  #x <- c(0,avg_subset$x)
  #y <- c(avg_subset$fine,avg_subset$fine)
  #x <- avg_subset$x
  #y <- avg_subset$fine
  #xnew <- seq(5, zmax, 1)
  #ynew <- splint(x, y, xnew)
  #lines(y,x, col="Green", lwd=2, lty=2)
  
  x <- c(0,wavg_subset$x)
  y <- c(wavg_subset$fine,wavg_subset$fine)
  #x <- avg_subset$x
  #y <- avg_subset$fine
  #xnew <- seq(5, zmax, 1)
  #ynew <- splint(x, y, xnew)
  lines(y,x, col="Green", lwd=2, lty=2)
  
  #x <- avg_subset$x
  #y <- avg_subset$water
  #xnew <- seq(5, zmax, 1)
  #ynew <- splint(x, y, xnew)
  #lines(y,x, col="Cornflowerblue", lwd=1.5, lty=2)
  
  #lines(ASD_subset$sand,ASD_subset$x, col="Red", lwd=2, lty=1)
  #lines(ASD_subset$silt,ASD_subset$x, col="Blue", lwd=2, lty=1)
  #lines(ASD_subset$clay,ASD_subset$x, col="Purple", lwd=2, lty=1)
  #points(Lab_subset$sand,Lab_subset$x, col="Red", lwd=2, pch=19, cex=1.5)
  #points(Lab_subset$silt,Lab_subset$x, col="Blue", lwd=2, pch=19, cex=1.0)
  #points(Lab_subset$clay,Lab_subset$x, col="Purple", lwd=2, pch=19, cex=1.0)
  #points(Lab_subset$fine,Lab_subset$x, col="Green", lwd=2, pch=19, cex=1.5)
  #points(samp_subset$sample, samp_subset$x, col="cornflowerblue", lwd=2, pch=19, cex=1.5)
  points(dpth_subset$dpth, dpth_subset$x, col="cornflowerblue", lwd=2, pch=19, cex=1.5)
  
  # Title
  mtext("Depth (cm)", side=2, line=2, cex=0.8)
  mtext("Particle Percentage (%)", side=1, line=2, cex=0.8)
  Location <- paste0(paste0(paste0("X: ", iData2_subset$X[1]), "   Y: "), iData2_subset$Y[1])
  mtext(Location, side=3, line=1.5, cex=0.6, outer=T)
  #mtext(iData2_subset$FEID[1], side=3, line=3, cex=1.3, outer=T)
  mtext(substr(iData2_subset$FEID[1], 1, nchar(as.character(iData2_subset$FEID[1]))-4) , side=3, line=3, cex=1.3, outer=T)
}  

dev.off()




