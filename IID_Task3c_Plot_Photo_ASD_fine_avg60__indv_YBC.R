
# the script will plot core photo and ASD results on the right
# sand, silt, clay, fine and 60cm average of sand and fine
# the script saves one jpeg / pdf per core
# example input: IID_Task3c_Plot_Photo_ASD_fine_avg60_YBC.csv
# example output: IID_Task3c_Plot_Photo_ASD_fine_avg60_indv_YBC_####.pdf
# example output: IID_Task3c_Plot_Photo_ASD_fine_avg60_indv_YBC_####.jpg                

##################################################
# This script is to create 2D soil variation map at XY Plane for Depth Range
# Use Original Collection Points as inputs
#
##################################################

rm(list=ls())

Photo_Folder = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/PhotoDocumentation/Final"

#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Final/CSV/CH_ALL.csv"
#csv_file = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/4PLS/AS_S1_C1_ver2/AS_C3_003_results.csv"
csv_file = "C:/Users/ybcheng/Documents/R/SaltonSea/examples_plotting/IID_Task3c_Plot_Photo_ASD_fine_avg60_YBC.csv"

#SavePDF = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/ASD/Processing/Charts2/NewRiver/Photo_And_ASD_"
SavePDF = "C:/Users/ybcheng/Documents/R/SaltonSea/examples_plotting/IID_Task3c_Plot_Photo_ASD_fine_avg60_indv_YBC_"
#SavePDF = "C:/Users/ybcheng/Documents/R/SaltonSea/examples_plotting/IID_Task3c_Plot_Photo_ASD_fine_avg60_YBC.pdf"

#jpgORpdf = 11 #save plots as jpg
jpgORpdf = 22 #save plots as pdf

  
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
library(plotrix)

# Importing Data
iData <- read.csv(csv_file,header=T)
temp <- data.frame(do.call('rbind', strsplit(as.character(iData$FEID),'_',fixed=TRUE)))
Zmax <- max(iData$Z)

FEIDjpg <- paste(paste(paste(paste(paste(temp[,1],temp[,2], sep="_"),temp[,3], sep="_"),temp[,4], sep="_"),temp[,5], sep="_"),".jpg",sep="")
iData2 <- data.frame(iData,jpgName=FEIDjpg)

FEIDjpg_Unique <- unique(FEIDjpg)
JPGList <- Sys.glob("*.jpg")

#pdf(file=SavePDF,width=4,height=5) #save all charts into one pdf file

for (jpgfile in FEIDjpg_Unique) {

  #jpgpath = "K:/IID_SaltonSea/Tasks/Task3c/Tasks/PhotoDocumentation/Final/IID20160913_AS_S1_001_SL.jpg"
  jpgpath <- paste(Photo_Folder,jpgfile,sep="/")
  jpgfilename <- basename(jpgpath)
  
  #save each chart into individual pdf or jpeg
  if (jpgORpdf == 11){
    jpeg(file=paste0(SavePDF,gsub(".jpg",".jpg",jpgfilename)),width=1180,height=1580, units="px", res=400)
    #jpeg(file=paste0(SavePDF,gsub(".jpg",".jpg",jpgfilename)),width=2360,height=1580, units="px", res=400)
  }else if (jpgORpdf == 22){
    pdf(file=paste0(SavePDF,gsub(".jpg",".pdf",jpgfilename)),width=4,height=5)
  }else {
    stop("ERROR!!! check save file type!!!")
  }
  
  # Subset Data
  iData2_subset <- iData2[which(iData2$jpgName == jpgfilename),]
  ASD_subset <- data.frame(x=iData2_subset$Z, sand=iData2_subset$sand, silt=iData2_subset$silt, clay=iData2_subset$clay, fine=iData2_subset$fine)
  #ASD_subset <- data.frame(x=iData2_subset$Z, sand=iData2_subset$sand, fine=iData2_subset$fine, water=iData2_subset$water)
  
  iData2_subset2 <- na.omit(iData2_subset)
  avg_subset <- data.frame(x=iData2_subset2$Z, sand=iData2_subset2$avg_sand, fine=iData2_subset2$avg_fine)  #water=iData2_subset2$avg_water)
  
  #Lab_subset <- data.frame(x=iData2_subset2$Z, sand=iData2_subset2$lab_sand, silt=iData2_subset2$lab_silt, clay=iData2_subset2$lab_clay, fine=iData2_subset2$lab_fine)
  
  # Setup Layout
  Lmat <- matrix(c(1,2,3,4), 2, 2, byrow = TRUE)
  Lwidth <- c(3,6)
  Lheight <- c(0.5,7)
  #layout(Lmat)
  layout(Lmat,Lwidth,Lheight)
  par(oma = c(3,1,1,1))
  
  # Read Mosaic
  Photo.rast <- stack(jpgpath)

  #par(fig = c(0, 1, 0, 1), oma = c(0, 0, 2, 0), mar = c(0, 10, 0, 0), new = TRUE)
  #plot two blank charts first for pretty layout 
  par(mar = c(0,0,0,0))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  par(mar = c(0,0,0,0))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
  # Plot Legend
  legend('bottomright', c("Sand (ASD)","Silt (ASD)","Clay (ASD)", "Fine (ASD)"), inset = c(0,0.2), horiz = TRUE, lty=c(1,1,1,1), lwd=2, bty='n', cex=0.4, col=c("Red","Blue","Purple","Green"))
  legend('bottomright', c("Sand (AVERAGE)","Fine (AVERAGE)"), inset = c(0,0), horiz = TRUE, lty=c(3,3), lwd=2,  bty='n', cex=0.4, col=c("Red","Green"))
  
  # Plot Photo Mosaic
  par(mar = c(0,0,0,0))
  plotRGB(Photo.rast, 1, 2, 3, asp=0.6) # higher number, thinner the figure
  
  # Preplot for Data
  par(mar = c(0,1,0,0))
  plot(cbind(c(0,100),c(0,Zmax)), axes=FALSE,  ylim = rev(range(0:Zmax)), xaxs="i", yaxs="i")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
  box(col = 'grey')
  axis(1, seq(0, 100, 10), col = 'grey', col.axis = 'black', col.ticks = 'black', cex.axis = 0.6, mgp = c(0.3, 0.3, 0), tck=-0.005) #font = 3, 
  axis(1, seq(0, 100,  5), labels=rep("", 21), tck=-0.005)
  axis(2, col = 'grey', col.axis = 'black', col.ticks = 'black', cex.axis = 0.6, mgp = c(0.3, 0.3, 0), tck=-0.005) #font = 3, 
  grid(lwd=1, lty=1, col="white")
  
    # Create a dashed black line box focused on 0-60 cm area
  #abline(v=45, lty=2, lwd=1.5)
  arrows(0.1, 0.1, 99.9, 0.1, length=0, lwd=1.5, lty=3)
  arrows(0.1, 0.1, 0.1, 59.9, length=0, lwd=1.5, lty=3)
  #abline(h=25, lty=2, lwd=1.5)
  arrows(0.1, 59.9, 99.9, 59.9, length=0, lwd=1.5, lty=3)
  arrows(99.9, 0.1, 99.9, 59.9, length=0, lwd=1.5, lty=3)
  
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
  #lines(ynew,xnew, col="Blue", lwd=2, lty=1)
  
  x <- c(0,avg_subset$x)
  y <- c(avg_subset$sand,avg_subset$sand)
  #x <- avg_subset$x
  #y <- avg_subset$sand
  lines(y,x, col="Red", lwd=2, lty=2) #lty=3
  
  x <- c(0,avg_subset$x)
  y <- c(avg_subset$fine,avg_subset$fine)
  #x <- avg_subset$x
  #y <- avg_subset$fine
  lines(y,x, col="Green", lwd=2, lty=2)

  #x <- c(0,avg_subset$x)
  #y <- c(avg_subset$water,avg_subset$water)
  #x <- avg_subset$x
  #y <- avg_subset$water
  #lines(y,x, col="Blue", lwd=2, lty=2)
  
  # Title
  mtext("Depth (cm)", side=2, line=1, cex=0.6)
  mtext("Particle Percentage (%)", side=1, line=1, cex=0.6)
  Location <- paste0(paste0(paste0("X: ", iData2_subset$X[1]), "   Y: "), iData2_subset$Y[1])

}  


dev.off()

