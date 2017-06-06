# Read_and_Plot_AN_Veg_Data.R
# Jacob Kollen 
# K:/IID_SaltonSea/Tasks/VegetationPilotStudies/PoeRoad/Documentation/Veg_Continuity_Methods_Dev/BaseData
# 4/06/2017
# Script to run a two way anova on VegLaterals Remotely Sensed Continuity

######################################################################################################
# Clear all close all clc
cat("\014")  
rm(list = ls())

#######################################################################################################
# Load libraries
#######################################################################################################

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

##################################################################################################
# Set Libraries, Folders and Key files
##################################################################################################
# pkgTest("plotly")
# pkgTest("plot3D")
pkgTest("ggplot2")
pkgTest("readxl")
pkgTest("dplyr")
pkgTest("plyr")
pkgTest("reshape2")
pkgTest("grid")
pkgTest("gridExtra")
# 
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library(plot3D)
library(ggplot2)
library(readxl)
library(grid)
library(gridExtra)
#######################################################################################################
# Set base_path
base_path = "K:/IID_SaltonSea/Tasks/Task3b/Tasks/HedgeRowConituity_Multspec/Base Data"

# Set workspace
setwd(base_path)
getwd()

#Set Base File
file_name <- "MS_Cont_Table_Export_20170427.xls"

#######################################################################################################
# Import data and format it 
MyData <- as.data.frame(read_excel(file_name, sheet=1,col_names = TRUE,col_types = NULL, na="",skip=0))


#######################################################################################################
# Boxplot Hedge Row Continuity vs Treatment with No Factor at All Sites

plot_data <- ggplot(data=MyData, aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Color_Code)))+
  # scale_fill_brewer(palette="YlGn")+
  # geom_jitter(width = 0.2)+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("All Sites\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplot Hedge Row Continuity vs Treatment with No Factor at Poe Road

plot_data <- ggplot(data=subset(MyData, Site == 'Poe Road'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Color_Code)))+
  # geom_jitter(width = 0.2)+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Poe Road\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplot Hedge Row Continuity vs Treatment with No Factor at Bombay Beach

plot_data <- ggplot(data=subset(MyData, Site == 'Bombay Beach'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Color_Code)))+
  # geom_jitter(width = 0.2)+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Bombay Beach\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplot Hedge Row Continuity vs Treatment with No Factor at Coachella Playa

plot_data <- ggplot(data=subset(MyData, Site == 'Coachella Playa'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Color_Code)))+
  # geom_jitter(width = 0.2)+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Coachella Playa\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)




#######################################################################################################
#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of depth to ground water at all sites

plot_data <- ggplot(data=MyData, aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Lateral)))+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("All Sites\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of depth to ground water at Poe Road

plot_data <- ggplot(data=subset(MyData, Site == 'Poe Road'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Lateral)))+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Poe Road\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of depth to ground water at Bombay Beach

plot_data <- ggplot(data=subset(MyData, Site == 'Bombay Beach'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Lateral)))+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Bombay Beach\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of depth to ground water at Coachella Playa

plot_data <- ggplot(data=subset(MyData, Site == 'Coachella Playa'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Lateral)))+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Coachella Playa\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)


#######################################################################################################
#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of site at all of the laterals

plot_data <- ggplot(data=MyData, aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Site)))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("All Laterals\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of site at the shallow lateral

plot_data <- ggplot(data=subset(MyData, Lateral == 'Shallow'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Site)))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Shallow Lateral\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of site at the medium lateral

plot_data <- ggplot(data=subset(MyData, Lateral == 'Medium'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Site)))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Medium Lateral\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)

#######################################################################################################
# Boxplots of hedge row continuity vs treatment with a factor of site at the deep lateral

plot_data <- ggplot(data=subset(MyData, Lateral == 'Deep'), aes(x=reorder(Color_Code,cont_0pt38), y=cont_0pt38))+
  geom_boxplot(aes(fill=factor(Site)))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Treatment (class)", y="Hedge Row Continuity (area/area)")+
  ggtitle("Deep Lateral\nHedge Row Continuity vs Veg Enhance Treatement\nObserved 03/17/17: 125 Days After Sowing\nNDVI Threshold = 0.38")
plot(plot_data)


pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"))
    # ,
    # width, height, onefile, family, title, fonts, version,
    # paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
    # useDingbats, useKerning)

