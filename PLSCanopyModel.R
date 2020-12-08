#This code will analyze patterns in PLS canopy to create a model of canopy cover
#Author: Lindsay Darling
#Started on: 12/1/2020
#CRS = WGS 84 utm 16N, espg 32616
#TODO: Everything

#Load libraries

library(tidyverse)
library(sf)
library(magrittr)
library(raster)
library(spatialEco)
library(ggplot2)

setwd('D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin')

#Load PLS vegetation data

PLSVeg <- st_read('ChiRegVeg2.shp')

PLSVegV <- st_make_valid(PLSVeg)


#Load tree data

PLSTree <- st_read('AllTree.shp') #Change to 

PLSTreeUTM <- st_transform(PLSTree,("+init=epsg:32616"))

#Add BA

PLSTreeUTM %<>%
  mutate(DBH_cm = `DIA` * 2.54,
         BA = (`DBH_cm`^2)*0.00007854)

#Add plot attributes to each tree

TreeVeg <- st_intersection(PLSTreeUTM, PLSVegV) #Be patient
TreeVegDF <- as_tibble(TreeVeg) #Want to treat this is a table for a while

#Summarize tree data by plot

TreePatch <- TreeVegDF %>%
  group_by(OBJECTID, DESCRIPTIO, AreaHa) %>%
  summarize(sumBA = sum(BA))

TreePatch %<>%
  mutate(BAPerHa = `sumBA`/`AreaHa`)

p <- ggplot(TreePatch, aes(x = DESCRIPTIO, y = BAPerHa)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)

plot(p + scale_y_log10() + theme(axis.text.x = element_text(angle =90)))

####Integrate soils data

soil <- st_read('ILSoils.shp')

#Dissolve PLS veg

VegDissolve <- PLSVegV %>%
  group_by(DESCRIPTIO) %>%
  summarize()

#Intersect soils and dissolved PLS
