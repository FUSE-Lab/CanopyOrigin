#This code preps PLS data for later use
#Author: Lindsay Darling
#Started on: 11/25/2020

#Load libraries

library(tidyverse)
library(sf)
library(magrittr)
library(raster)
library(spatialEco)

#Load PLS vegetation data

PLSVeg <- st_read('AllVeg.shp')

PLSVegCRS <- st_transform(PLSVeg,("+init=epsg:32616")) #change to UTM 16N

PLSVegValid <- st_make_valid(PLSVegCRS)

#Clip to project area

ChiReg <- st_read('ChiReg.shp')

ChiRegCRS <- st_transform(ChiReg,("+init=epsg:32616")) #change to UTM 16N

ChiRegValid <- st_make_valid(ChiRegCRS)

RegVeg <- st_crop(PLSVegValid, ChiRegValid)

st_write(RegVeg, 'ChiRegVeg2.shp', driver = 'ESRI shapefile')

#Load tree data

PLSTree <- st_read('AllTree.shp')

#Add BA

PLSTree %<>%
  mutate(DBH_cm = `DIA` * 2.54,
         BA = (`DBH_cm`^2)*0.00007854)
