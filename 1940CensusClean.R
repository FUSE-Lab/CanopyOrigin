#This code will clean 1940 census tract data and join to shapefile
#Author: Lindsay Darling
#Started on: 12/8/2020

#Load libraries

library(tidyverse)
library(sf)
library(magrittr)
library(dplyr)

#Set WD
setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin")

#Load tract race, education, and housing value data.

census <- read.csv('1940/1940Tract.csv') %>%
  
  rename(White40T = `BUQ001`, #rename variables to something meaningful
         NotWhite40T = `BUQ002`,
         MedianHomeValue40T =`BVC001`) %>% 
  
  mutate( NoSchool40T = (`BUH001`+ `BUH010`), #Combine schooling categories. They were previously separated by gender and lots of buckets.
          NoHighSchool40T = (`BUH002`+ `BUH003`+`BUH004`+ `BUH011` + `BUH012`+ `BUH013`),
          HighSchool40T = (`BUH005` + `BUH006`+ `BUH014`+ `BUH015`),
          College40T = (`BUH007` + `BUH008`+ `BUH016` + `BUH017`)) %>%
  
  dplyr::select(White40T, #Drop the unneeded columns
                NotWhite40T,
                MedianHomeValue40T,
                NoSchool40T,
                NoHighSchool40T,
                HighSchool40T,
                College40T)
head(census)
