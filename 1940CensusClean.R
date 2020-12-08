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
  
  rename(White1940 = `BUQ001`, #rename variables to something meaningful
         NotWhite1940 = `BUQ002`,
         MedianHomeValue1940 =`BVC001`) %>% 
  
  mutate( NoSchool1940 = (`BUH001`+ `BUH010`), #Combine schooling categories. They were previously separated by gender and lots of buckets.
          NoHighSchool1940 = (`BUH002`+ `BUH003`+`BUH004`+ `BUH011` + `BUH012`+ `BUH013`),
          HighSchool1940 = (`BUH005` + `BUH006`+ `BUH014`+ `BUH015`),
          College1940 = (`BUH007` + `BUH008`+ `BUH016` + `BUH017`)) %>%
  
  dplyr::select(White1940, #Drop the unneeded columns
                NotWhite1940,
                MedianHomeValue1940,
                NoSchool1940,
                NoHighSchool1940,
                HighSchool1940,
                College1940)
head(census)
