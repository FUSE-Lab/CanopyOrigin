#This code will clean 1940 census county data and join to shapefile
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

census <- read.csv('1940/1940County.csv') %>%
  
  rename(White40C = `AF15001`, #rename variables to something meaningful
         NotWhite40C = `AF15002`,
         MedianHomeValue40C =`BYO001`) %>% 
  
  mutate( NoSchool40C = (`BWW001`+ `BWW010`), #Combine schooling categories. They were previously separated by gender and lots of buckets.
          NoHighSchool40C = (`BWW002`+ `BWW003`+`BWW004`+ `BWW011` + `BWW012`+ `BWW013`),
          HighSchool40C = (`BWW005` + `BWW006`+ `BWW014`+ `BWW015`),
          College40C = (`BWW007` + `BWW008`+ `BWW016` + `BWW017`)) %>%
  
  dplyr::select(White40C, #Drop the unneeded columns
                NotWhite40C,
                MedianHomeValue40C,
                NoSchool40C,
                NoHighSchool40C,
                HighSchool40C,
                College40C)
head(census)
