#This code will clean 1940 census county and export to csv
#Author: Lindsay Darling
#Started on: 12/15/2020

#Load libraries

library(tidyverse)
library(sf)
library(magrittr)
library(dplyr)

#Set WD
setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin")

#Load tract race, education, and housing value data.

census1 <- read.csv('1940/1940County.csv') 

census2 <- read.csv('1940/1940CountyRent.csv')

census3 <- read.csv('1940/1940WhiteNotWhiteCounty.csv')

tc <- c('Cook', 'Dupage', 'Kane', 'Kendall', 'Lake', 'McHenry', 'Will') 

censusAll <- left_join(census1, census2, by = "GISJOIN") %>%
  filter(STATE.x == 'Illinois',
         COUNTY.x %in% tc) %>% 
  
  rename(White40C = `AF15001`, #rename variables to something meaningful
         NotWhite40C = `AF15002`,
         MedHomeVal40C =`BYO001`,
         Rent40C = `BYR001`) %>% 
  
  mutate( NoSchool40C = (`BWW001`+ `BWW010`), #Combine schooling categories. They were previously separated by gender and lots of buckets.
          NoHighSchool40C = (`BWW002`+ `BWW003`+`BWW004`+ `BWW011` + `BWW012`+ `BWW013`),
          HighSchool40C = (`BWW005` + `BWW006`+ `BWW014`+ `BWW015`),
          College40C = (`BWW007` + `BWW008`+ `BWW016` + `BWW017`),
          TotalPop40C = White40C + NotWhite40C) %>%
  
  dplyr::select(GISJOIN,
                TotalPop40C,
                White40C, #Drop the unneeded columns
                NotWhite40C,
                MedHomeVal40C,
                Rent40C,
                NoSchool40C,
                NoHighSchool40C,
                HighSchool40C,
                College40C)

#Write it up!

write.csv(censusAll, 'DataStandardized/1940County.csv')
