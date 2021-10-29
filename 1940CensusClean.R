#This code will clean 1940 census tract data
#Author: Lindsay Darling
#Started on: 12/8/2020

#Load libraries

library(tidyverse)
library(tidylog)

#Set WD
setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin")

#Load tract race, education, and housing value data.

census1 <- read.csv('1940/1940Tract.csv') 

census2 <- read.csv('1940/1940TractRent.csv')

tc <- c('Cook', 'Dupage', 'Kane', 'Kendall', 'Lake', 'McHenry', 'Will') #These data only have Chicago, but filter will still work

censusAll <- left_join(census1, census2, by = "GISJOIN") %>%
  filter(STATE.x == 'Illinois',
         COUNTY.x %in% tc) %>% 
  
  rename(White40T = `BUQ001`, #rename variables to something meaningful
         NotWhite40T = `BUQ002`,
         MedHomeVal40T =`BVC001`,
         MedRent40T = `BVF001`) %>% 
  mutate( NoSchool40T = (`BUH001`+ `BUH010`), #Combine schooling categories. They were previously separated by gender and lots of buckets.
          NoHighSchool40T = (`BUH002`+ `BUH003`+`BUH004`+ `BUH011` + `BUH012`+ `BUH013`),
          HighSchool40T = (`BUH005` + `BUH006`+ `BUH014`+ `BUH015`),
          College40T = (`BUH007` + `BUH008`+ `BUH016` + `BUH017`)) %>%
  dplyr::select(GISJOIN,
                White40T, #Drop the unneeded columns
                NotWhite40T,
                MedHomeVal40T,
                MedRent40T,
                NoSchool40T,
                NoHighSchool40T,
                HighSchool40T,
                College40T) %>% 
  mutate(TotPop40T = `White40T` + `NotWhite40T`,
         PWhite40T = `White40T`/`TotPop40T`,
         PNotWhite40T = `NotWhite40T`/`TotPop40T`,
         PNoSchool40T = `NoSchool40T`/`TotPop40T`,
         PNoHighSchool40T = `NoHighSchool40T`/`TotPop40T`,
         PHighSchool40T = `HighSchool40T`/`TotPop40T`,
         PCollege40T = `College40T`/`TotPop40T`)
head(censusAll)

write.csv(censusAll, 'DataStandardized/Census1940Tract.csv')
