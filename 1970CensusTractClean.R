#This code will clean 1970 census tract data and join to shapefile
#Author: Lindsay Darling
#Started on: 12/8/2020

#Load libraries


library(tidyverse)
library(magrittr)
library(dplyr)

  #Set WD
setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin")

#Load tract race, education, and housing value data. These data were downloaded from the IPUMS website

census1 <- read.csv('1970/1970Tract.csv') 

census2 <- read.csv('1970/1970Tract2.csv') 

census3 <- read.csv('1970/1970Tract3.csv') 

census4 <- read.csv('1970/1970Tract4.csv') 

tc <- c('Cook', 'Dupage', 'Kane', 'Lake', 'McHenry', 'Will') 

censusAll <- left_join(census1, census2, by = "GISJOIN") %>% #If we want to add more variables they can be joined here
              left_join(., census3, by ='GISJOIN') %>%
              left_join(., census4, by ='GISJOIN')%>%
              filter(STATE.x == 'Illinois',
                     COUNTY.x %in% tc)

#Changing header names and calculating white, not white, etc.


 censusAll %<>%
  
  mutate(White70T = (`CEB001` + `CEB010`), #TODO I'm keeping this white/not white to match the 1940 data. Is this right?
         NotWhite70T = (`CEB002`+`CEB003` + `CEB004`+`CEB005`+`CEB006`+`CEB007`+`CEB008`+`CEB009`+
                          `CEB011`+`CEB012`+`CEB013`+`CEB014`+`CEB015`+`CEB016`+`CEB017`+`CEB018`),
         NoSchool70T = (`C06001`), 
         NoHighSchool70T = (`C06002`+ `C06003`+`C06004`+ `C06005`),
         HighSchool70T = (`C06006`+ `C06007`),
         College70T = (`C06010` + `C06008`+ `C06009`))
         
         
#Trying to figure out how to unbin data to find median value. Code adapted from https://github.com/deanhardy/berkeley-county/blob/master/scripts/analysis-demographic-berkeley.R#L209
#Dean needs to be given a heads up if we use this code and acknowledged

table <- tibble(variable=c('CG7001','CG7002','CG7003','CG7004','CG7005','CG7006','CG7007','CG7008','CG7009','CG7010','CG7011'),
                bin_min=c(0,5000,7500,10000,12500,15000,17500,20000,25000,35000,50000),
                bin_max=c(4999,7499,9999,12499,14999,17499,19999,24999,34999,49999,NA))%>%
  mutate(interval = paste(bin_min, bin_max, sep = "-"))
         
gatherMedHouse <- censusAll %>%
  select('GISJOIN','CG7001':'CG7011')%>%
  gather(key = 'variable',
         value = 'HouseVal',
         -GISJOIN)%>%
  mutate(HouseVal = replace_na(HouseVal,0)) %>% 
  filter(HouseVal > 0)


gm <- left_join(gatherMedHouse, table, by = 'variable')

## define function following stackoverflow post
# https://stackoverflow.com/questions/18887382/how-to-calculate-the-median-on-grouped-dataset
## but revised per variables from 
# https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html
## B modified to account for when median group is the 0-9999 bin
GMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of the group containing the median 
  w <- diff(intervals[, Midrow]) # width of median class
  G <- frequencies[Midrow]       # frequency of median class
  B <- ifelse(Midrow > 1, cf[Midrow - 1], as.vector(0))  # cumulative frequency of the groups before median group
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - B)/G * w)
}


gm %<>% group_by(GISJOIN) %>% 
  summarise(MedHouseVal = GMedian(frequencies = HouseVal, intervals = interval, sep = "-", trim = 'cut'))

censusHsValue <- left_join(censusAll, gm, by = 'GISJOIN') 

#Calculate median house age

table <- tibble(variable=c('CZ2001','CZ2002','CZ2003','CZ2004','CZ2005','CZ2006'),
                bin_min=c(NA,1940,1950,1260,1965,1969),
                bin_max=c(1939,1949,1959,1964,1968,1970))%>%
  mutate(interval = paste(bin_min, bin_max, sep = "-"))

gatherMedAge <- censusAll %>%
  select('GISJOIN','CZ2001':'CZ2006')%>%
  gather(key = 'variable',
         value = 'MedHsAge',
         -GISJOIN)%>%
  mutate(MedHsAge = replace_na(MedHsAge,0)) %>% 
  filter(MedHsAge > 0)


gm <- left_join(gatherMedAge, table, by = 'variable')

gm %<>% group_by(GISJOIN) %>% 
  summarise(MedHsAge = GMedian(frequencies = MedHsAge, intervals = interval, sep = "-", trim = 'cut'))


censusHsAge <- left_join(censusHsValue, gm, by = 'GISJOIN') #TODO: Ths needs to be rounded and change values that are below 1939 to 1939.

#Write it up!

write.csv(censusAll, 'DataStandardized/1970tract.csv')
