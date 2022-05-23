#Script to download and clean census data
#Lindsay Darling
#3/19/2021

#libraries--------------------

library(tidyverse)
library(tidylog)
library(magrittr)
library(ggplot2)
library(sf)
library(tidycensus)
library(raster)

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin")

#Load census data

# Put your key in the quotes and run so it doesn't have to be specified with each api call 
#TODO: scrub this key!
census_api_key(key = "5f032d28baabc79c1f27b32d210860e80880b5cd", install=T, overwrite = TRUE)

# Set up variable, scale and year choices

geom <- 'tract'
YR <- 2015
ST <- c('IL')
CNTY <- c('Cook', 'Dupage', 'Kane', 'Kendall', 'Lake', 'McHenry', 'Will County')
vars <- c('B19013_001', 'B25001_001', 'B25035_001', 'B25003A_002', 'B25003A_003', 'B15003_022', 'B02001_002', 'B02001_003', 
          'B02001_005', 'B02001_007', 'B23025_005', 'B23024_002', 'B01003_001', 'B25064_001', 'B01002_001')

#Use tidycensus to get data
census<- get_acs(geography = geom,
               variables = vars,
               state = ST,
               county = CNTY,
               year = YR,
               output = 'wide',
               geometry = TRUE) %>% #That pulls in tigerlines
  sf::st_transform(., crs = 5070) #Change to albers equal conic

census$AreaHa <-st_area(census) %>% #Add area and convert to ha
  units::set_units(value = ha)

#Rename, select, and modify variables

census %<>%
  rename(FIPS = `GEOID`,
         Wht_pop = `B02001_002E`,
         Blk_pop = `B02001_003E`,
         Asn_pop = `B02001_005E`,
         Other_pop = `B02001_007E`,
         College_ed = `B15003_022E`,
         Med_ncm = `B19013_001E`,
         Below_poverty = `B23024_002E`,
         Unemployed_pop = `B23025_005E`,
         Housing_unit = `B25001_001E`,
         Owner_house = `B25003A_002E`,
         Renter_house = `B25003A_003E`,
         House_age = `B25035_001E`,
         Med_rent = `B25064_001E`,
         Tot_pop = `B01002_001E`) %>% 
  dplyr::select(FIPS,
         Wht_pop,
         Blk_pop,
         Asn_pop,
         Other_pop,
         College_ed,
         Med_ncm,
         Below_poverty,
         Unemployed_pop,
         Housing_unit,
         Owner_house,
         Renter_house,
         House_age,
         Med_rent,
         Tot_pop,
         AreaHa) %>% 
  mutate(Med_ncm = ifelse(is.na(Med_ncm), 0, Med_ncm)) %>% #Change NA income to zeros
  mutate(WhtPopP = (Wht_pop/(Tot_pop +0.000001)), #Change some variables to percent
         BlkPopP = (Blk_pop/(Tot_pop +0.000001)),
         AsnPopP = (Asn_pop/(Tot_pop +0.000001)),
         OthPopP = (Other_pop/(Tot_pop+0.000001)),
         UnivP = (College_ed/(Tot_pop+0.000001)),
         PovP = (Below_poverty/(Tot_pop+0.000001)),
         UnplydP = (Unemployed_pop/(Tot_pop+0.000001)),
         HouseD = (Housing_unit/AreaHa),
         OwnerP = (Owner_house/(Housing_unit+0.000001)),
         RenterP = (Renter_house/(Housing_unit+0.000001)),
         TotPopD = (Tot_pop/AreaHa),
         HouseD = (Housing_unit/AreaHa)) 

#Write it up
st_write(census, file.path('2015Clean1.shp'), driver = 'ESRI Shapefile', append = TRUE)


#Get project area----------------------------------------

censusCounty<- get_acs(geography = 'county',
                     variables = 'B01003_001',
                     state = ST,
                     #county = CNTY,
                     year = YR,
                     output = 'wide',
                     geometry = TRUE)%>% #That pulls in tigerlines
  sf::st_transform(., crs = 5070) %>%  #Change to albers equal conic 
  filter(NAME %in% c('Cook County, Illinois', 'DuPage County, Illinois', 'Kane County, Illinois', 
                      'Kendall County, Illinois', 'Lake County, Illinois', 'McHenry County, Illinois', 'Will County, Illinois')) %>% 
  dplyr::select(GEOID)

st_write(censusCounty, '2015/SevenCountyRegion.shp', driver = 'ESRI Shapefile', overwrite = TRUE) #TODO: this is throwing an error

#Get municipal boundaries--------------------------------------

censusMuni<- get_acs(geography = 'place',
                 variables = 'B01003_001',
                 state = ST,
                 #county = CNTY,
                 year = YR,
                 output = 'wide',
                 geometry = TRUE)%>% #That pulls in tigerlines
  sf::st_transform(., crs = 5070) %>% #Change to albers equal conic
  dplyr::select(GEOID)

st_write(censusMuni, '2015/MuniBoundaries.shp', driver = 'ESRI Shapefile', overwrite = TRUE)



