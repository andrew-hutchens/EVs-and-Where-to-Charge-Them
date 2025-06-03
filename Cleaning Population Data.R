### PURPOSE: Clean ACS population data
### LAST EDITED: 1/6/2025
### EDITED FROM: Laptop

library(RecordLinkage)
library(foreach)
library(geosphere)
library(osmdata)
library(tm)
library(tidyr)
library(dplyr)
library(ggplot2)
library(usmap)
library(viridis)
library(maps)
library(geoR)
library(mapproj)
library(geosphere)
library(osmdata)
library(httr)
library(jsonlite)
library(choroplethr)
library(readxl)

## NOTE: Population data is obtained at the tract level for use as a station-level control (and station-level
## descriptive statistics) but also at the COUNTY level to construct EVs per capita variable (since EV data
## is at the county level).

################################################################################
### Tract-level population
################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/ACS/Population/Tract")
years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
tractpopdata <- data.frame()
for (i in 1:length(years)) {
  # Reading in the raw data
  year <- years[i]
  print(paste0("Working on ", year))
  filepath <- paste0("ACSDT5Y",year,".B01003-Data.csv")
  data <- read.csv(filepath, header=TRUE, na.strings=c("**","-","(X)"))
  # Removing unnecessary row of longer column names)
  data <- data[!rownames(data)==1,]
  rownames(data) <- 1:nrow(data)
  # Keeping only geoid and population count variable
  data <- data %>%
    select("geoid"="GEO_ID", "pop"="B01003_001E")
  # Creating tract geoid variable 'tractgeoidXXXX' to match analysis data's tract geoid variable,
  # which depends on what year of ACS data is being processed
  if (year<2020) {
    data$tractgeoid2010 <- as.numeric(substr(data$geoid, 10, 20))
    data$tractgeoid2020 <- NA
  } else if (year>=2020) {
    data$tractgeoid2010 <- NA
    data$tractgeoid2020 <- as.numeric(substr(data$geoid, 10, 20))
  }
  # Converting population variable 'pop' to numeric
  data$pop <- as.numeric(data$pop)
  # Adding a year variable, dropping the geoid column, and collecting the data
  data$year <- year
  data <- data %>%
    select(year, tractgeoid2010, tractgeoid2020, pop)
  data <- as.data.frame(data)
  rownames(data) <- 1:nrow(data)
  tractpopdata <- rbind(tractpopdata, data)
}

## Saving
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(tractpopdata, file="tractpopdata.csv", row.names=FALSE)

################################################################################
## County-level population
################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/ACS/Population/County")
years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
countypopdata <- data.frame()
for (i in 1:length(years)) {
  # Reading in the raw data
  year <- years[i]
  print(paste0("Working on ", year))
  filepath <- paste0("ACSDT5Y",year,".B01003-Data.csv")
  data <- read.csv(filepath, header=TRUE, na.strings=c("**","-","(X)"))
  # Removing unnecessary row of longer column names)
  data <- data[!rownames(data)==1,]
  rownames(data) <- 1:nrow(data)
  # Keeping only geoid and population count variable
  data <- data %>%
    select("geoid"="GEO_ID", "pop"="B01003_001E")
  # Creating a countyfips variable from the raw geoid variable
  data$countyfips <- as.numeric(substr(data$geoid, 10, 14))
  # Converting population variable 'pop' to numeric
  data$pop <- as.numeric(data$pop)
  # Adding a year variable, dropping the geoid column, and collecting the data
  data$year <- year
  data <- data %>%
    select(countyfips, year, pop)
  data <- as.data.frame(data)
  rownames(data) <- 1:nrow(data)
  countypopdata <- rbind(countypopdata, data)
}

## Saving
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(countypopdata, file="countypopdata.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())