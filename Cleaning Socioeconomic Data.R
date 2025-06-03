### PURPOSE: Clean socioeconomic data (annual mean household income from 2010-2023)
### LAST EDITED: 12/12/2024
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

# NOTE: Only using 5-Year ACS estimates of median/mean income for now because the 1-Year ACS estimate for 2020 
# is not available (due to COVID-19's impacts on ACS' surveying).

### Mean income: 5-Year ACS
## Reading in raw income data, doing basic cleaning, and combining all separate year files into 1 file
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data")
mean_inc <- data.frame()
years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
for (i in 1:length(years)) {
  # Reading in the raw data (data column changes between pre-2017 and post-2017)
  year <- years[i]
  print(paste0("Working on ", year))
  if (year<2017) { # Pre-2017
    filename <- paste0("ACS/Mean Income/5 Year Estimates/ACSST5Y", year, ".S1902-Data.csv")
    data <- read.csv(filename, header=TRUE, na.strings=c("**","-","(X)"))
    data <- data[2:dim(data)[1],] # Dropping unnecessary second row that just describes the variables
    data <- data %>%
      select("geoid"="GEO_ID", "mean_hhinc"="S1902_C02_001E")
    data$mean_hhinc <- as.numeric(data$mean_hhinc)
  } else if (year>=2017) { # Post-2017
    filename <- paste0("ACS/Mean Income/5 Year Estimates/ACSST5Y", year, ".S1902-Data.csv")
    data <- read.csv(filename, header=TRUE, na.strings=c("**","-","(X)"))
    data <- data[2:dim(data)[1],] # Dropping unnecessary second row that just describes the variables
    data <- data %>%
      select("geoid"="GEO_ID", "mean_hhinc"="S1902_C03_001E")
    data$mean_hhinc <- as.numeric(data$mean_hhinc)
  }
  # Creating tract geoid variable 'tractgeoidXXXX' to match analysis data's tract geoid variable,
  # which depends on what year of ACS data is being processed
  if (year<2020) {
    data$tractgeoid2010 <- substr(data$geoid, 10, 20)
    data$tractgeoid2020 <- NA
  } else if (year>=2020) {
    data$tractgeoid2010 <- NA
    data$tractgeoid2020 <- substr(data$geoid, 10, 20)
  }
  # Dropping old geoid variable, creating a year variable, and collecting the cleaned data
  data$year <- year
  data <- as.data.frame(data)
  data <- data %>%
    select(year, tractgeoid2010, tractgeoid2020, mean_hhinc)
  rownames(data) <- 1:nrow(data)
  mean_inc <- rbind(mean_inc, data)
}
rm(data)

## Saving cleaned data
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(mean_inc, file="5year_mean_inc.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())