### PURPOSE: Clean and combine the zip-county crosswalk files
### LAST EDITED: 12/5/2024
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

######################################################################################################
### Reading in the raw data, performing basic cleaning, and combining all quarterly datasets into one
### dataset
######################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/HUDUSPS")
years <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022",
           "2023")
quarters <- c("03","06","09","12")
zip_county_crosswalk <- data.frame()
for (i in 1:length(years)) {
  for (j in 1:length(quarters)) {
    year <- years[i]
    quarter <- quarters[j]
    print(paste0("Working on ", year, " Q", quarter))
    ## Reading in the data and reformatting/restructuring
    filename <- paste0("ZIP_COUNTY_",quarter,year,".xlsx")
    data <- read_xlsx(filename, col_names=TRUE)
    colnames(data) <- tolower(colnames(data))
    data <- data %>%
      select(zip, "countyfips"="county", tot_ratio)
    ## For each ZIP, keeping only the county where the highest ratio of the ZIP's addresses
    ## (residential, business, and other) lies, i.e., the county with the highest tot_ratio
    data <- data %>% 
      group_by(zip) %>% 
      slice_max(tot_ratio, n=1, with_ties=FALSE) %>%
      ungroup() %>%
      select(!(tot_ratio))
    ## Creating quarter and year variables and collecting the data in a dataframe, converting quarter variable
    ## from 03, 06, 09, and 12 to 1, 2, 3, and 4 respectively
    data$year <- as.numeric(year)
    data <- data %>%
      mutate(quarter=case_when(
        quarter=="03" ~ 1,
        quarter=="06" ~ 2,
        quarter=="09" ~ 3,
        quarter=="12" ~ 4,
        TRUE ~ NA_real_
      )
      )
    data$quarter <- as.numeric(data$quarter)
    zip_county_crosswalk <- rbind(zip_county_crosswalk, data)
  }
}
rm(data)

zip_county_crosswalk <- as.data.frame(zip_county_crosswalk)
rownames(zip_county_crosswalk) <- 1:nrow(zip_county_crosswalk)

# Saving
zip_county_crosswalk <- zip_county_crosswalk %>%
  select(zip, countyfips, quarter, year)
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(zip_county_crosswalk, file="zip_county_crosswalk.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())