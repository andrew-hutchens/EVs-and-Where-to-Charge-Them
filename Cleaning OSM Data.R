### PURPOSE: Clean OpenStreetMap (OSM) gas station location data (i.e., get from geoJSON to csv)
### LAST EDITED: 12/3/2024
### EDITED FROM: Laptop

install.packages("geojsonR")
library(foreach)
library(geosphere)
library(osmdata)
library(tm)
library(tidyr)
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
library(geojsonR)
library(dplyr)

####################################################################################################
### Reading in the raw data and converting it to an interpretable table format
####################################################################################################
## Setting the initial working directory and cleaning the raw data, creating a separate cleaned
## dataframe for each region (west, middle, and east)
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/OSM")
regions <- c("west","middle","east")
for (i in 1:length(regions)) {
  # Reading in raw data
  filename <- paste0(regions[i],".geojson")
  result <- FROM_GeoJson(filename)
  # Collecting characteristics data (e.g., amenity, brand, and name) into a dataframe
  charsets <- list()
  for (j in 1:length(result$features)) {
    charsets[[j]] <- as.data.frame(result$features[[j]]$properties)
  }
  characteristics <- do.call(plyr::rbind.fill, charsets)
  # Only keeping amenity, name, and brand variables for now
  characteristics <- characteristics[c("amenity","name","brand")] # Note that 4 obs have NAs for 
                                                                  # amenity, but they were identified
                                                                  # using another, similar amenity
                                                                  # variable, so no problem here.
  # Collecting coordinates data into a dataframe
  coordsets <- list()
  for (k in 1:length(result$features)) {
    if (class(result$features[[k]]$geometry$coordinates)[1]=="matrix") {
      input <- as.data.frame(result$features[[k]]$geometry$coordinates)[1,1:2]
    } else if (class(result$features[[k]]$geometry$coordinates)[1]=="list") {
      if (length(result$features[[k]]$geometry$coordinates)==1) {
        input <- as.data.frame(result$features[[k]]$geometry$coordinates[[1]][[1]])[1,1:2]
      } else if (length(result$features[[k]]$geometry$coordinates)!=1) {
        input <- as.data.frame(result$features[[k]]$geometry$coordinates[[1]])[1,1:2]
      }
    } else if (class(result$features[[k]]$geometry$coordinates)[1]=="numeric") {
      input <- data.frame(matrix(ncol=2,nrow=1))
      colnames(input) <- c("lon","lat")
      input$lon <- as.numeric(result$features[[k]]$geometry$coordinates[1])
      input$lat <- as.numeric(result$features[[k]]$geometry$coordinates[2])
    }
    colnames(input) <- c("lon","lat")
    input$lon <- as.numeric(input$lon)
    input$lat <- as.numeric(input$lat)
    coordsets[[k]] <- input
    rm(input)
  }
  coordinates <- do.call(plyr::rbind.fill, coordsets)
  
  # Combining characteristics and coordinates dataframes (not a merge because both dataframes
  # are in the same order on an observation-by-observation basis)
  combined <- cbind(characteristics, coordinates)
  combined <- combined[c("lon","lat","name","brand","amenity")]
  assign(paste0(regions[i],"_stations"), combined)
}

## Combining each region's cleaned dataframe into one final cleaned OSM dataframe
osm_stations <- rbind(west_stations, middle_stations, east_stations)

## Identifying each OSM station's state using Census geocoding and dropping non-U.S. observations
## that were picked up in the raw data download (used manual bounding box)
# Note: Observations w/NA for state have NA for state because they are not in the U.S. (they are in Canada or
# Mexico and thus do not have data in the Census Geocoder, hence no state data)
# Note: Using 2020 Census data for this
osm_stations$state <- NA
for (i in 1:dim(osm_stations)[1]){
  print(paste0("Working on observation ", i))
  result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                query=list(benchmark="Public_AR_Current",
                           vintage="Current_Current",
                           y=osm_stations$lat[i],
                           x=osm_stations$lon[i],
                           format="json"))
  # Conditioning on Geocoder timeout; telling loop to reattempt an observation's coordinates
  # when the observation's API call happens to coincide with a timeout of the Census Geocoder API
  if (result$url=="https://outage.census.gov/unavailable.html") {
    print(paste0("Reworking observation ", i))
    result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                  query=list(benchmark="Public_AR_Current",
                             vintage="Current_Current",
                             y=osm_stations$lat[i],
                             x=osm_stations$lon[i],
                             format="json"))
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling only state name
    if (length(result2$result$geographies)!=0) {
      osm_stations$state[i] <- result2$result$geographies$States$STUSAB
    }
  } else if (result$url!="https://outage.census.gov/unavailable.html") {
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling only state name
    if (length(result2$result$geographies)!=0) {
      osm_stations$state[i] <- result2$result$geographies$States$STUSAB
    }
  }
}
osm_stations <- osm_stations[!is.na(osm_stations$state),]

# Saving the cleaned OSM data
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(osm_stations, file="osm_stations.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())

# Note: the 'osm_stations' dataframe now has the locations of all gas stations. Now you just need to
# "merge" it with the EPA UST data to identify what observations in the UST data are gas stations.
# Merge is in quotation marks because you won't actually merge; what you'll actually do is geolocate
# each UST facility's nearest observation in the 'osm_stations' dataframe. UST facilities w/a neighbor
# in the 'osm_stations' dataframe will then be marked as gas stations.

# Note: STILL need to watch out for stations that are located at supermarket chains (e.g., Safeway and
# Costco). This is addressed in subsequent scripts.