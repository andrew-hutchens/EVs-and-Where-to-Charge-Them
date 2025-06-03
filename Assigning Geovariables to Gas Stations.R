### PURPOSE: Assign geovariables to UST data on gas stations
### LAST EDITED: 12/23/2024
### EDITED FROM: Laptop

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

##################################################################################################################
### Assigning county name, county FIPS, MSA name, MSA ID, and census tract codes to all gas stations
##################################################################################################################
## Setting the working directory and reading in the gas stations UST data
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
gasstations <- read.csv("gasstations.csv")

### Creating dataframe of just unique facility ID-lat-lon combos to iterate through Census Geocoder to pull
### geographical data (less observations to iterate through, so should be quicker than iterating through every
### observation in gasstations since gasstations has >1 obs for many facilities that have >1 tank).
facilities <- gasstations %>%
  group_by(facility_id) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(facility_id, lat, lon)
facilities <- as.data.frame(facilities)
rownames(facilities) <- 1:nrow(facilities)

## Assigning geovariables using the Census' Geocoder API
## Pulling BOTH 2020 AND 2020 Census census tract data (geo ID and individual tract codes)
## Pulling 2020 Census data
# Querying the Census Geocoder using their API
for (i in 1:dim(facilities)[1]){
  print(paste0("Working on observation ", i, " of ", dim(facilities)[1]))
  result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                query=list(benchmark="Public_AR_Current",
                           vintage="Current_Current",
                           y=facilities$lat[i],
                           x=facilities$lon[i],
                           format="json"))
  if (result$status_code==502) {
    print(paste0("Observation ", i, " is not available in Geocoder"))
  } else if (result$status_code==400) {
    print(paste0("Observation ", i, " has NA for lat/long"))
  } else if (result$url=="https://outage.census.gov/unavailable.html") {
    print(paste0("Reworking observation ", i))
    result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                  query=list(benchmark="Public_AR_Current",
                             vintage="Current_Current",
                             y=facilities$lat[i],
                             x=facilities$lon[i],
                             format="json"))
    result2 <- fromJSON(rawToChar(result$content))
    
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      facilities$msa_name2020[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      facilities$csa_id2020[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      facilities$msa_name2020[i] <- NA
      facilities$csa_id2020[i] <- NA
    }
    # Pulling county name
    if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)!=0) {
      facilities$county[i] <- result2[[1]][["geographies"]][["Counties"]]$BASENAME
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)==0) {
      facilities$county[i] <- NA
    }
    # Pulling county FIPS code
    if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)!=0) {
      facilities$countyfips[i] <- as.numeric(result2[[1]][["geographies"]][["Counties"]]$GEOID)
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)==0) {
      facilities$countyfips[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      facilities$tractgeoid2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      facilities$tractgeoid2020[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      facilities$tract2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      facilities$tract2020[i] <- NA
    }
  } else if (result$url!="https://outage.census.gov/unavailable.html"&result$status_code!=502&
             result$status_code!=400) {
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      facilities$msa_name2020[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      facilities$csa_id2020[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      facilities$msa_name2020[i] <- NA
      facilities$csa_id2020[i] <- NA
    }
    # Pulling county name
    if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)!=0) {
      facilities$county[i] <- result2[[1]][["geographies"]][["Counties"]]$BASENAME
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)==0) {
      facilities$county[i] <- NA
    }
    # Pulling county FIPS code
    if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)!=0) {
      facilities$countyfips[i] <- as.numeric(result2[[1]][["geographies"]][["Counties"]]$GEOID)
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)==0) {
      facilities$countyfips[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      facilities$tractgeoid2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      facilities$tractgeoid2020[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      facilities$tract2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      facilities$tract2020[i] <- NA
    }
  }
}

## Pulling 2010 Census data (to use in years pre-2020; census tract definitions changed for the 2020 Census)
## Note: not re-obtaining county data since it'll be the same as in the 2020 Census
# Querying the Census Geocoder using their API
for (i in 23944:dim(facilities)[1]){
  print(paste0("Working on observation ", i, " of ", dim(facilities)[1]))
  result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                query=list(benchmark="Public_AR_Current",
                           vintage="Census2010_Current",
                           y=facilities$lat[i],
                           x=facilities$lon[i],
                           format="json"))
  if (result$status_code==502) {
    print(paste0("Observation ", i, " is not available in Geocoder"))
  } else if (result$status_code==400) {
    print(paste0("Observation ", i, " has NA for lat/long"))
  } else if (result$url=="https://outage.census.gov/unavailable.html") {
    print(paste0("Reworking observation ", i))
    result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                  query=list(benchmark="Public_AR_Current",
                             vintage="Census2010_Current",
                             y=facilities$lat[i],
                             x=facilities$lon[i],
                             format="json"))
    result2 <- fromJSON(rawToChar(result$content))
    
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      facilities$msa_name2010[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      facilities$csa_id2010[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      facilities$msa_name2010[i] <- NA
      facilities$csa_id2010[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      facilities$tractgeoid2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      facilities$tractgeoid2010[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      facilities$tract2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      facilities$tract2010[i] <- NA
    }
  } else if (result$url!="https://outage.census.gov/unavailable.html"&result$status_code!=502&
             result$status_code!=400) {
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      facilities$msa_name2010[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      facilities$csa_id2010[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      facilities$msa_name2010[i] <- NA
      facilities$csa_id2010[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      facilities$tractgeoid2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      facilities$tractgeoid2010[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      facilities$tract2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      facilities$tract2010[i] <- NA
    }
  }
}
rm(result, result2)
facilities <- facilities %>%
  select(!c(lat, lon))

## Merging geographical data in 'facilities' dataframe into 'gasstations' dataframe
gasstations <- left_join(gasstations, facilities, by="facility_id")
gasstations <- gasstations %>%
  select(!(county.x)) %>%
  rename("county"="county.y") %>%
  select(facility_id, name, address, city, county, state, zip, lat, lon, everything())

## Saving new, complete gas station dataset and 'facilities' dataframe (to avoid rerunning time-consuming
## code above if the need to recreate gasstations arises)
write.csv(gasstations, file="gasstations.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())