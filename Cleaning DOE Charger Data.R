### PURPOSE: Clean DOE charger data
### LAST EDITED: 12/12/2024
### EDITED FROM: Laptop

### NOTE: The 'id' variable in the DOE charger data identifies INDIVIDUAL CHARGERS, NOT CHARGER LOCATIONS. So it is
### possible to have multiple observations at the same address because of there being multiple chargers at the
### same address/location.

### NOTE: Thus, the n_levelX variables (where X is 1, 2, or dc fast) measures the number of PORTS at each charger.
### For example, charger A with ID 7878 has 2 Level 2 ports, which means that it will have 2 for n_level2. Charger B,
### which is also located at the same address/location as Charger A, could have 3 Level 2 ports, meaning that it will
### have 3 for n_level2 but will have its own ID. Note that in this case, the address (for Charger A and Charger B) 
### will have 2 observations.

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
library(lubridate)

## Setting the working directory and loading the data
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/DOE")
chargerdata <- read.csv("doe_public_stations.csv")

## Creating separate (numeric) year, month, and day variables; these are EVSE OPENING DATES (some facilities
## have a blank opening date; 6 to be precise)
chargerdata <- separate(chargerdata, col=Open.Date, into=c("evse_open_year","evse_open_month","evse_open_day"), 
                        sep="-", remove=FALSE, convert=TRUE)
## Dropping HI, AK, ON (Ontario), and PR obs, filtering obs, and renaming/selecting relevant variables
chargerdata <- chargerdata %>%
  rename("lat"="Latitude", "lon"="Longitude", "station_name"="Station.Name", "street_address"="Street.Address", 
         "city"="City", "state"="State", "zip"="ZIP", "status_code"="Status.Code", 
         "groups_with_access_code"="Groups.With.Access.Code", "access_days_time"="Access.Days.Time",
         "cards_accepted"="Cards.Accepted", "n_level1"="EV.Level1.EVSE.Num", "n_level2"="EV.Level2.EVSE.Num",
         "n_dcfast"="EV.DC.Fast.Count", "ev_other_info"="EV.Other.Info", "ev_network"="EV.Network",
         "ev_network_web"="EV.Network.Web", "id"="ID", "owner_type_code"="Owner.Type.Code", 
         "federal_agency_id"="Federal.Agency.ID", "federal_agency_name"="Federal.Agency.Name",
         "open_date"="Open.Date", "access_code"="Access.Code", "access_detail_code"="Access.Detail.Code",
         "federal_agency_code"="Federal.Agency.Code", "facility_type"="Facility.Type", 
         "ev_pricing"="EV.Pricing", "restricted_access"="Restricted.Access", 
         "ev_workplace_charging"="EV.Workplace.Charging") %>%
  filter(!state%in%c("HI","AK","PR","ON")) %>%
  filter(lat!=1 & lon!=1) %>%
  filter(lat!=-1 & lon!=-1) %>%
  select(id, station_name, street_address, city, state, zip, status_code, n_level1, n_level2, n_dcfast, 
         ev_network, open_date, facility_type, ev_pricing, evse_open_year, evse_open_month, evse_open_day, 
         lat, lon, status_code, groups_with_access_code, access_days_time, cards_accepted, ev_other_info, 
         ev_network_web, owner_type_code, federal_agency_id, federal_agency_name, federal_agency_code, 
         access_code, access_detail_code, restricted_access, ev_workplace_charging) %>%
  mutate(n_level1=case_when(
    is.na(n_level1) ~ 0,
    TRUE ~ as.numeric(n_level1)
  ), n_level2=case_when(
    is.na(n_level2) ~ 0,
    TRUE ~ as.numeric(n_level2)
  ), n_dcfast=case_when(
    is.na(n_dcfast) ~ 0,
    TRUE ~ as.numeric(n_dcfast)
  )
  )
View(chargerdata %>% group_by(id) %>% summarise(n=n()) %>% filter(n>1)) # Each charger ID appears once
View(chargerdata %>% group_by(street_address) %>% summarise(n=n()) %>% filter(n>1)) 
  # Some addresses have >1 charger

View(table(chargerdata$year))
table(chargerdata$year, chargerdata$month)
any(is.na(chargerdata$Open.Date))
any(chargerdata$Open.Date=="")
length(which(chargerdata$Open.Date=="")) # Only 6 obs don't have a date, NBD

################################################################################
## Assigning an MSA name, CSA code, county name, county FIPS, and census tract code to each observation;
## Pulling BOTH 2010 AND 2020 Census census tract data (geo ID and individual tract codes)
################################################################################
## NOTE: If running for first time, change the starting index to 1!
## NOTE: Some observations' lat/lon simply are not available in the Geocoder (e.g., lat=37.07541 and
## lon=-84.61809 corresponding to station "Don Franklin Nissan - Somerset").

## Pulling 2020 Census data
# Querying the Census Geocoder using their API
for (i in 1:dim(chargerdata)[1]){
  print(paste0("Working on observation ", i))
  result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                query=list(benchmark="Public_AR_Current",
                           vintage="Current_Current",
                           y=chargerdata$lat[i],
                           x=chargerdata$lon[i],
                           format="json"))
  
  if (result$status_code==502) {
    print(paste0("Observation ", i, " is not available in Geocoder"))
  } else if (result$url=="https://outage.census.gov/unavailable.html") {
    print(paste0("Reworking observation ", i))
    result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                  query=list(benchmark="Public_AR_Current",
                             vintage="Current_Current",
                             y=chargerdata$lat[i],
                             x=chargerdata$lon[i],
                             format="json"))
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      chargerdata$msa_name2020[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      chargerdata$csa_id2020[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      chargerdata$msa_name2020[i] <- NA
      chargerdata$csa_id2020[i] <- NA
    }
    # Pulling county name
    if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)!=0) {
      chargerdata$county[i] <- result2[[1]][["geographies"]][["Counties"]]$BASENAME
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)==0) {
      chargerdata$county[i] <- NA
    }
    # Pulling county FIPS code
    if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)!=0) {
      chargerdata$countyfips[i] <- as.numeric(result2[[1]][["geographies"]][["Counties"]]$GEOID)
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)==0) {
      chargerdata$countyfips[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      chargerdata$tractgeoid2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      chargerdata$tractgeoid2020[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      chargerdata$tract2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      chargerdata$tract2020[i] <- NA
    }
  } else if (result$url!="https://outage.census.gov/unavailable.html"&result$status_code!=502) {
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      chargerdata$msa_name2020[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      chargerdata$csa_id2020[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      chargerdata$msa_name2020[i] <- NA
      chargerdata$csa_id2020[i] <- NA
    }
    # Pulling county name
    if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)!=0) {
      chargerdata$county[i] <- result2[[1]][["geographies"]][["Counties"]]$BASENAME
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$BASENAME)==0) {
      chargerdata$county[i] <- NA
    }
    # Pulling county FIPS code
    if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)!=0) {
      chargerdata$countyfips[i] <- as.numeric(result2[[1]][["geographies"]][["Counties"]]$GEOID)
    } else if (length(result2[[1]][["geographies"]][["Counties"]]$GEOID)==0) {
      chargerdata$countyfips[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      chargerdata$tractgeoid2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      chargerdata$tractgeoid2020[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      chargerdata$tract2020[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      chargerdata$tract2020[i] <- NA
    }
  }
}
rm(result, result2)

## Pulling 2010 Census data (to use in years pre-2020; census tract definitions changed for the 2020 Census)
## Note: not re-obtaining county data since it'll be the same as in the 2020 Census
# Querying the Census Geocoder using their API
for (i in 38849:dim(chargerdata)[1]){
  print(paste0("Working on observation ", i))
  result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                query=list(benchmark="Public_AR_Current",
                           vintage="Census2010_Current",
                           y=chargerdata$lat[i],
                           x=chargerdata$lon[i],
                           format="json"))
  
  if (result$status_code==502) {
    print(paste0("Observation ", i, " is not available in Geocoder"))
  } else if (result$url=="https://outage.census.gov/unavailable.html") {
    print(paste0("Reworking observation ", i))
    result <- GET("https://geocoding.geo.census.gov/geocoder/geographies/coordinates", 
                  query=list(benchmark="Public_AR_Current",
                             vintage="Census2010_Current",
                             y=chargerdata$lat[i],
                             x=chargerdata$lon[i],
                             format="json"))
    result2 <- fromJSON(rawToChar(result$content))
    
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      chargerdata$msa_name2010[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      chargerdata$csa_id2010[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      chargerdata$msa_name2010[i] <- NA
      chargerdata$csa_id2010[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      chargerdata$tractgeoid2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      chargerdata$tractgeoid2010[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      chargerdata$tract2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      chargerdata$tract2010[i] <- NA
    }
  } else if (result$url!="https://outage.census.gov/unavailable.html"&result$status_code!=502) {
    result2 <- fromJSON(rawToChar(result$content))
    # Pulling MSA name and CSA ID
    if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)!=0) {
      chargerdata$msa_name2010[i] <- result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME
      chargerdata$csa_id2010[i] <- as.numeric(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$CSA)
    } else if (length(result2[[1]][["geographies"]][["Combined Statistical Areas"]]$BASENAME)==0) {
      chargerdata$msa_name2010[i] <- NA
      chargerdata$csa_id2010[i] <- NA
    }
    # Pulling complete GEOID (FIPS and census tract code concatenated)
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)!=0) {
      chargerdata$tractgeoid2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$GEOID
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$GEOID)==0) {
      chargerdata$tractgeoid2010[i] <- NA
    }
    # Pulling 6-digit census tract code only
    if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)!=0) {
      chargerdata$tract2010[i] <- result2[[1]][["geographies"]][["Census Tracts"]]$TRACT
    } else if (length(result2[[1]][["geographies"]][["Census Tracts"]]$TRACT)==0) {
      chargerdata$tract2010[i] <- NA
    }
  }
}
rm(result, result2)

## Seeing how many obs have no charger count data at all --> 3
dim(chargerdata[chargerdata$n_level1==0 & chargerdata$n_level2==0 & chargerdata$n_dcfast==0,])[1]
any(is.na(chargerdata$n_level1)) # None
any(is.na(chargerdata$n_level2)) # None
any(is.na(chargerdata$n_dcfast)) # None

## Creating total number of chargers variable (summing across L2 and L3-DC)
# Any 0s should be dropped since these are the 3 obs that don't have charger count data
chargerdata$totchargers <- rowSums(chargerdata[,colnames(chargerdata)%in%c("n_level1","n_level2",
                                                      "n_dcfast")], na.rm=TRUE)
any(is.na(chargerdata$totchargers)) # None, false
any(chargerdata$totchargers==0)
chargerdata <- chargerdata %>%
  filter(totchargers!=0) # Removing observations that don't have data on charger count (only 3 of these as of 12/6/2024)

## Creating date object for EVSE opening date
chargerdata <- chargerdata %>%
  mutate(open_date=ymd(open_date))

## Seeing if any stations installed >1 type of charger (e.g., both Level 2 and Level 3)
length(which(chargerdata$n_level1!=0 & chargerdata$n_level2!=0)) # 92 stations w/L1 & L2
length(which(chargerdata$n_level1!=0 & chargerdata$n_dcfast!=0)) # 3 stations w/L1 & L3
length(which(chargerdata$n_level2!=0 & chargerdata$n_dcfast!=0)) # 1,203 stations w/L2 & L3
  # YES, some stations have >1 type of charger installed.
length(which(chargerdata$n_level1!=0 & chargerdata$n_level2!=0 & chargerdata$n_dcfast!=0))
  # 3 stations w/L1, L2, and L3 charging

## Creating dummies to indicate the presence of L1, L2, and/or L3 charging
chargerdata$level1 <- ifelse(!is.na(chargerdata$n_level1) & chargerdata$n_level1!=0, 1, 0)
chargerdata$level2 <- ifelse(!is.na(chargerdata$n_level2) & chargerdata$n_level2!=0, 1, 0)
chargerdata$level3 <- ifelse(!is.na(chargerdata$n_dcfast) & chargerdata$n_dcfast!=0, 1, 0)

## Creating dummy for Tesla chargers
chargerdata$tesla <- ifelse(chargerdata$ev_network=="Tesla"|chargerdata$ev_network=="Tesla Destination", 1, 0)

## Creating start dates for each EV charging type (i.e., intro date for Level 1, Level 2, Level 3 non-Tesla, and
## Level 3 Tesla)
chargerdata$l1_date <- as.Date("2000-01-01", format="%Y-%m-%d")
chargerdata$l2_date <- as.Date("2010-01-01", format="%Y-%m-%d")
chargerdata$l3ntchademo_date <- as.Date("2016-03-01", format="%Y-%m-%d")
chargerdata$l3ntccs_date <- as.Date("2012-06-01", format="%Y-%m-%d")
chargerdata$l3tesla_date <- as.Date("2012-09-01", format="%Y-%m-%d")

## Saving current 'chargerdata' dataframe as preliminary charging location data (all stations);
## updated Nov 2024
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
chargerdata <- chargerdata %>%
  select(id, station_name, street_address, city, state, zip, status_code, n_level1, n_level2, n_dcfast, totchargers, 
         everything())
write.csv(chargerdata, file="chargerdata.csv", row.names=FALSE)
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/DOE")
rm(chargerdata, i)
## NOTE: In this December 2023 data, there are 357 locations that are gas stations (that have
## facility_type=="GAS_STATION") and XXX locations that are convenience stores (that have
## faclity_type=="CONVENIENCE_STORE", but some are not gas stations!)...keep this in mind.