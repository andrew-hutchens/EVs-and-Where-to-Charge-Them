### PURPOSE: Clean EPA UST facilities and tanks data and create one dataframe containing data on both
### LAST EDITED: 11/6/2024
### EDITED FROM: Laptop

install.packages("tidytext")
install.packages("wordcloud")
install.packages("tm")

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

## Setting the working directory, loading the raw facilities and tank data, and dropping unnecessary columns
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/EPA UST")
facilities <- read.csv("ust_facilities.csv")
facilities <- facilities %>%
  select(!c(OBJECTID, Address_Match_Type, EPA_Region, Tribe))
tanks <- read.csv("ust_tanks.csv")
tanks <- tanks %>%
  select(!c(OBJECTID))

#########################################################################################################
#### Facilities
#########################################################################################################
## Dropping non-contiguous states/territories (only 2 obs have blanks for state; will just drop them) and obs w/no open USTs
## Also renaming variables and removing duplicate entries of the same station (or facility_id)
facilities <- facilities %>%
  filter(!State%in%c("Commonwealth of the Northern Mariana Islands", "American Samoa", "U.S. Virgin Islands", 
                     "Puerto Rico", "Guam", "Hawaii", "Alaska", "")) %>%
  filter(Open_USTs!=0) %>%
  group_by(Facility_ID) %>%
  mutate(n=row_number()) %>%
  filter(n==1) %>%
  ungroup() %>%
  select(!(n)) %>%
  rename("zip"="Zip_Code", "lat"="Latitude", "lon"="Longitude")
colnames(facilities) <- tolower(colnames(facilities))
# NOTE: 81 facilities have missing lat/lon.

#########################################################################################################
#### Tanks
#########################################################################################################
## Dropping non-contiguous states/territories (only 55 obs have blanks for state; will just drop them), only keeping open
## tanks, (1580 obs have blanks for tank status, will drop them to be safe; 32113 obs have a tank status of "Temporarily", 
## "Temporarily out of Service", or "Temporarily Out of Service", all of which will just be dropped /// Also creating separate
## installation and removal date variables /// Also creating a no_install_date flagging variable for tanks w/blank for
## installation date
tanks <- tanks %>%
  filter(!State%in%c("Commonwealth of the Northern Mariana Islands", "American Samoa", "U.S. Virgin Islands", 
                     "Puerto Rico", "Guam", "Hawaii", "Alaska", "")) %>%
  select(!(State)) %>%
  mutate(no_install_date=case_when(
    Installation_Date=="" ~ 1, 
    TRUE ~ 0
    )
    ) %>%
  separate(col=Installation_Date, into=c("install_date","install_time"), sep=" ", 
           remove=FALSE, convert=TRUE) %>%
  separate(col=Removal_Date, into=c("removal_date","removal_time"), sep=" ", 
           remove=FALSE, convert=TRUE) %>%
  separate(col=install_date, into=c("install_month","install_day","install_year"), 
           sep="/", remove=FALSE, convert=TRUE) %>%
  separate(col=removal_date, into=c("remove_month","remove_day","remove_year"), 
           sep="/", remove=FALSE, convert=TRUE) %>%
  select(!c(Installation_Date, Removal_Date, install_time, removal_time))
colnames(tanks) <- tolower(colnames(tanks))

## Keeping obs w/Open tank status <- NOT DOING THIS AS OF 11/5/2024 SINCE TANKS W/CLOSED STATUS WERE OPEN AT SOME POINT, 
##                                   POSSIBLY DURING THE SAMPLE PERIOD. THUS, THEY SHOULD COUNT TOWARDS A STATION'S 
##                                   (POSSIBLY TIME-VARYING) CAPACITY, if the station appears in the tanks data
## Keeping obs w/a blank removal date (this presumably means that they are still in the ground)<-NOT DOING THIS AS OF 11/5/2024
#tanks <- tanks[tanks$Removal_Date=="",]

tankfacilitiesnotinfacilities <- tanks %>% filter(!facility_id%in%facilities$facility_id)
tankfacilitiesnotinfacilities <- unique(tankfacilitiesnotinfacilities$facility_id)

########################################################################################################
#### Merging facilities and tanks, then summarizing capacity and substance data at the facility level
########################################################################################################
mergedust <- left_join(facilities, tanks, by="facility_id")
mergedust <- mergedust %>%
  filter(!is.na(tank_id)) %>%
  select(facility_id, name, address, city, county, state, zip, lat, lon, open_usts, closed_usts, 
         facility_status, tank_id, tank_status, capacity, install_date, install_month, install_day, 
         install_year, removal_date, remove_month, remove_day, remove_year, no_install_date) %>%
  mutate(tank_removed=case_when(
    removal_date=="" ~ 0,
    TRUE ~ 1
  )
  )

# Saving merged dataframe
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(mergedust, file="mergedust.csv", row.names=FALSE)
# NOTE: 'mergedust' contains ALL data on ALL USTs of ALL types, not just gas stations.