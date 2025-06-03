### PURPOSE: Use a spatial join to assign each station its electric service territory/territories
### LAST EDITED: 1/6/2025
### EDITED FROM: Laptop

library(sf)
library(sp)
library(dplyr)
library(tidyr)

#########################################################################################################
### Reading in shapefile of retail electric service territories
#########################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/HIFLD/Electric_Retail_Service_Territories")
retelec <- st_read("Electric_Retail_Service_Territories.shp")

## Keeping only 2020 data (implicitly assuming that service territories do not change over time) and
## dropping data from outside the contiguous U.S.)
retelec <- retelec %>%
  filter(YEAR=="2020") %>%
  filter(!STATE%in%c("HI","AK","VI","GU","AS","MP"))

#########################################################################################################
### Reading in the analysis dataset  and converting it to an sf object
#########################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
analysis <- read.csv("analysis.csv")
sf_analysis <- st_as_sf(analysis, coords=c("lon","lat"), remove=FALSE, na.fail=FALSE)
# Note that in the st_as_sf() line above, that is the correct coords() order, i.e., lon then lat.

## Changing coordinate reference system (CRS) of obs in analysis to match the CRS of the service territory data
st_crs(retelec) # Just to see what the CRS of the servuce territory data is; it's WGS84
st_crs(sf_analysis) <- "WGS84"
st_crs(sf_analysis) # Same as service territory data now

## Making separate dataset of single station observations (i.e., one row/obs per station) for 
## computational simplicity and to construct a wide dataset of retail electric service territories
  # Note: Creating a wide dataset of service territories is done for stations that are located within
  # the boundaries of multiple service territories. With a wide dataset of service territories, each
  # station will have a separate column for each territory that it is within. For example, station
  # FL8500002 in Gainesville, FL is within 4 retail service territories (City of Newberry (FL), Clay
  # Electric Cooperative, Duke Energy Florida, and Gainesville Regional Utilities)
sing_sf_analysis <- sf_analysis %>% 
  group_by(facility_id) %>% 
  slice_head(n=1) %>%
  ungroup() %>%
  select(facility_id, name, city, county, state, lat, lon, geometry)

#########################################################################################################
### Performing a spatial join w/the sing_sf_analysis data on the left (to get each obs' service territory)
#########################################################################################################
## Spatial join
# Turning off spherical geometry since some service territories' polygons are incorrectly specified
# Not interested in measuring distances between service territory polygons and station coordinates,
# so this is fine.
sf_use_s2(FALSE)
# Performing the spatial join and dropping irrelevant variables; singlestation_util contains each
# station's facility ID and the utility name(s) and utility ID(s) that it belongs
singlestation_util <- st_join(sing_sf_analysis, retelec, join=st_intersects, left=TRUE)
singlestation_util <- as.data.frame(singlestation_util)
singlestation_util <- singlestation_util %>%
  select(facility_id, "utility_id"="ID", "utility_name"="NAME")

## Creating separate variables for each territory matched to a single station by reshaping from long
## to wide format
# Creating match number variable (i.e., a variable for marking each utility match); for example,
# a station matched to two utilities would have 1 and 2 as its n values, whereas a station matched to
# four utilities would have 1, 2, 3, and 4 as its n values.
singlestation_util <- singlestation_util %>% 
  group_by(facility_id) %>% 
  mutate(n=row_number()) %>%
  ungroup()
singlestation_util <- as.data.frame(singlestation_util)
# Keeping long version of singlestation_util for use in "Assigning Max Demand Charges" code
long_singlestation_util <- singlestation_util
# Reshaping singlestation_util to wide in preparation for merging it into analysis data
singlestation_util <- reshape(singlestation_util, direction="wide", idvar="facility_id", timevar="n",
                              v.names=c("utility_id","utility_name"), sep="")
length(which(is.na(singlestation_util$utility_id2)&is.na(singlestation_util$utility_id3)&
               is.na(singlestation_util$utility_id4)&is.na(singlestation_util$utility_id5)&
               is.na(singlestation_util$utility_id6)&is.na(singlestation_util$utility_id7)&
               is.na(singlestation_util$utility_id8)&is.na(singlestation_util$utility_id9)))
  # According to the length() run above, only 13,231 (~37%) stations have been assigned just
  # one utility service territory.
dim(singlestation_util[is.na(singlestation_util$utility_id1),])[1] 
  # According to the dim() run above, 49 stations (~0.1%) have no matched utility.
missingutil <- unique(analysis[analysis$facility_id%in%singlestation_util[is.na(singlestation_util$utility_id1),]$facility_id,]$facility_id)
  # IMPORTANT NOTE: 49 stations weren't assigned a utility because their coordinates were OFF such that their 
  # location was marked as being in the ocean or not in any territory when their street address places them 
  # within a territory. Went back and manually investigated each of the 49 stations to see which are truly not 
  # in any service territory and which just have inaccurate coordinates. Some stations are also missing 
  # coordinate data, which would also cause them to not get assigned a utility territory.
## Manually assigning stations w/erroneous coordinates their utility
# Wide single station dataset
singlestation_util <- singlestation_util %>%
  mutate(utility_id1=case_when(
    facility_id%in%c("FL8624720","FL9501795","FL9800972","FL9804044","FL9804675") ~ "6452",
    facility_id=="FL9045599" ~ "6455",
    facility_id=="NM30620" ~ "15473",
    facility_id%in%c("VA6004597","VA6042447") ~ "19876",
    facility_id=="WA49279288" ~ "15500",
    TRUE ~ utility_id1
  )
  ) %>%
  mutate(utility_name1=case_when(
    facility_id%in%c("FL8624720","FL9501795","FL9800972","FL9804044","FL9804675") ~ "FLORIDA POWER & LIGHT CO",
    facility_id=="FL9045599" ~ "DUKE ENERGY FLORIDA, LLC",
    facility_id=="NM30620" ~ "PUBLIC SERVICE CO OF NM",
    facility_id%in%c("VA6004597","VA6042447") ~ "VIRGINIA ELECTRIC & POWER CO",
    facility_id=="WA49279288" ~ "PUGET SOUND ENERGY INC",
    TRUE ~ utility_name1
  )
  )
# Long single station dataset
long_singlestation_util <- long_singlestation_util %>%
  mutate(utility_id=case_when(
    facility_id%in%c("FL8624720","FL9501795","FL9800972","FL9804044","FL9804675") ~ "6452",
    facility_id=="FL9045599" ~ "6455",
    facility_id=="NM30620" ~ "15473",
    facility_id%in%c("VA6004597","VA6042447") ~ "19876",
    facility_id=="WA49279288" ~ "15500",
    TRUE ~ utility_id
  )
  ) %>%
  mutate(utility_name=case_when(
    facility_id%in%c("FL8624720","FL9501795","FL9800972","FL9804044","FL9804675") ~ "FLORIDA POWER & LIGHT CO",
    facility_id=="FL9045599" ~ "DUKE ENERGY FLORIDA, LLC",
    facility_id=="NM30620" ~ "PUBLIC SERVICE CO OF NM",
    facility_id%in%c("VA6004597","VA6042447") ~ "VIRGINIA ELECTRIC & POWER CO",
    facility_id=="WA49279288" ~ "PUGET SOUND ENERGY INC",
    TRUE ~ utility_name
  )
  )

######################################################################################################
### Adding the utility name and utility ID data to the main analysis dataset and saving
######################################################################################################
## Adding the utility data and, AS OF 1/27/2023, only keeping the first two matched utilities (i.e.,
## utility_id1 and utility_id2); barring the discovery of some systematic selection method for 
## determining which matched utility is actually servicing a station
analysis <- left_join(analysis, singlestation_util, by="facility_id")

## Saving the analysis dataset
write.csv(analysis, file="analysis.csv", row.names=FALSE)