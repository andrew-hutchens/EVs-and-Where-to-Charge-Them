### PURPOSE: Calculate each station's distance from a major highway or highway corridor and add it as
### a variable in the analysis data
### LAST EDITED: 2/4/2025
### EDITED FROM: Laptop

library(lwgeom)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(geosphere)
library(ggplot2)
library(maps)
library(mapdata)
library(usmap)
library(nngeo)

# Note: According to the TIGER Shapefiles Technical Documentation, the "Primary and Secondary Roads
# Shapefile contains all linear street features with MTFCCs of primary roads (S1100) or secondary roads
# (S1200) in the MTS. Secondary roads are main arteries, usually in the U.S. highway, state highway, or
# county highway system. These roads have one or more lanes of traffic in each direction, may or not be
# divided, and usually have at-grade intersections with many other roads and driveways."

# Note: RTTYP is route type and seems to follow these definitions - S=state road, U=U.S. highway,
# I=Interstate highway, C=county highway, O=other, and M="common name" (see technical documentation).

# Note: Proximity to highways is probably more important to stations in rural areas than to stations
# in urban areas (densely populated).

################################################################################
### Reading in the raw TIGER shapefiles and cleaned analysis data
################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/TIGER/Primary and Secondary")
## Reading in individual state files and combining them in one dataframe
usroads <- data.frame()
statecodes <- c("01","04","05","06","08","09","10","11","12","13","16","17","18","19","20","21","22",
                "23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39",
                "40","41","42","44","45","46","47","48","49","50","51","53","54","55","56")
for (i in 1:length(statecodes)) {
  code <- statecodes[i]
  filename <- paste0("tl_2022_",code,"_prisecroads.shp")
  file <- st_read(filename)
  usroads <- rbind(usroads, file)
}
colnames(usroads) <- tolower(colnames(usroads))
rm(statecodes, code, i, filename)

## Loading the analysis and complete station datasets
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
analysis <- read.csv("analysis.csv")
gas_complete <- read.csv("gas_complete.csv")

################################################################################
### Calculating each station's distance from an interstate or U.S. highway
################################################################################
## Dataset of station IDs and their coordinates in sf object form
length(unique(analysis[is.na(analysis$lat),]$facility_id)) # 4 stations have NA for lat/lon, so will
                                                           # kick these out below
# Removing stations with NA for lat/lon and constructing sf object version of analysis data's coordinates
analysis <- analysis[!is.na(analysis$lat),]
stationcoords <- analysis %>% 
  select(facility_id, lon, lat) %>%
  group_by(facility_id) %>% 
  filter(row_number()==1) %>%
  ungroup()
stationcoords <- as.data.frame(stationcoords)
stationcoords_sf <- stationcoords %>% 
  st_as_sf(coords=c("lon","lat")) %>% 
  st_set_crs(4326)

## Setting the coordinate reference system for the usroads dataset to be the same as the station
## coordinates dataset
usroads <- st_transform(usroads, crs=st_crs(stationcoords_sf))

## Calculating stations' distance to its nearest interstate or U.S. highway
# Creating dataset of only interstate highways
inter <- usroads %>%
  filter(!is.na(rttyp) & rttyp=="I")
rownames(inter) <- 1:nrow(inter)
# Finding nearest interstate first
start_time <- Sys.time()
nearesthwy <- st_nearest_feature(stationcoords_sf, inter)
end_time <- Sys.time()
end_time-start_time
rm(start_time, end_time)
# Calculating distance between station coordinates and their nearest interstate highway (distance is in meters)
dist <- st_distance(stationcoords_sf, inter[nearesthwy,], by_element=TRUE) 
dist <- as.numeric(dist)
# Adding distance to nearest interstate highway into the original station coordinates data
stationcoords$disthwy <- dist

## Adding distance to nearest interstate/U.S. highway into the analysis data
# Creating dataset of facilities and their distance to nearest interstate/U.S. highway
stationhwy <- stationcoords[c("facility_id","disthwy")]
# Merging in the distance to highway variable
analysis <- left_join(analysis, stationhwy, by="facility_id")
analysis <- analysis %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date, 
         n_evs, maxcharge, disthwy, mean_hhinc, tractpop, countypop, everything())
# Saving the analysis data
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(analysis, file="analysis.csv", row.names=FALSE)