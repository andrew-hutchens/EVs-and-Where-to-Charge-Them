### PURPOSE: Map stations (adapted and unadapted) and tally the number of stations by tract and other geographic
### areas if necessary
### LAST EDITED: 12/23/2024
### EDITED FROM: Laptop

# Installing choroplethrZip from Github (not CRAN) for this version of R (CRAN version doesn't work
# on this version of R)
install_github('arilamstein/choroplethrZip@v1.5.0')

install.packages("choroplethrMaps")

library(choroplethrMaps)
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

# choroplethrZip stuff
library(devtools)
library(choroplethrZip)

############################################################################################################
### Setting the working directory and loading the data
############################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
gas_complete <- read.csv("gas_complete.csv")
chargerdata <- read.csv("chargerdata.csv")

############################################################################################################
### Point plots of adapted stations, unadapted stations, and chargers
############################################################################################################
## Creating a map of all chargers but distinguishing by level
adaptedpoints <- unique(gas_complete[!is.na(gas_complete$id),][c("lat","lon","level1","level2","level3")])
colors <- c("Level 1"="darkolivegreen3","Level 2"="darkturquoise","Level 3"="darkorange")
setwd("G:/My Drive/EV Charging/Images")
jpeg("adapted gas stations.jpg", width=800, height=600, quality=100, res=110)
ggplot(adaptedpoints, aes(x=lon)) + borders("state") +
  geom_point(data=adaptedpoints[adaptedpoints$level1==1,], aes(y=lat, colour="Level 1"), size=2,
             alpha=0.5, shape=25) + 
  geom_point(data=adaptedpoints[adaptedpoints$level2==1,], aes(y=lat, colour="Level 2"), size=2,
             alpha=0.5, shape=17) + 
  geom_point(data=adaptedpoints[adaptedpoints$level3==1,], aes(y=lat, colour="Level 3"), size=2,
             alpha=0.5, shape=19) + 
  coord_map("stereographic") +
  xlab("Longitude") + ylab("Latitude") + labs(title="Gas Stations w/Chargers") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom",legend.justification="right") + 
  scale_color_manual(name="Charger Type", values=colors)
dev.off()
setwd("G:/My Drive/EV Charging/Data/Cleaned Data")

############################################################################################################
### Identifying what TRACTS have the most adapted stations and finding the number of stations (adapted or
### unadapted) in each tract in the data
############################################################################################################
## Creating dataset with ONE obs per station (purpose is simply to count the number of adapted stations)
singlestations <- gas_complete %>% group_by(facility_id) %>% filter(row_number(tank_id)==1)

## Number of stations by tract (adapted or unadapted)
stations_tract <- singlestations %>% group_by(tractgeoid2010) %>% summarise(N_stations=n())
View(stations_tract)
summary(stations_tract$N_stations)
hist(stations_tract$N_stations)

## Number of adapted stations by tract
adaptedstations_tract <- singlestations[!is.na(singlestations$id),] %>% 
  group_by(tractgeoid2010) %>% summarise(N_adapted=n())
View(adaptedstations_tract)
summary(adaptedstations_tract$N_adapted) # Any tract has at most TWO adapted station in it! In fact,
                                         # only two tracts have >1 (2) adapted stations in them. All
                                         # other stations have just one.
hist(adaptedstations_tract$N_adapted)

############################################################################################################
### Tract choropleths
############################################################################################################
## Choropleths of all stations (adapted and unadapted)
stations_tract2 <- stations_tract
colnames(stations_tract2) <- c("region","value")
stations_tract2$region <- as.numeric(stations_tract2$region)

setwd("G:/My Drive/EV Charging/Images")
pdf(file="all stations state tract choropleths.pdf")
for (i in 1:length(unique(gas_complete$state))) {
  state <- tolower(unique(gas_complete$state)[i])
  if (state!="washington dc") {
    print(tract_choropleth(stations_tract2, state=state, title=paste0(str_to_title(state),
                                                                      " - Number of Filling Stations by Census Tract"),
                           num_colors=1) + scale_fill_gradientn(name="# of stations", colours=viridis(30, option="C"), 
                                                                na.value="white"))
  }
}
dev.off()

# County-zoomed choropleths of all stations (adapted and unadapted) for places like Los Angeles County, CA and 
# Miami-Dade County, FL
alabama_majorcounties <- c(1073,1089,1101)
colorado_majorcounties <- c(8031,8041,8069,8013)
florida_majorcounties <- c(12099,12011,12086,12057,12095,12117,12097,12031,12073)
idaho_majorcounties <- c(16001)
illinois_majorcounties <- c(17031)
kentucky_majorcounties <- c(21067,21111,21015)
ohio_majorcounties <- c(39061,39035,39049)
rhodeisland_majorcounties <- c(44007)
tennessee_majorcounties <- c(47157,47037,47093)
vermont_majorcounties <- c(50007)
virginia_majorcounties <- c(51013,51087,51041)
westvirginia_majorcounties <- c()
wisconsin_majorcounties <- c(55079,55009)
delaware_majorcounties <- c()
maine_majorcounties <- c()
california_majorcounties <- c(6037,6059,6073,6085,6087,6081,6075,6041,6097,6055,6095,6013,6001,6065)
statemajorcounties <- list(alabama_majorcounties,colorado_majorcounties,florida_majorcounties,idaho_majorcounties,
                      illinois_majorcounties,kentucky_majorcounties,ohio_majorcounties,rhodeisland_majorcounties,
                      tennessee_majorcounties,vermont_majorcounties,virginia_majorcounties,
                      wisconsin_majorcounties,maine_majorcounties,california_majorcounties)
countynames <- as.data.frame(list(c(1073,1089,1101,8031,8041,8069,8013,12099,12011,12086,12057,12095,12117,12097,12031,
                               12073,16001,17031,21067,21111,21015,39061,39035,39049,44007,47157,47037,47093,
                               50007,51013,51087,51041,NA,55079,55009,NA,NA,6037,6059,6073,6085,6087,6081,
                               6075,6041,6097,6055,6095,6013,6001,6065),c("Jefferson","Madison","Montgomery",
                                                                          "Denver","El Paso","Larimer","Brown",
                                                                          "Palm Beach","Broward","Miami-Dade",
                                                                          "Hillsborough","Orange","Seminole",
                                                                          "Osceola","Duval","Leon","Ada","Cook",
                                                                          "Fayette","Jefferson","Boone","Hamilton",
                                                                          "Cuyahoga","Franklin","Providence",
                                                                          "Shelby","Davidson","Knox","Chittenden",
                                                                          "Arlington","Henrico","Chesterfield",NA,
                                                                          "Milwaukee","Brown",NA,NA,"Los Angeles",
                                                                          "Orange","San Diego","Santa Clara",
                                                                          "Santa Cruz","San Mateo","San Francisco",
                                                                          "Marin","Sonoma","Napa","Solano",
                                                                          "Contra Costa","Alameda","Riverside")))
colnames(countynames) <- c("countyfips","countyname")
rm(alabama_majorcounties,colorado_majorcounties,florida_majorcounties,idaho_majorcounties,
   illinois_majorcounties,kentucky_majorcounties,ohio_majorcounties,rhodeisland_majorcounties,
   tennessee_majorcounties,vermont_majorcounties,virginia_majorcounties,
   westvirginia_majorcounties,wisconsin_majorcounties,delaware_majorcounties,
   maine_majorcounties,california_majorcounties)

setwd("G:/My Drive/EV Charging/Images")
pdf(file="all stations major county tract choropleths.pdf")
for (i in 1:(length(unique(gas_complete$State.x)))) {
  state <- tolower(unique(gas_complete$State.x)[i])
  counties <- statemajorcounties[[i]]
  if (state!="West Virginia"&state!="Delaware"&state!="Maine") {
    for (j in 1:length(counties)) {
      county <- counties[j]
      countynm <- countynames[countynames$countyfips==county,]$countyname
      print(tract_choropleth(stations_tract2, state=state, title=paste0(countynm,"-",str_to_title(state),
                                                                " : Number of Filling Stations by Census Tract"), 
                 num_colors=1, county_zoom=county) + scale_fill_gradientn(name="# of stations", 
                                                                        colours=viridis(30, option="C"), 
                                                                        na.value="white"))
    }
  }
}
dev.off()
rm(state,counties,county,countynm)

## Choropleths of adapted stations - no sense in making these since EVERY tract that has an adapted station only
## has ONE such station.

############################################################################################################
### Identifying what COUNTIES have the most adapted stations and finding the number of stations (adapted or
### unadapted) in each tract in the data
############################################################################################################
## Number of stations by county (adapted or unadapted)
stations_county <- singlestations %>% group_by(countyfips) %>% summarise(N_stations=n())
View(stations_county)
summary(stations_county$N_stations)
hist(stations_county$N_stations)

## Number of adapted stations by county
adaptedstations_county <- singlestations[!is.na(singlestations$id),] %>% group_by(countyfips) %>% 
  summarise(N_adapted=n())
View(adaptedstations_county)
summary(adaptedstations_county$N_adapted)
hist(adaptedstations_county$N_adapted)