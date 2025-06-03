### PURPOSE: Format the analysis data for survival/duration modeling
### LAST EDITED: 1/29/2025
### EDITED FROM: Laptop

library(survminer)
library(survival)
library(geosphere)
library(lubridate)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(lubridate)

# NOTE: ALL "Tesla Destination" CHARGERS ARE LEVEL 2 CHARGERS! So for stations with Tesla Destination
# chargers, will use the lone Level 2 start date (l2_date in gas_complete).

# NOTE: The 'adapted' variable in the analysis data is NOT a fixed dummy denoting that a station
# has adapted at some point. It is only equal to one AFTER a station's adaptation date (i.e., in all
# periods after the period in which a station adapts and including the adaptation period too).

setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
analysis <- read.csv("analysis.csv")
gas_complete <- read.csv("gas_complete.csv")

## Quickly seeing how many stations have adapted
adaptedpoints <- unique(gas_complete[!is.na(gas_complete$id),][c("facility_id",
                                                                         "lat","lon","level1",
                                                                         "level2","level3")])
adaptedpoints <- adaptedpoints %>% 
  group_by(facility_id) %>% 
  mutate(n=n()) %>% 
  ungroup()
length(unique(adaptedpoints$lat)) # There are 525 adapted stations

######################################################################################################
### Preparing the analysis data for survival/duration analysis (creating variables)
######################################################################################################
## Adding dummy variables for charger level to the analysis data (one for a station having Level 2
## charging and one for a station having Level 3 charging; stations can have both or just one)
# Creating a dataframe linking each station (facility_id) to what level(s) of charging they installed
station_charge_cats <- gas_complete %>% 
  group_by(facility_id) %>% 
  slice_head(n=1) %>%
  ungroup() %>%
  select(facility_id, level1, level2, level3) %>%
  mutate(
    level1=as.numeric(level1), 
    level2=as.numeric(level2), 
    level3=as.numeric(level3)
    ) %>%
  mutate(
    level1=dplyr::if_else(is.na(level1), 0, level1),
    level2=dplyr::if_else(is.na(level2), 0, level2),
    level3=dplyr::if_else(is.na(level3), 0, level3)
    )
# Adding charger level dummies to the analysis data
analysis <- left_join(analysis, station_charge_cats, by="facility_id")

## Adding dummy variables for charger network to the analysis data
# Creating a dataframe linking each station to their EV charging network (if they've adapted)
station_charge_networks <- gas_complete %>%
  group_by(facility_id) %>%
  slice_head(n=1) %>%
  select(facility_id, ev_network)
# Creating dummies for each EV network
station_charge_networks <- fastDummies::dummy_cols(station_charge_networks, 
                                                   select_columns="ev_network", ignore_na=TRUE,
                                                   remove_selected_columns=FALSE)
station_charge_networks <- station_charge_networks %>%
  rename("7charge"="ev_network_7CHARGE", "ampup"="ev_network_AMPUP", "blink"="ev_network_Blink Network",
         "chargelab"="ev_network_CHARGELAB", "chargepoint"="ev_network_ChargePoint Network",
         "chargeup"="ev_network_CHARGEUP", "circlek"="ev_network_CIRCLE_K", 
         "electrifyamerica"="ev_network_Electrify America", "evconnect"="ev_network_EV Connect",
         "evcs"="ev_network_EVCS", "evgateway"="ev_network_EVGATEWAY", "evgo"="ev_network_eVgo Network",
         "evrange"="ev_network_EVRANGE", "fcn"="ev_network_FCN", "flo"="ev_network_FLO",
         "fplev"="ev_network_FPLEV", "graviti"="ev_network_GRAVITI_ENERGY", 
         "livingston"="ev_network_LIVINGSTON", "nonnetworked"="ev_network_Non-Networked",
         "rede"="ev_network_RED_E", "rivianadventure"="ev_network_RIVIAN_ADVENTURE",
         "rivianwaypoints"="ev_network_RIVIAN_WAYPOINTS", "shellrecharge"="ev_network_SHELL_RECHARGE",
         "tesla"="ev_network_Tesla", "tesladestination"="ev_network_Tesla Destination", 
         "volta"="ev_network_Volta", "zefnet"="ev_network_ZEFNET")
# Adding charger network dummies to the analysis data
analysis <- left_join(analysis, station_charge_networks, by="facility_id")

## Adding a Level 2 charger start date and Level 3 start dates to the analysis data
# For stations that have installed both Level 2 and Level 3, will use the Level 2 start date because it
# precedes all Level 3 start dates (i.e., Level 2 charging became available before all types of Level 3
# charging)
dates <- gas_complete[!is.na(gas_complete$id),]
analysis$lvl2date <- ymd(dates$l2_date[1])
analysis$lvl3date_chademo <- ymd(dates$l3ntchademo_date[1])
analysis$lvl3date_ccs <- ymd(dates$l3ntccs_date[1])
analysis$lvl3date_tesla <- ymd(dates$l3tesla_date[1])
rm(dates)

## Creating start date variable 'typestartdate' FOR ADAPTED STATIONS that varies with the level/type of 
## charging that adapted stations chose to install (i.e., different across Level 3 CHAdeMO, CCS, 
## and Tesla types)
# Note: Only one station in the analysis data has Level 1 charging (facility WI412391).
# Note: Using the CCS intro date as the sole non-Tesla Level 3 date (only 3 months separate CCS' and
# CHAdeMO's start dates and CCS/J1772 are more prevalent in the data)
# NOTE: ALL "Tesla Destination" CHARGERS ARE LEVEL 2 CHARGERS! So for stations with Tesla Destination
# chargers, will use the lone Level 2 start date (l2_date in gas_complete).

# Creating time-invariant dummy variable flagging stations that end up adapting
adaptedstations <- gas_complete %>%
  filter(!is.na(id)) %>%
  group_by(facility_id) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(facility_id)
analysis$willadapt <- ifelse(analysis$facility_id%in%adaptedstations$facility_id, 1, 0)
analysis <- analysis %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, willadapt, adapted, 
         adapt_date, everything())
# Creating 'typestartdate'
analysis <- analysis %>%
  mutate(typestartdate=case_when(
    willadapt==1 & level1==0 & level2==1 & level3==0 ~ lvl2date[1],
    willadapt==1 & level1==0 & level2==1 & level3==1 ~ lvl2date[1],
    willadapt==1 & level1==0 & level2==0 & level3==1 & tesla==1 ~ lvl3date_tesla[1],
    willadapt==1 & tesladestination==1 & !is.na(tesladestination) ~ lvl2date[1],
    willadapt==1 & level1==0 & level2==0 & level3==1 & tesla==0 & tesladestination==0 ~ lvl3date_ccs[1],
    willadapt==1 & level1==1 ~ ymd("2010-01-01"),
    willadapt==0 ~ lvl2date[1]
  ),
  allstartdate=lvl2date[1] # Start date variable for all stations that does not vary with charger level/type.
  )

######################################################################################################
### Shaping the analysis data for use in survival models w/time-dependent covariates and saving
######################################################################################################
## Ending a station's panel after it adapts (installs a charger; this means not keeping post-adaptation 
## periods for stations that adapt)
## Accomplishing this by first keeping all observations sequentially over time UNTIL the first date in which
## adapted=1 (i.e., the month-year in which a station installs its first charger aka adapts).
analysis <- analysis %>% 
  group_by(facility_id) %>% 
  filter(dplyr::cumall(dplyr::lag(adapted, n=1, default=0)<1)) %>%
  ungroup()
check <- analysis %>% 
  group_by(facility_id) %>% 
  summarise(n_adapts=sum(adapted)) %>%
  ungroup()
any(check$n_adapts>1) # Should be FALSE and it is, so all good
rm(check)
# 'n_adapts' in the summarise call above should be at most 1 because I'm only keeping periods up to
# and including the first period in which adapted=1. Hence, all stations will have at most one period
# in their panel with adapted=1, meaning that the sum of adapted for each station is at most 1 (a 
# bunch of 0s plus a single 1).

# NOTE: For the creation of time variables that follows, taking the difference between type/all start
# date and the calendar (sample period) date because each station's calendar (sample period) date
# ends when they adapt (i.e., in the first period that adapted==1).

## Creating a time variable (measured in months) whose start date varies by charger level/type
survanalysis_type <- analysis
survanalysis_type$typetime <- NA
survanalysis_type$typetime <- interval(start=survanalysis_type$typestartdate, 
                                       end=survanalysis_type$date) %/% months(1)
# Dropping rows w/negative typetime (this happens for Level 3 stations w/type start date after 2010-01-01)
survanalysis_type <- survanalysis_type[survanalysis_type$typetime>=0,]
# Adjusting typetime for stations that opened after the earliest charger availability date (Level 2
# charger, which is Jan 2010)
survanalysis_type <- survanalysis_type %>% 
  group_by(facility_id) %>% 
  mutate(typetime=(typetime-min(typetime))) %>% 
  filter(!is.na(typetime)) %>%
  ungroup()
check <- survanalysis_type %>% 
  group_by(facility_id) %>% 
  summarise(min_period=min(typetime)) %>%
  ungroup()
any(check$min_period!=0) # Should be FALSE and it is, so all good w/typetime
rm(check)

## Creating a time variable (measured in months) whose start date is the same (Level 2 date) for all
## stations regardless of the level/type of charger they installed
survanalysis_all <- analysis
survanalysis_alltime <- NA
survanalysis_all$alltime <- interval(start=survanalysis_all$allstartdate, 
                                     end=survanalysis_all$date) %/% months(1)
# Dropping rows w/negative alltime (which happens if the start date, which is for Level 2 chargers here, is 
# AFTER the date, which doesn't happen here because the Level 2 start date is 2010-01-01 and the earliest date
# in the data is 2010-01-01)
survanalysis_all <- survanalysis_all[survanalysis_all$alltime>=0,] # Doesn't do anything here because
                                                                   # no station has a data point BEFORE
                                                                   # the Level 2 start date of 2010-01-01.
# Adjusting alltime for stations that opened after the earliest charger availability date (which would start
# their "clock" at some non-zero value instead of zero like we need)
survanalysis_all <- survanalysis_all %>% 
  group_by(facility_id) %>% 
  mutate(alltime=(alltime-min(alltime))) %>%
  ungroup()
check <- survanalysis_all %>% 
  group_by(facility_id) %>% 
  summarise(min_period=min(alltime)) %>%
  ungroup()
any(check$min_period!=0) # Should be FALSE and it is, so all good w/alltime
rm(check)

## Creating single survival time variable (equal to the max of typetime/alltime; this is each 
## station's total survival time or time to adaptation, which is right-censored for stations that 
## have not yet adapted)
survanalysis_type <- survanalysis_type %>% 
  group_by(facility_id) %>% 
  mutate(typesurvtime=max(typetime)) %>%
  ungroup()
survanalysis_all <- survanalysis_all %>% 
  group_by(facility_id) %>% 
  mutate(allsurvtime=max(alltime)) %>%
  ungroup()

## Creating survival time interval variables for running survival/duration models w/time-varying 
## independent variables
# Type-varying start dates
survanalysis_type <- survanalysis_type %>% 
  group_by(facility_id) %>% 
  mutate(tstart=typetime, tstop=typetime+1) %>%
  ungroup() %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, willadapt, adapted, 
         adapt_date, typestartdate, typetime, typesurvtime, tstart, tstop, everything())
# Single common start date
survanalysis_all <- survanalysis_all %>% 
  group_by(facility_id) %>% 
  mutate(tstart=alltime, tstop=alltime+1) %>%
  ungroup() %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, willadapt, adapted, 
         adapt_date, allstartdate, alltime, allsurvtime, tstart, tstop, everything())

## Making a # of EVs per capita variable (using county-level population because n_evs is at the county-level)
survanalysis_all$evs_percap <- survanalysis_all$n_evs/survanalysis_all$countypop
survanalysis_all <- survanalysis_all %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, willadapt, adapted, 
         adapt_date, allstartdate, alltime, allsurvtime, tstart, tstop, n_evs, evs_percap, everything())

################################################################################
### Saving final datasets
################################################################################
## Saving final version of analysis data (not in survival form though, that's what survanalysis_X is for)
write.csv(analysis, file="analysis.csv", row.names=FALSE)

## Saving survival analysis versions of the analysis data
# By charger type/level
#write.csv(survanalysis_type, file="survanalysis_type.csv", row.names=FALSE)
#save(survanalysis_type, file="survanalysis_type.RData")

# Single start date for all stations
write.csv(survanalysis_all, file="survanalysis_all.csv", row.names=FALSE)

## Number of stations per tract summary stats
trash <- gas_complete %>% 
  group_by(tractgeoid2010) %>% 
  summarise(n=n()) %>%
  ungroup()
summary(trash$n)