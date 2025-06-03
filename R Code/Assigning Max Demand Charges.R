### PURPOSE: Add utility-level demand charge data to analysis data (really just adding each utility's 
### maximum demand charge since identifying the most pertinent demand charge rate for stations is
### too demanding at this time; would require text mining of some sort to identify, say, demand charge
### rates that apply to commercial firms like gas stations)
### LAST EDITED: 1/29/2025
### EDITED FROM: Laptop

# Note: Each utility's maximum demand charge rate is not a perfect measure of the demand charges that
# gas stations face (since the maximum demand charge rate is not necessarily the rate that gas stations
# face; what gas stations actually face is probably the rate(s) specific to commercial firms like
# gas stations) but it is the best option available right now (short of text-mining the raw data or
# something of the sort).

library(dplyr)
library(tidyr)
library(readxl)

#######################################################################################################
### Setting the working directory and loading the raw data
#######################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/NREL")
chargerates <- read_xlsx("demand charge rates.xlsx", sheet = "Data", col_names = TRUE)
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
chargerates <- chargerates %>%
  rename("utility_name" = "Utility Name", 
         "utility_id" = "Utility ID (EIA)", 
         "tariff_name" = "Tariff Name",
         "urdb_id" = "URDB ID", 
         "urdb_url" = "URDB URL", 
         "sector" = "Sector", 
         "tariff_desc" = "Tariff Description",
         "tariff_source" = "Tariff Source", 
         "max_demand" = "Maximum allowable demand", 
         "min_demand" = "Minimum allowable demand", 
         "max_kwh_month" = "Maximum allowable kWh per month",
         "min_kwh_month" = "Minimum allowable kWh per month",
         "voltage_cat" = "Voltage Category",
         "phase_wiring" = "Phase wiring", 
         "max_demand_charge" = "Maximum Demand Charge ($/kW)")

View(chargerates %>%
       group_by(utility_id) %>%
       mutate(
         onlyhaszerocharge=dplyr::if_else(any(max_demand_charge!=0)==FALSE, 1, 0)
       ) %>%
       ungroup() %>%
       filter(onlyhaszerocharge==1) %>%
       group_by(utility_id) %>%
       slice_head(n = 1)
) 
  # 581 utilities (~22% of all utilities in the data) ONLY have demand charge(s) of zero; these are true
  # zeros that will be kept in the data in the next filter in the next subsection.

######################################################################################################
### Identifying the maximum demand charge for each utility and adding it to the analysis data
######################################################################################################
## Calculating each utility's average demand charge across all of their tariffs (not all utilities have >1
## tariff but many do, necessitating an average)
maxcharges <- chargerates %>% 
  group_by(utility_id) %>% 
  filter(max_demand_charge>=0) %>% # Demand charges equal to zero appear frequently; ~22% of utilities in
                                   # the data have ONLY a zero demand charge. So will keep observations
                                   # w/zero for max demand charge since true zeros actually appear.
  summarise(maxcharge = mean(max_demand_charge, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(utility_id = as.character(utility_id))

## For each station in analysis, seeing which of their matched utilities has data on max_demand_charge
## in maxcharges; this is being done to systematically assign as many stations a max demand charge as possible, 
## since using only stations' first match (utility_id1) in the past has left ~1k observations in analysis
## w/NAs for maxcharge
# Identifying the first matched utility that has max demand charge data for each station
check <- long_singlestation_util %>% 
  select(facility_id, utility_id) %>% 
  group_by(facility_id) %>% 
  mutate(hasdata = if_else(utility_id %in% maxcharges$utility_id & !is.na(utility_id), 1, 0)) %>% 
  filter(hasdata==1) %>% 
  slice_head(n = 1) %>%
  ungroup()
# Merging in each station's utility_id that has max demand charge data; any stations left with NA for
# utility_id truly do not have any demand charge data (there should be 35645-35511=134 such stations,
# meaning that 134 stations will be left without max demand charge data, which is an improvement over 
# the number of stations that were left without demand charge data when I used only stations' first
# utility match 'utility_id1' to merge in max demand charge data)
# Also dropping now unnecessary utility_idX and utility_nameX variables 1-9
analysis <- left_join(analysis, check[c("facility_id", "utility_id")], by = "facility_id")
analysis <- analysis %>% 
  select(!c(
    "utility_id1",
    "utility_id2",
    "utility_id3",
    "utility_id4",
    "utility_id5",
    "utility_id6",
    "utility_id7",
    "utility_id8",
    "utility_id9",
    "utility_id1",
    "utility_name1",
    "utility_name2",
    "utility_name3",
    "utility_name4",
    "utility_name5",
    "utility_name6",
    "utility_name7",
    "utility_name8",
    "utility_name9"))
analysis <- as.data.frame(analysis)
# Merging in max demand charge data
analysis <- left_join(analysis, maxcharges, by = "utility_id")
# Setting maxcharge=NA for stations that have NA for utility_id
analysis$maxcharge <- ifelse(is.na(analysis$utility_id), NA, analysis$maxcharge)
# Reordering variables
analysis <- analysis %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date,
         n_evs, maxcharge, mean_hhinc, tractpop, countypop, everything())

# Some stations don't get assigned a max demand charge because their 1st assigned utility isn't in 
# the NREL demand charge data; no adapted stations don't get assigned a demand charge because their
# assigned utility isn't in the NREL demand charge data.

stations_na_charge <- unique(analysis[is.na(analysis$maxcharge),]$facility_id) # 134 stations w/o demand
                                                                               # charge data as expected
adaptedstations <- unique(analysis[analysis$adapted==1,]$facility_id) # 305 adapted stations
length(which(adaptedstations%in%stations_na_charge)) # 0 adapted stations have NA demand charge data, good

## Saving the analysis data
analysis <- as.data.frame(analysis)
rownames(analysis) <- 1:nrow(analysis)
write.csv(analysis, file="analysis.csv", row.names=FALSE)



analysis <- read.csv("analysis.csv")

nonhighway <- analysis %>%
  filter(disthwy>750) %>%
  group_by(facility_id) %>%
  filter(row_number()==1) %>%
  group_by(utility_id) %>%
  summarise(n_stations = n_distinct(facility_id)) %>%
  ungroup()
summary(nonhighway$n_stations)
  # Mean: 49.05 stations per utility service territory

nonhighway <- analysis %>%
  filter(disthwy>750) %>%
  group_by(facility_id) %>%
  filter(row_number()==1) %>%
  ungroup()
length(unique(nonhighway$utility_id)) # 567 utilities

highway <- analysis %>%
  filter(disthwy<=750) %>%
  group_by(facility_id) %>%
  filter(row_number()==1) %>%
  group_by(utility_id) %>%
  summarise(n_stations = n_distinct(facility_id)) %>%
  ungroup()
summary(highway$n_stations)
  # Mean: 27.58 stations per utility service territory

highway <- analysis %>%
  filter(disthwy<=750) %>%
  group_by(facility_id) %>%
  filter(row_number()==1) %>%
  ungroup()
length(unique(highway$utility_id)) # 284 utilities
