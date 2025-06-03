### PURPOSE: Merge income, population, and EV registration data into analysis data
### LAST EDITED: 1/29/2025
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
library(readxl)
library(choroplethr)

################################################################################
### Setting the initial working directory and loading the data
################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
analysis <- read.csv("analysis.csv")
mean_inc <- read.csv("5year_mean_inc.csv")
evregs <- read.csv("evregs.csv")
tractpopdata <- read.csv("tractpopdata.csv")
countypopdata <- read.csv("countypopdata.csv")

################################################################################
### Merging income data into analysis data
################################################################################
meaninc2010 <- mean_inc[mean_inc$year<2020,]
meaninc2010 <- meaninc2010[c("year","tractgeoid2010","mean_hhinc")]
analysis <- left_join(analysis, meaninc2010, by=c("year","tractgeoid2010"))

meaninc2020 <- mean_inc[mean_inc$year>=2020,]
meaninc2020 <- meaninc2020[c("year","tractgeoid2020","mean_hhinc")]
analysis <- left_join(analysis, meaninc2020, by=c("year","tractgeoid2020"))
# Replacing NAs and keeping only one mean income variable (instead of having separate variables for pre- and
# post-2020)
analysis$mean_hhinc.x <- ifelse(analysis$year>=2020 & is.na(analysis$mean_hhinc.x), analysis$mean_hhinc.y, 
                                   analysis$mean_hhinc.x)
analysis <- analysis %>%
  select(!(mean_hhinc.y)) %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date, 
         "mean_hhinc"="mean_hhinc.x", everything())

################################################################################
### Merging population data into analysis data
################################################################################
# NOTE: Population is at the tract AND COUNTY level because it will be used to create per-capita number of 
# EVs and the EV data (currently) is at the county level.

## Tract-level population
# 2010 Census data
tractpopdata2010 <- tractpopdata %>%
  rename("tractpop"="pop") %>%
  filter(year<2020) %>%
  select(year, tractgeoid2010, tractpop)
analysis <- left_join(analysis, tractpopdata2010, by=c("year", "tractgeoid2010"))
# 2020 Census data
tractpopdata2020 <- tractpopdata %>%
  rename("tractpop"="pop") %>%
  filter(year>=2020) %>%
  select(year, tractgeoid2020, tractpop)
analysis <- left_join(analysis, tractpopdata2020, by=c("year", "tractgeoid2020"))
# Replacing NAs and keeping only one tract-level population variable (instead of having separate variables for 
# pre- and post-2020)
analysis$tractpop.x <- ifelse(analysis$year>=2020 & is.na(analysis$tractpop.x), analysis$tractpop.y, 
                                analysis$tractpop.x)
analysis <- analysis %>%
  select(!(tractpop.y)) %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date, 
         "tractpop"="tractpop.x", everything())
## County-level population
countypopdata <- countypopdata %>%
  rename("countypop"="pop")
analysis <- left_join(analysis, countypopdata, by=c("year","countyfips"))
## Reordering variables
analysis <- analysis %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date,
         mean_hhinc, tractpop, countypop, everything())

################################################################################
### Merging EV registration data into analysis data
################################################################################
## Aggregating monthly EV counts to annual level (really only applies to states that don't already report
## annual data) by averaging the number of EVs across all available months in each data year
  # NOTE: Not summing across months because each month is a SNAPSHOT.
annual_evregs <- evregs %>%
  group_by(state, countyfips, year) %>%
  summarise(n_evs=mean(n_evs, na.rm=TRUE))

## Merging in annual EV count data at the county level
# Creating abbreviated state name variable to replace current full-name state variable in 'analysis'
analysis <- analysis %>%
  mutate(state=case_when(
    state=="Alabama" ~ "AL",
    state=="Alaska" ~ "AK",
    state=="Arizona" ~ "AZ",
    state=="Arkansas" ~ "AR",
    state=="California" ~ "CA",
    state=="Colorado" ~ "CO",
    state=="Connecticut" ~ "CT",
    state=="Delaware" ~ "DE",
    state=="Washington DC" ~ "DC",
    state=="Florida" ~ "FL",
    state=="Georgia" ~ "GA",
    state=="Hawaii" ~ "HI",
    state=="Idaho" ~ "ID",
    state=="Illinois" ~ "IL",
    state=="Indiana" ~ "IN",
    state=="Iowa" ~ "IA",
    state=="Kansas" ~ "KS",
    state=="Kentucky" ~ "KY",
    state=="Louisiana" ~ "LA",
    state=="Maine" ~ "ME",
    state=="Maryland" ~ "MD",
    state=="Massachusetts" ~ "MA",
    state=="Michigan" ~ "MI",
    state=="Minnesota" ~ "MN",
    state=="Mississippi" ~ "MS",
    state=="Missouri" ~ "MO",
    state=="Montana" ~ "MT",
    state=="Nebraska" ~ "NE",
    state=="Nevada" ~ "NV",
    state=="New Hampshire" ~ "NH",
    state=="New Jersey" ~ "NJ",
    state=="New Mexico" ~ "NM",
    state=="New York" ~ "NY",
    state=="North Carolina" ~ "NC",
    state=="North Dakota" ~ "ND",
    state=="Ohio" ~ "OH",
    state=="Oklahoma" ~ "OK",
    state=="Oregon" ~ "OR",
    state=="Pennsylvania" ~ "PA",
    state=="Rhode Island" ~ "RI",
    state=="South Carolina" ~ "SC",
    state=="South Dakota" ~ "SD",
    state=="Tennessee" ~ "TN",
    state=="Texas" ~ "TX",
    state=="Utah" ~ "UT",
    state=="Vermont" ~ "VT",
    state=="Virginia" ~ "VA",
    state=="Washington" ~ "WA",
    state=="West Virginia" ~ "WV",
    state=="Wisconsin" ~ "WI",
    state=="Wyoming" ~ "WY"
  )
  )
# Merging the annual EV count data
analysis <- left_join(analysis, annual_evregs, by=c("state", "countyfips", "year"))
analysis <- analysis %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date, 
         n_evs, mean_hhinc, tractpop, countypop, everything())

## Dropping states for which we do not (currently) have EV registration data for (since otherwise
## all of the observations for stations in such states appear as false NAs; SC could very well have
## EVs but we do not have data on SC EV registrations, so all SC stations' observations will have
## NAs that are not actually missing)
analysis <- analysis %>%
  filter(state%in%unique(evregs$state))

## Assigning 0s for n_evs for stations that have missing (NA) n_evs
  # NOTE: This is done because after only keeping data on stations in states with available EV registration
  # data, any remaining stations w/NA for n_evs are not in counties/ZIPs that have EVs (at least in
  # certain periods) in the Atlas EV data or CA CEC data and thus should have 0 for n_evs, BUT ONLY
  # IN YEARS PRECEDING AND INCLUDING EACH STATE'S LATEST YEAR OF DATA. So for example, all NAs for n_evs
  # in MT should be converted to 0s before and including 2022 (which is MT's latest year of data), but
  # should be LEFT AS TRUE NAs in 2023. dplyr line below does this, and any remaining NAs for n_evs are 
  # true NAs
# Adding each state's latest data year as a temporary variable and then populating n_evs with NAs according
# to the details in the commented paragraph above
analysis <- analysis %>%
  group_by(state) %>%
  mutate(maxyear=max(year)) %>%
  mutate(n_evs=dplyr::if_else(is.na(n_evs) & year<=maxyear, 0, n_evs)) %>%
  ungroup() %>%
  select(!(maxyear))

################################################################################
### Saving the analysis dataset
################################################################################
write.csv(analysis, file="analysis.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())