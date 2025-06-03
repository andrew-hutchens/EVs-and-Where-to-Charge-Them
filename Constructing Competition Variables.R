### PURPOSE: Create variables for cumulative counts of competitors (other stations) and participants (non-station
### firms that installed chargers) at the region-month level (currently TRACT-month level)
### LAST EDITED: 1/29/2025
### EDITED FROM: Laptop

### Note: Current working market definition is tract-month.
### Note: No longer calculating cumulative capacity for each station. Station capacity changes are rare and
### do not occur month-to-month often if ever, such that there is little empirical benefit to using cumulative
### capacity. Will just calculate each station's total capacity across its open tanks as of the latest UST
### data pull.

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
library(lubridate)

###########################################################################################################
### Setting the initial working directory and loading the data
###########################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
gas_complete <- read.csv("gas_complete.csv")
chargerdata <- read.csv("chargerdata.csv")
gasstations <- read.csv("gasstations.csv")

### NOTE: Some facilities' tanks were installed under one ID and then removed but then another tank was
### later installed under the removed tank's ID. For example, facility 4010002's Tank 1 was originally
### installed on 9/1/1961 and then removed on 9/10/1997, only for ANOTHER Tank 1 to be installed on 10/1/2008
### (it remains installed and open, i.e., tank_status==Open).

###########################################################################################################
### Creating station opening date variable using oldest tank's installation date
###########################################################################################################
## Creating station opening date and adding it to the main merged data 'gas_complete' (ignoring erroneous
## entries with 1/1/1900 as their tank installation date; the first station was built in 1905)
# Modifying gas_complete's 'install_date' variable to date format to correctly capture dates
gas_complete <- gas_complete %>%
  mutate(install_date=mdy(install_date)) %>%
  group_by(facility_id) %>%
  filter(install_date>"1905-01-01") %>%
  mutate(facility_open_date=min(install_date)) %>%
  ungroup() %>%
  mutate(
    facility_open_year=as.numeric(substr(facility_open_date, 1, 4)),
    facility_open_month=as.numeric(substr(facility_open_date, 6, 7)),
    facility_open_day=as.numeric(substr(facility_open_date, 9, 10))
  )
  # NOTE: facility_open_date, facility_open_year, and facility_open_month refer to the STATION's opening date,
  # not charger opening date.

## Brief summary statistics for facility opening date
min(gas_complete[!is.na(gas_complete$facility_open_date),]$facility_open_date) # 1/1/1907
max(gas_complete[!is.na(gas_complete$facility_open_date),]$facility_open_date) # 11/5/2020

###########################################################################################################
### Reshaping main merged data into new dataframe in long panel form
###########################################################################################################
## Brief summary statistics for charger open dates to see if the starting month for the panel should depend
## on chargers' opening dates
min(chargerdata[chargerdata$open_date!="" & !is.na(chargerdata$open_date),]$open_date) # 8/30/1995
max(chargerdata[chargerdata$open_date!="" & !is.na(chargerdata$open_date),]$open_date) # 12/18/2023
View(gas_complete[!is.na(gas_complete$id),][c("facility_id","name","id","open_date")] %>% 
       group_by(facility_id) %>% summarise(n_distinct(open_date)))
  # Some stations have >1 unique open_date due to having >1 charger location (charger ID or 'id') associated
  # with them in the chargerdata. That is, >1 charger location (denoted by the 'id' variable) had the same
  # station assigned to them as their nearest neighbor and associated adapted station. For example, facility
  # both chargers 150281 and 182774 (which are confirmed to be located at the same place, so no problem there)
  # were linked to station CA10138405 (as their nearest station neighbor and the adapted station that they are
  # are located at).
  # This is not a problem: will use the station's first or EARLIEST open_date as its adaptation date!

# Note: Will use the earliest charger open date (Aug 1995) as the starting date for the panel created below.
# Note: Will use latest charger open date (Dec 2023) as the end date for the panel created below.

## Creating vector of all possible dates (every month from Aug 1995 to Dec 2023)
allmonths <- seq(from=as.Date("08/01/1995", format="%m/%d/%Y"), to=as.Date("12/01/2023", format="%m/%d/%Y"),
                 by="month")
allmonths <- as.data.frame(allmonths)
colnames(allmonths) <- "date"
allmonths <- data.frame(allmonths$date, as.numeric(substr(allmonths$date,1,4)), 
                        as.numeric(substr(allmonths$date,6,7)))
allmonths <- allmonths %>%
  rename("date"="allmonths.date", "year"="as.numeric.substr.allmonths.date..1..4..", 
         "month"="as.numeric.substr.allmonths.date..6..7..")

## Creating dataframe w/one obs per station
singleobs <- gas_complete %>% 
  group_by(facility_id) %>% 
  slice_head(n=1) %>%
  ungroup() %>%
  select(facility_id, name, address, city, county, state, zip, lat, lon, msa_name2010, csa_id2010,
         tractgeoid2010, tract2010, msa_name2020, csa_id2020, tractgeoid2020, tract2020, countyfips,
         facility_open_date, facility_open_month, facility_open_day, facility_open_year)
singleobs <- as.data.frame(singleobs)

## Replicating each station's single observation across each time period (i.e., each month)
# Replicating each station's single observation as many times as there are months in the sample period
multipledateobs <- singleobs[rep(seq_len(nrow(singleobs)), each=dim(allmonths)[1]),]
# Replicating date (year, month) variables as many times as there are unique stations to merge in date
# variables into multipledateobs
multipleallmonths <- allmonths[rep(seq_len(nrow(allmonths)), times=dim(singleobs)[1]),]
multipledateobs <- cbind(multipledateobs, multipleallmonths)
rownames(multipledateobs) <- 1:nrow(multipledateobs)
multipledateobs$year <- as.numeric(multipledateobs$year)
multipledateobs$month <- as.numeric(multipledateobs$month)
rm(singleobs,multipleallmonths)
# 'multipledateobs' is the initial form of the eventual analysis dataset w/station-month-year as the unit
# of observation.
# Note that 'year' and 'month' in 'multipledateobs' refer to TIME PERIODS, not any sort of install or open
# date for stations or chargers.

###########################################################################################################
### Summarizing station data (total capacity, capacity by substance type, substance type dummies) at the
### month-year level
###########################################################################################################
## Summarizing gas station capacity, specifically summing total capacity across all 
## open tanks as of the last UST data pull (since capacity changes are rare and difficult to
## properly track with incomplete data on tank removals and installations).
totalcapacity <- gasstations %>%
  group_by(facility_id) %>%
  filter(tank_status=="Open") %>%
  summarise(totcapacity=sum(capacity, na.rm=TRUE)) %>%
  ungroup()

## Adding total capacity to 'multipledateobs'
multipledateobs <- left_join(multipledateobs, totalcapacity, by=c("facility_id"))

## Dropping observations for periods (month-years) that precede stations' opening date
multipledateobs <- multipledateobs %>% 
  filter(date>=facility_open_date)

## Dropping stations that have no capacity data at all
nocapfacs <- gasstations %>% 
  group_by(facility_id) %>% 
  summarise(any(!is.na(capacity))) %>%
  ungroup()
colnames(nocapfacs) <- c("facility_id","has_nonmiss_cap")
nocapfacs <- nocapfacs[nocapfacs$has_nonmiss_cap==FALSE,] # 4580 facilities have zero non-NA capacity obs
multipledateobs <- multipledateobs[-which(multipledateobs$facility_id%in%nocapfacs$facility_id),]
length(which(is.na(multipledateobs$totcapacity))) # This should be 0 and it is
rm(nocapfacs)

## Dropping stations w/no installation dates; NOT DOING WHAT'S IN PARENTHESES BELOW ANYMORE
## (Adding back in capacities for stations w/no installation dates at all; specifically inputting whatever
## their lone known capacity is (if it isn't NA; stations w/NA capacity throughout will simply have 
## missing capacity)) <- NOT DOING THIS. DOING SO WOULD ONLY PICK UP ~3 MORE ADAPTED STATIONS AND
## COMPLICATE THE PANEL CONSTRUCTION CODE ABOVE.

# NOTE: You lose several states' data by doing this, including states that have adapted chargers
# like South Carolina. This is a necessary sacrifice in order to construct panel data, but maybe
# a purely cross-sectional analysis can keep stations w/o install dates for their tanks.
# Creating dataframe of stations w/no installation dates
noinstalldates <- gasstations %>% 
  group_by(facility_id) %>% 
  summarise(any(install_date!="")) %>%
  ungroup()
colnames(noinstalldates) <- c("facility_id","has_nonmiss_date")
noinstalldates <- noinstalldates[noinstalldates$has_nonmiss_date==FALSE,]
# Dropping stations w/no installation dates from multipledateobs
length(which(multipledateobs$facility_id%in%noinstalldates$facility_id)) # 0
# EDIT: All of the stations in noinstalldates are ALREADY not in multipledateobs, so no need to
# do anything else; stations w/o install dates are already not in the dataset we're working with moving
# forward (multipledateobs).
rm(noinstalldates)

###########################################################################################################
### Creating analysis dataset for use w/all further modifications from here on
###########################################################################################################
analysis <- multipledateobs

# Note: Because the earliest charger availability date is Jan 2010 (Level 2's availability date),
# trimming the analysis sample period down to Jan 2010 - Dec 2023.
analysis <- analysis %>% 
  filter(date>="2010-01-01") %>%
  select(facility_id, date, year, month, totcapacity, everything())
analysis <- as.data.frame(analysis)
rownames(analysis) <- 1:nrow(analysis)

###########################################################################################################
### Creating number of chargers ("participants") in each tract-month: complete DOE AFDC public chargers data
###########################################################################################################
# Note: 'id' variable in chargerdata uniquely identifies each CHARGER, not the location of chargers. 
# Can uniquely identify charger LOCATIONS by address-ID combos (i.e., street_address-id combos in variable
# name terms).
# ^ Don't need to worry about this since I am just summing total chargers in each market (tract). Thus,
# chargers who have separate obs for each port still only contribute one charger/port per port, the same
# outcome as if all ports were counted in one obs like most observations. For example, if charger
# 7-11-FL has 3 DCFC ports and is recorded in the data 3 times (with one obs per DCFC port), then 
# each row of totchargers and n_dcfast for each obs will have a value of 1 and contribute a total of 3
# to the tract-level count. Equivalently, if charger 7-11-FL has 3 DCFC ports but is recorded in the data
# once then totchargers=3 and n_dcfast=3 and 7-11-FL will again contribute a total of 3 to the
# tract-level count of totchargers and n_dcfast.

# Note: Census tract definitions change in 2020, but will stick with 2010 Census tract definitions for all
# variables except socioeconomic variables (e.g., income). See the README note for more on this choice.

### Constructing cumulative sums of chargers (not charger sites) across all types in each tract-month 
### from Jan 2010-Dec 2023
# Converting allmonths' year and month variables to numeric
allmonths$year <- as.numeric(allmonths$year)
allmonths$month <- as.numeric(allmonths$month)

## Creating dataframe of tracts and dates (sticking with 2010 census tract definitions); need the chargertracts2010
## dataframe because it houses every possible month-year in the sample, whereas total_chargers2010 (created later) can
## only calculate the number of chargers in each tract according to EVSE opening dates)
chargertracts2010 <- as.data.frame(unique(chargerdata$tractgeoid2010))
colnames(chargertracts2010) <- "tractgeoid2010"
# Expanding 'chargertracts' so that each tract has an observation for each month from Aug 1995-Dec 2023
chargertracts2010 <- as.data.frame(chargertracts2010[rep(seq_len(nrow(chargertracts2010)), 
                                                         each=dim(allmonths)[1]),])
colnames(chargertracts2010) <- "tractgeoid2010"
chargertracts2010 <- cbind(chargertracts2010, allmonths)
rownames(chargertracts2010) <- 1:nrow(chargertracts2010)

## Constructing sum of all chargers in a tract (BUT NOT AT GAS STATIONS) at each opening date (or EVSE 
## installation date) and then renaming evse_open_year and evse_open_month to year and month respectively for 
## merging w/chargertracts2010
total_chargers2010 <- chargerdata %>%
  group_by(tractgeoid2010, evse_open_year, evse_open_month) %>%
  filter(!id%in%gas_complete$id) %>% # To not count gas station chargers; stations only counted as competitors
  filter(!is.na(tractgeoid2010)) %>%
  filter(!is.na(evse_open_year) & !is.na(evse_open_month)) %>%
  summarise(totchargers=sum(totchargers, na.rm=TRUE)) %>%
  ungroup() %>%
  rename("year"="evse_open_year", "month"="evse_open_month")
# Note that 6 chargers in 'chargerdata' do not have any opening date data (e.g., year, month, or day), hence
# the filtering of observations w/missing opening year and month above.
length(which(is.na(chargerdata$open_date))) #^

## Merging in 'totchargers' variable (which actually captures how many new chargers were added to a tract 
## at the observation's opening date 'opendate', which 'year' and 'month' are based on) into dataframe of
## tracts and dates 'chargertracts2010'
# Merging in the totchargers data
chargertracts2010 <- left_join(chargertracts2010, total_chargers2010, by=c("tractgeoid2010","year","month"))
# Replacing NAs w/0s since in this dataframe, an NA in a given month-year indicates that no additional
# chargers opened or were installed in that month-year
chargertracts2010$totchargers <- ifelse(is.na(chargertracts2010$totchargers), 0, chargertracts2010$totchargers)
# Creating tract-month-level cumulative total chargers variable
chargertracts2010 <- chargertracts2010 %>% 
  group_by(tractgeoid2010) %>% 
  mutate(cumchargers=cumsum(totchargers)) %>%
  ungroup()
# Removing observations from before Jan 2010 to fit the final sample period of Jan 2010-Dec 2023
chargertracts2010 <- chargertracts2010 %>% 
  filter(date>="2010-01-01")
# Keeping only necessary variables
chargertracts2010 <- chargertracts2010[c("tractgeoid2010","year","month","cumchargers")]
chargertracts2010 <- as.data.frame(chargertracts2010)
rownames(chargertracts2010) <- 1:nrow(chargertracts2010)

rm(total_chargers2010)

## Adding tract-month-level total chargers variable by period (2010 Census and 2020 Census) to analysis data
analysis <- left_join(analysis, chargertracts2010, by=c("tractgeoid2010","year","month"))
analysis$cumchargers <- ifelse(is.na(analysis$cumchargers), 0, analysis$cumchargers)

## Constructing cumulative sums of chargers (not charger sites) by type (Level 1, Level 2, Level 3) in
## each tract-month from Aug 1995-Dec 2023 (IN PROGRESS)

###########################################################################################################
### Creating number of chargers ("participants") in each tract-month: dropping plausibly non-public chargers
### from DOE AFDC data
###########################################################################################################
# Note: Census tract definitions change in 2020, but will stick with 2010 Census tract definitions for all
# variables except socioeconomic variables (e.g., income). See the README note for more on this choice.

### Constructing cumulative sums of chargers (not charger sites) across all types in each tract-month 
### from Jan 2010-Dec 2023, OMITTING CHARGERS THAT ARE PLAUSIBLY NOT ACTUALLY PUBLIC (E.G., AIRPORT PARKING, META HQ'S
### PARKING GARAGE, OTHER MAJOR SINGLE COMPANY HQ'S PARKING GARAGES, ETC.)
## Recreating dataframe of tracts and dates (sticking with 2010 census tract definitions since most data
## is from 2010-2020, i.e., pre-2020 census)
chargertracts2010_trimmed <- as.data.frame(unique(chargerdata$tractgeoid2010))
colnames(chargertracts2010_trimmed) <- "tractgeoid2010"
# Expanding 'chargertracts' so that each tract has an observation for each month from Aug 1995-Dec 2023
chargertracts2010_trimmed <- as.data.frame(chargertracts2010_trimmed[rep(seq_len(nrow(chargertracts2010_trimmed)), 
                                                         each=dim(allmonths)[1]),])
colnames(chargertracts2010_trimmed) <- "tractgeoid2010"
chargertracts2010_trimmed <- cbind(chargertracts2010_trimmed, allmonths)
rownames(chargertracts2010_trimmed) <- 1:nrow(chargertracts2010_trimmed)

## Constructing sum of all chargers in a tract (BUT NOT AT GAS STATIONS) at each opening date (or EVSE 
## installation date) and then renaming evse_open_year and evse_open_month to year and month respectively for 
## merging w/chargertracts2010_trimmed

# State, muni, and federal gov chargers are defensibly NOT workplace charging, since the public can probably still use
# them if they are visiting govt offices for personal or business matter.

total_chargers2010_trimmed <- chargerdata %>%
  group_by(tractgeoid2010, evse_open_year, evse_open_month) %>%
  filter(!id%in%gas_complete$id) %>% # To not count gas station chargers; stations only counted as competitors
  filter(!is.na(tractgeoid2010)) %>%
  filter(!is.na(evse_open_year) & !is.na(evse_open_month)) %>%
  # Filtering by available facility_type variable info (only ~22% of obs have non-blank facility_type)
  filter(!facility_type %in% c("RESEARCH_FACILITY", "CAR_DEALER", "OFFICE_BLDG", "WORKPLACE", "FLEET_GARAGE", 
                              "RENTAL_CAR_RETURN", "FIRE_STATION")) %>%
  filter(!(facility_type=="UTILITY" & grepl("florida power & light - evolution", station_name, ignore.case=T)==F)) %>%
  filter(
    !(facility_type=="COOP" &
        (grepl("city market - onion river", station_name, ignore.case=T)==F) &
        (grepl("honest weight", station_name, ignore.case=T)==F) & 
        (grepl("food", station_name, ignore.case=T)==F) &
        (grepl("farm and garden", station_name, ignore.case=T)==F) &
        (grepl("hendersonville community", station_name, ignore.case=T)==F) &
        (grepl("weavers way", station_name, ignore.case=T)==F) &
        (grepl("bluff country", station_name, ignore.case=T)==F) &
        (grepl("the merc", station_name, ignore.case=T)==F)
    )
  ) %>%
  # Filtering by keywords in station name (e.g., Meta, IRVINE  CO  OFC SCS 2725 L1 17, etc.)
  filter(grepl("meta ", station_name, ignore.case=T)==F) %>%
  summarise(totchargers=sum(totchargers, na.rm=TRUE)) %>%
  ungroup() %>%
  rename("year"="evse_open_year", "month"="evse_open_month")

## Merging in 'totchargers' variable (which actually captures how many new chargers were added to a tract 
## at the observation's opening date 'opendate', which 'year' and 'month' are based on) into dataframe of
## tracts and dates 'chargertracts2010_trimmed'
# Merging in the totchargers data
chargertracts2010_trimmed <- left_join(chargertracts2010_trimmed, total_chargers2010_trimmed, 
                                       by=c("tractgeoid2010","year","month"))
# Replacing NAs w/0s since in this dataframe, an NA in a given month-year indicates that no additional
# chargers opened or were installed in that month-year
chargertracts2010_trimmed$totchargers <- ifelse(is.na(chargertracts2010_trimmed$totchargers), 0, 
                                                chargertracts2010_trimmed$totchargers)
# Creating tract-month-level cumulative total chargers variable
chargertracts2010_trimmed <- chargertracts2010_trimmed %>% 
  group_by(tractgeoid2010) %>% 
  mutate(subcumchargers=cumsum(totchargers)) %>%
  ungroup()
# Removing observations from before Jan 2010 to fit the final sample period of Jan 2010-Dec 2023
chargertracts2010_trimmed <- chargertracts2010_trimmed %>% 
  filter(date>="2010-01-01")
# Keeping only necessary variables
chargertracts2010_trimmed <- chargertracts2010_trimmed[c("tractgeoid2010","year","month","subcumchargers")]
chargertracts2010_trimmed <- as.data.frame(chargertracts2010_trimmed)
rownames(chargertracts2010_trimmed) <- 1:nrow(chargertracts2010_trimmed)

rm(total_chargers2010_trimmed)

## Adding tract-month-level total chargers variable by period (2010 Census and 2020 Census) to analysis data
analysis <- left_join(analysis, chargertracts2010_trimmed, by=c("tractgeoid2010","year","month"))
analysis$subcumchargers <- ifelse(is.na(analysis$subcumchargers), 0, analysis$subcumchargers)

###########################################################################################################
## Creating number of competitors (stations) in each tract-month and adding it to the analysis data
###########################################################################################################
# Note: Again sticking w/2010 Census tract definitions when constructing this variable.
any(is.na(analysis$facility_id)) # No NA facility IDs, so no need to filter any out in the code below.
analysis <- analysis %>%
  group_by(tractgeoid2010, year, month) %>%
  mutate(n_competitors=n_distinct(facility_id)) %>%
  ungroup() %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, everything())

###########################################################################################################
### Creating outcome variable: adapted (0 or 1), based on stations' FIRST charger installation date
###########################################################################################################
### Taking the EARLIEST EVSE installation date as the station's adaptation date (particularly important in 
### cases where a station has multiple chargers and thus multiple installation dates).
  ## NOTE: Many stations adapted in the middle of a month or late in a month while the chronological date
  ## variable `date' has the first day of the month (01) as its default day, such that later in this
  ## subsection such stations' `adapted' variable takes on a value of 1 A MONTH LATE. For example, station A
  ## has an actual earliest open_date (i.e., adapt_date) of 04-15-2022. If I just take their true earliest
  ## open_date of 04-15-2022 as their adapt_date, then their `adapted' variable wouldn't equal 1 until a month
  ## later in month 05 (May), even though they opened their charger in month 04 (April).
    ## To remedy this, I construct a new open_date variable `open_date2' that has 01 as the default day (since
    ## we only care about the year-month combo) and use it to create each station's adapt_date.
adapted <- gas_complete %>%
  filter(!is.na(id)) %>% # To only keep data on stations that have a charger (i.e., that have ADAPTED)
  select(facility_id, id, open_date, evse_open_year, evse_open_month) %>%
  mutate(open_date2=paste(evse_open_year, evse_open_month, "01", sep="-")) %>%
  mutate(open_date2=ymd(open_date2)) %>%
  group_by(facility_id) %>%
  mutate(adapt_date=min(open_date2)) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(facility_id, adapt_date)
length(which(is.na(adapted$adapt_date))) # 0, good

## Adding station adaptation dates to analysis data and creating outcome variable 'adapted' equal to 1 if
## date>=adapt_date and 0 otherwise
analysis <- left_join(analysis, adapted, by="facility_id")
analysis <- analysis %>%
  mutate(adapted=case_when(
    !is.na(adapt_date) & date>=adapt_date ~ 1,
    TRUE ~ 0
  )
  ) %>%
  select(facility_id, date, year, month, cumchargers, subcumchargers, n_competitors, totcapacity, adapted, adapt_date, 
         everything())
analysis <- as.data.frame(analysis)
rownames(analysis) <- 1:nrow(analysis)

###########################################################################################################
### Saving the analysis dataset
###########################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(analysis, file="analysis.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())