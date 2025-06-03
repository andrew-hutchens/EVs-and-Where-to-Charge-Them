### PURPOSE: Clean EV registration data
### LAST EDITED: 12/4/2024
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
library(choroplethr)
library(readxl)

# NOTE: As of 1/13/2024, all states' data have the same variables (described in Atlas' data dictionary)
# EXCEPT for CA, (a ZIP state), FL (a county state), and WI (a county state). Adjusted code below from 
# last edit in December 2022 accordingly.

setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/Atlas")
zipstates <- c("co","ct","me","mn","nc","nj","nm","ny","or","tx","vt","wa","wi")
countystates <- c("fl","mt","tn","va") # CA is also a county data state but it is being processed separately (see end of code)

###################################################################################################
### States w/zip-level data
###################################################################################################
### NOTE: It IS possible for >1 snapshot to occur in the same month...use DMV ID to address this type of reporting?
  ## YES; for snapshot months w/more than one DMV ID, this means that >1 snapshot occurred in the same month,
  ## which means that summing vehiclecount in each snapshot month would double/triple/quadruple/etc. count
  ## the month's vehiclecount value. So what you need to do first is keep the latest DMV ID or snapshot day
  ## in each snapshot month, THEN sum vehiclecount in each snapshot month.

### NOTE: SUMMING UP vehiclecount SUMS ACROSS ALL VEHICLE TYPES IN A ZIP, SNAPSHOT YEAR, SNAPSHOT MONTH FOR STATES THAT
### HAVE A vehiclecount VARIABLE. FOR STATES W/O A VEHICLE COUNT VARIABLE (CURRENTLY WA AND WI), SIMPLY COUNTING THE NUMBER
### OF ROWS IN EACH ZIP-MONTH-YEAR COMBO TO CONSTRUCT NUMBER OF EVs (n_evs) VARIABLE.
  
## Reading in each state's raw data and performing basic cleaning
zipdata <- data.frame()
for (i in 1:length(zipstates)) {
  state <- zipstates[i]
  print(paste0("Working on ", toupper(state)))
  filepath <- paste0(state,"_ev_registrations_public.csv")
  data <- read.csv(filepath, header=TRUE)
  colnames(data) <- tolower(colnames(data))
  
  if (state!="wi"&state!="wa") {
    ## Keeping only necessary variables and renaming them
    data <- data %>%
      select("dmv_id"="dmv.snapshot.id", "snapshot_date"="dmv.snapshot..date.", "zip"="zip.code", 
             "reg_date"="registration.date", "vehiclecount"="vehicle.count")
    ## Creating separate day, month, and year variables based on DMV snapshot date variable
    # Breaking apart DMV snapshot variable
    data <- separate(data, col=snapshot_date, into=c("sep1","sep2","sep3"), sep=" ",
                     remove=TRUE, convert=TRUE)
    # Keeping only the date piece of the DMV snapshot variable
    data <- data %>%
      select(!c(sep1, sep2))
    # Removing the parentheses from the original date piece of the DMV snapshot variable
    data$sep3 <- gsub(pattern="(", replacement="", x=data$sep3, fixed=TRUE)
    data$sep3 <- gsub(pattern=")", replacement="", x=data$sep3, fixed=TRUE)
    data <- data %>%
      rename("snap_date"="sep3")
    # Splitting the now parentheses-less date piece into separate date variables
    data <- separate(data, col=snap_date, into=c("snap_month","snap_day","snap_year"), sep="/",
                     remove=FALSE, convert=TRUE)
    ## Summing up total number of EVs in each ZIP in each month
    ## The resulting n_evs variable is already a cumulative count of EVs on the road in each period (month) because
    ## the Atlas data collects snapshots of vehicle registration databases at different points
    ## in time, i.e., snapshots of the number of EVs (by make, model, etc) on the road at different points in time.
    data <- data %>% 
      group_by(zip, snap_year, snap_month) %>%
      filter(dmv_id==max(dmv_id)) %>%
      summarise(n_evs=sum(vehiclecount, na.rm=TRUE)) %>%
      group_by(zip) %>%
      mutate(n=n()) %>%
      dplyr::arrange(desc(n), zip, snap_year, snap_month) %>%
      ungroup() %>%
      select(!(n))
    ## Converting ZIP, month, and year variables to character for consistency across files
    data$zip <- as.character(data$zip)
    data$snap_month <- as.character(data$snap_month)
    data$snap_year <- as.character(data$snap_year)
    ## Adding in a state variable, rearranging variables, and collecting the dataset
    data$state <- toupper(state)
    data <- data %>%
      select(state, zip, snap_month, snap_year, n_evs)
    zipdata <- rbind(zipdata, data)
  } else if (state=="wi") {
    ## Keeping only necessary variables and renaming them
    data <- data %>%
      select("dmv_id"="dmv.id", "snapshot_date"="dmv.snapshot..date.", "zip"="zip.code",
             "reg_date"="registration.valid.date")
    ## Creating separate day, month, and year variables based on DMV snapshot date variable
    # Breaking apart DMV snapshot variable
    data <- separate(data, col=snapshot_date, into=c("sep1","sep2","sep3"), sep=" ",
                     remove=TRUE, convert=TRUE)
    ## Keeping only the date piece of the DMV snapshot variable
    data <- data %>%
      select(!c(sep1, sep2))
    # Removing the parentheses from the original date piece of the DMV snapshot variable
    data$sep3 <- gsub(pattern="(", replacement="", x=data$sep3, fixed=TRUE)
    data$sep3 <- gsub(pattern=")", replacement="", x=data$sep3, fixed=TRUE)
    data <- data %>%
      rename("snap_date"="sep3")
    # Splitting the now parentheses-less date piece into separate date variables
    data <- separate(data, col=snap_date, into=c("snap_month","snap_day","snap_year"), sep="/",
                     remove=FALSE, convert=TRUE)
    ## Summing up total number of EVs in each ZIP in each month
    ## The resulting n_evs variable is already a cumulative count of EVs on the road in each period (month) because
    ## the Atlas data collects snapshots of vehicle registration databases at different points
    ## in time, i.e., snapshots of the number of EVs (by make, model, etc) on the road at different points in time.
      # WI does not have vehiclecount variable, so each row of data corresponds to a separate vehicle during each snapshot
      # period (month-year). Thus, creating n_evs (# of EVs) variable involves counting then number of rows in each period
      # for each ZIP (after filtering each month for the latest DMV ID, which sequentially counts snapshots, to keep the
      # latest snapshot data in each month for months w/>1 snapshot).
    data <- data %>% 
      group_by(zip, snap_year, snap_month) %>%
      filter(dmv_id==max(dmv_id)) %>%
      summarise(n_evs=n()) %>%
      group_by(zip) %>%
      mutate(n=n()) %>%
      dplyr::arrange(desc(n), zip, snap_year, snap_month) %>%
      ungroup() %>%
      select(!(n))
    ## Converting ZIP, month, and year variables to character for consistency across files
    data$zip <- as.character(data$zip)
    data$snap_month <- as.character(data$snap_month)
    data$snap_year <- as.character(data$snap_year)
    ## Adding in a state variable, rearranging variables, and collecting the dataset
    data$state <- toupper(state)
    data <- data %>%
      select(state, zip, snap_month, snap_year, n_evs)
    zipdata <- rbind(zipdata, data)
  } else if (state=="wa") {
    ## Keeping only necessary variables and renaming them
    data <- data %>%
      select("dmv_id"="dmv.id", "snapshot_date"="dmv.snapshot", "zip"="zip.code",
             "reg_date"="registration.valid.date")
    ## Correcting weird 06-06-2022, 2022-08-10 entries in snapshot_date to 6/6/2022, 8/10/2022 as 
    ## they should be
    data$snapshot_date <- ifelse(data$snapshot_date=="WA DMV Direct (2022-06-06)", 
                                 "WA DMV Direct (6/6/2022)", data$snapshot_date)
    data$snapshot_date <- ifelse(data$snapshot_date=="WA DMV Direct (2022-08-10)", 
                                "WA DMV Direct (8/10/2022)", data$snapshot_date)
    ## Creating separate day, month, and year variables based on DMV snapshot date variable
    # Breaking apart DMV snapshot variable
    data <- separate(data, col=snapshot_date, into=c("sep1","sep2","sep3","sep4"), sep=" ",
                     remove=TRUE, convert=TRUE)
    # Keeping only the date piece of the DMV snapshot variable
    data <- data %>%
      select(!c(sep1, sep2, sep3))
    # Removing the parentheses from the original date piece of the DMV snapshot variable
    data$sep4 <- gsub(pattern="(", replacement="", x=data$sep4, fixed=TRUE)
    data$sep4 <- gsub(pattern=")", replacement="", x=data$sep4, fixed=TRUE)
    data <- data %>%
      rename("snap_date"="sep4")
    # Splitting the now parentheses-less date piece into separate date variables
    data <- separate(data, col=snap_date, into=c("snap_month","snap_day","snap_year"), sep="/",
                     remove=FALSE, convert=TRUE)
    ## Summing up total number of EVs in each ZIP in each month
    ## The resulting n_evs variable is already a cumulative count of EVs on the road in each period (month) because
    ## the Atlas data collects snapshots of vehicle registration databases at different points
    ## in time, i.e., snapshots of the number of EVs (by make, model, etc) on the road at different points in time.
      # WA does not have vehiclecount variable, so each row of data corresponds to a separate vehicle during each snapshot
      # period (month-year). Thus, creating n_evs (# of EVs) variable involves counting then number of rows in each period
      # for each ZIP (after filtering each month for the latest DMV ID, which sequentially counts snapshots, to keep the
      # latest snapshot data in each month for months w/>1 snapshot).
    data <- data %>% 
      group_by(zip, snap_year, snap_month) %>%
      filter(dmv_id==max(dmv_id)) %>%
      summarise(n_evs=n()) %>%
      group_by(zip) %>%
      mutate(n=n()) %>%
      dplyr::arrange(desc(n), zip, snap_year, snap_month) %>%
      ungroup() %>%
      select(!(n))
    ## Converting ZIP, month, and year variables to character for consistency across files
    data$zip <- as.character(data$zip)
    data$snap_month <- as.character(data$snap_month)
    data$snap_year <- as.character(data$snap_year)
    ## Adding in a state variable, rearranging variables, and collecting the dataset
    data$state <- toupper(state)
    data <- data %>%
      select(state, zip, snap_month, snap_year, n_evs)
    zipdata <- rbind(zipdata, data)
  }
}
rm(data)

# Converting ZIP variable to numeric
zipdata$zip <- as.numeric(zipdata$zip)

## Removing observations w/erroneous ZIPs in zipdata by state (e.g., 12139 is not an actual ZIP in CO)
ninestates <- c("CA","OR","WA")
eightstates <- c("NV","NM","AZ","ID","UT","CO","WY")
sevenstates <- c("TX","OK","LA","AR")
sixstates <- c("NE","KS","MO","IL")
fivestates <- c("MT","ND","SD","MN","IA","WI")
fourstates <- c("KY","OH","IN","MI")
threestates <- c("FL","GA","AL","MS","TN")
twostates <- c("SC","NC","VA","WV","MD")
onestates <- c("DE","PA","NY")
zerostates <- c("NJ","CT","RI","NH","MA","VT","ME")
# Dropping observations with missing ZIPs
zipdata$err <- ifelse(is.na(zipdata$zip), 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations with completely erroneous ZIPs (like only three digits or just 0)
zipdata$err <- ifelse(nchar(zipdata$zip)<4, 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have 5-digit ZIPs but have 4-digit ZIPs in the
# EV registration data
zipdata$err <- ifelse(!(zipdata$state%in%zerostates)&nchar(zipdata$zip)<5, 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that should have only 4-digits (zerostates) but have ZIPs
# from states that have 5-digit ZIPs
zipdata$err <- ifelse(zipdata$state%in%zerostates&nchar(zipdata$zip)>4, 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 9s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%ninestates&substr(zipdata$zip,1,1)!="9", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 8s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%eightstates&substr(zipdata$zip,1,1)!="8", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 7s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%sevenstates&substr(zipdata$zip,1,1)!="7", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 6s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%sixstates&substr(zipdata$zip,1,1)!="6", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 5s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%fivestates&substr(zipdata$zip,1,1)!="5", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 4s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%fourstates&substr(zipdata$zip,1,1)!="4", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 3s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%threestates&substr(zipdata$zip,1,1)!="3", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 2s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%twostates&substr(zipdata$zip,1,1)!="2", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]
# Dropping observations located in states that have ZIPs with leading 1s but have something else in
# the EV registration data
zipdata$err <- ifelse(zipdata$state%in%onestates&substr(zipdata$zip,1,1)!="1", 1, 0)
zipdata <- zipdata[!is.na(zipdata$err)&zipdata$err!=1,]

# Changing date variable names in zipdata, removing the 'err' variable now that it is no longer needed, and removing 2024
# data (current sample of interest ends in Dec 2023)
zipdata <- zipdata %>% 
  rename("month"="snap_month", "year"="snap_year") %>%
  select(state, zip, month, year, n_evs) %>%
  filter(year<2024)

## Aggregating ZIP-level EV count up to the county-level using zip-county crosswalk
# Briefly changing the working directory to load the data and convert crosswalk ZIPs to numeric
# for consistency with the EV registration data and other data
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
zip_county_crosswalk <- read.csv("zip_county_crosswalk.csv", header=TRUE)
zip_county_crosswalk$zip <- as.numeric(zip_county_crosswalk$zip)
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/Atlas")
# Creating quarter variables in the EV registration data zipdata to facilitate merging with the
# zip-county crosswalk data
zipdata <- zipdata %>%
  mutate(quarter=case_when(
    zipdata$month=="1"|zipdata$month=="2"|zipdata$month=="3" ~ 1,
    zipdata$month=="4"|zipdata$month=="5"|zipdata$month=="6" ~ 2,
    zipdata$month=="7"|zipdata$month=="8"|zipdata$month=="9" ~ 3,
    zipdata$month=="10"|zipdata$month=="11"|zipdata$month=="12" ~ 4,
    TRUE ~ NA_real_
  )
  )
# Merging in countyfips that correspond to each ZIP in zipdata and converting countyfips to numeric
# to match other cleaned data files
zipdata$year <- as.numeric(zipdata$year)
zipdata$month <- as.numeric(zipdata$month)
zipdata <- left_join(zipdata, zip_county_crosswalk, by=c("zip","year","quarter"))
zipdata$countyfips <- as.numeric(zipdata$countyfips)

## There are 75 ZIPs missing from the zip-county crosswalk:
missingzips <- unique(zipdata[is.na(zipdata$countyfips),]$zip)

## Manually inputting countyfips for the 70 ZIPs that have a missing county
for (j in 1:dim(zipdata)[1]) {
  print(paste0("Working on observation ", j))
  if (zipdata$zip[j]==81502) {
    zipdata$countyfips[j] <- 8077
  } else if (zipdata$zip[j]==87509) {
    zipdata$countyfips[j] <- 35049
  } else if (zipdata$zip[j]==56459) {
    zipdata$countyfips[j] <- 27035
  } else if (zipdata$zip[j]==10521) {
    zipdata$countyfips[j] <- 36119
  } else if (zipdata$zip[j]==10572) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==10997) {
    zipdata$countyfips[j] <- 36071
  } else if (zipdata$zip[j]==11043) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==12228) {
    zipdata$countyfips[j] <- 36001
  } else if (zipdata$zip[j]==12229) {
    zipdata$countyfips[j] <- 36001
  } else if (zipdata$zip[j]==12233) {
    zipdata$countyfips[j] <- 36001
  } else if (zipdata$zip[j]==12238) {
    zipdata$countyfips[j] <- 36001
  } else if (zipdata$zip[j]==13503) {
    zipdata$countyfips[j] <- 36065
  } else if (zipdata$zip[j]==13763) {
    zipdata$countyfips[j] <- 36007
  } else if (zipdata$zip[j]==14261) {
    zipdata$countyfips[j] <- 36029
  } else if (zipdata$zip[j]==97143) {
    zipdata$countyfips[j] <- 41057
  } else if (zipdata$zip[j]==75037) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==75245) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==75266) {
    zipdata$countyfips[j] <- 48113
  } else if (zipdata$zip[j]==76068) {
    zipdata$countyfips[j] <- 48363
  } else if (zipdata$zip[j]==77337) {
    zipdata$countyfips[j] <- 48201
  } else if (zipdata$zip[j]==78364) {
    zipdata$countyfips[j] <- 48273
  } else if (zipdata$zip[j]==79960) {
    zipdata$countyfips[j] <- 48141
  } else if (zipdata$zip[j]==95899) {
    zipdata$countyfips[j] <- 6067
  } else if (zipdata$zip[j]==96218) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==96319) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==96349) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==96530) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==96620) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==96662) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==96678) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==98205) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==98231) {
    zipdata$countyfips[j] <- 53073
  } else if (zipdata$zip[j]==98504) {
    zipdata$countyfips[j] <- 53067
  } else if (zipdata$zip[j]==98634) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==98810) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==99332) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==52407) {
    zipdata$countyfips[j] <- 19113
  } else if (zipdata$zip[j]==53712) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==53791) {
    zipdata$countyfips[j] <- 55025
  } else if (zipdata$zip[j]==54221) {
    zipdata$countyfips[j] <- 55071
  } else if (zipdata$zip[j]==56317) {
    zipdata$countyfips[j] <- 27097
  } else if (zipdata$zip[j]==27695) {
    zipdata$countyfips[j] <- 37183
  } else if (zipdata$zip[j]==27972) {
    zipdata$countyfips[j] <- 37055
  } else if (zipdata$zip[j]==28502) {
    zipdata$countyfips[j] <- 37107
  } else if (zipdata$zip[j]==7308) {
    zipdata$countyfips[j] <- 34017
  } else if (zipdata$zip[j]==7703) {
    zipdata$countyfips[j] <- 34025
  } else if (zipdata$zip[j]==8526) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==10602) {
    zipdata$countyfips[j] <- 36119
  } else if (zipdata$zip[j]==11571) {
    zipdata$countyfips[j] <- 36059
  } else if (zipdata$zip[j]==11595) {
    zipdata$countyfips[j] <- 36059
  } else if (zipdata$zip[j]==11597) {
    zipdata$countyfips[j] <- 36059
  } else if (zipdata$zip[j]==12201) {
    zipdata$countyfips[j] <- 36001
  } else if (zipdata$zip[j]==12227) {
    zipdata$countyfips[j] <- 36001
  } else if (zipdata$zip[j]==12593) {
    zipdata$countyfips[j] <- 36021
  } else if (zipdata$zip[j]==14302) {
    zipdata$countyfips[j] <- 36063
  } else if (zipdata$zip[j]==14602) {
    zipdata$countyfips[j] <- 36055
  } else if (zipdata$zip[j]==97258) {
    zipdata$countyfips[j] <- 41051
  } else if (zipdata$zip[j]==75258) {
    zipdata$countyfips[j] <- 48113
  } else if (zipdata$zip[j]==75262) {
    zipdata$countyfips[j] <- 48113
  } else if (zipdata$zip[j]==75429) {
    zipdata$countyfips[j] <- 48231
  } else if (zipdata$zip[j]==75962) {
    zipdata$countyfips[j] <- 48347
  } else if (zipdata$zip[j]==77343) {
    zipdata$countyfips[j] <- 48471
  } else if (zipdata$zip[j]==78262) {
    zipdata$countyfips[j] <- 48029
  } else if (zipdata$zip[j]==5302) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==5304) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==6859) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==6860) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==9622) {
    zipdata$countyfips[j] <- NA
  } else if (zipdata$zip[j]==27228) {
    zipdata$countyfips[j] <- 37037
  } else if (zipdata$zip[j]==8625) {
    zipdata$countyfips[j] <- 34021
  } else if (zipdata$zip[j]==8666) {
    zipdata$countyfips[j] <- 34021
  } else if (zipdata$zip[j]==11737) {
    zipdata$countyfips[j] <- 36059
  } else if (zipdata$zip[j]==14443) {
    zipdata$countyfips[j] <- 36069
  } else if (zipdata$zip[j]==13056) {
    zipdata$countyfips[j] <- 36023
  } else if (zipdata$zip[j]==75283) {
    zipdata$countyfips[j] <- 48113
  } else if (zipdata$zip[j]==5303) {
    zipdata$countyfips[j] <- 50025
  }
}
rm(missingzips)
# Removing observations w/NA counties
zipdata <- zipdata[!is.na(zipdata$countyfips),]

## Aggregating ZIP-level number of EVs data to the county level (note that the n_evs variable is
## still cumulative by month even after aggregating up to the county level)
zipdata <- zipdata %>% 
  group_by(state, countyfips, year, month) %>% 
  summarise(n_evs=sum(n_evs)) %>%
  ungroup() %>%
  arrange(state, countyfips, year, month)
# NOTE: Some states have counties from other states (e.g., CO has some counties from AZ). The
# subsection of code below will address this issue.

## Dropping observations w/counties that are not in the state listed in the data (e.g., dropping
## observations in CO that have countyfips from AZ)
# Creating dataframe of states and their respective state FIPS codes
statecodes <- as.data.frame(cbind(c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
                                    "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
                                    "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
                                    "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"), 
                                  c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,
                                    28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,
                                    50,51,53,54,55,56)))
colnames(statecodes) <- c("state","statefips")
statecodes$statefips <- as.numeric(statecodes$statefips)
# Flagging and dropping observations w/out-of-state countyfips
for (k in 1:dim(statecodes)[1]) {
  st <- statecodes$state[k]
  stfips <- statecodes$statefips[k]
  zipdata$err <- 0
  if (stfips<10) {
    # Dropping observations w/countyfips with >4 characters (in numeric form)
    zipdata$err <- ifelse(zipdata$state==st&nchar(zipdata$countyfips)>4, 1, 0)
    zipdata <- zipdata[zipdata$err==0,]
    # Dropping observations w/countyfips from other states and removing the err variable
    zipdata$err <- ifelse(zipdata$state==st&as.numeric(substr(zipdata$countyfips,1,1))!=stfips, 1, 0)
    zipdata <- zipdata[zipdata$err==0,]
    zipdata <- zipdata[c("state","countyfips","year","month","n_evs")]
  } else if (stfips>=10) {
    # Dropping observations w/countyfips from other states and removing the err variable
    zipdata$err <- ifelse(zipdata$state==st&as.numeric(substr(zipdata$countyfips,1,2))!=stfips, 1, 0)
    zipdata <- zipdata[zipdata$err==0,]
    zipdata <- zipdata[c("state","countyfips","year","month","n_evs")]
  }
}

## Converting month and year variables to numeric and sorting the data by state-county-year-month
zipdata$month <- as.numeric(zipdata$month)
zipdata$year <- as.numeric(zipdata$year)
zipdata <- zipdata %>% 
  arrange(state, countyfips, year, month)
zipdata <- as.data.frame(zipdata)

## Clearing out some objects to reduce clutter
rm(statecodes,onestates,twostates,threestates,fourstates,fivestates,sixstates,sevenstates,
   eightstates,ninestates,zerostates,st,stfips,state)

###################################################################################################
### States w/county-level data
###################################################################################################
countydata <- data.frame()
for (i in 1:length(countystates)) {
  state <- countystates[i]
  print(paste0("Working on ", toupper(state)))
  filepath <- paste0(state,"_ev_registrations_public.csv")
  data <- read.csv(filepath, header=TRUE)
  colnames(data) <- tolower(colnames(data))
  
  if (state!="ca"&state!="fl") {
    ## Keeping only necessary variables and renaming them
    data <- data %>%
      select("dmv_id"="dmv.snapshot.id", "snapshot_date"="dmv.snapshot..date.", "county"="county",
             "reg_date"="registration.date", "vehiclecount"="vehicle.count")
    ## Creating countyfips variable since MT, TN, and VA's county variable gives county NAMES not 
    ## FIPS codes, first dropping any obs with blanks for county
    data <- data[data$county!="",]
    data$countyfips <- NA
    for (i in 1:dim(data)[1]){
      data$countyfips[i] <- as.numeric(fips(state=state, county=data$county[i]))
    }
    # Removing county variable
    data <- data %>%
      select(!(county))
    ## Creating separate day, month, and year variables based on DMV snapshot date variable
    # Breaking apart DMV snapshot variable
    data <- separate(data, col=snapshot_date, into=c("sep1","sep2","sep3"), sep=" ",
                     remove=TRUE, convert=TRUE)
    # Keeping only the date piece of the DMV snapshot variable
    data <- data %>%
      select(!c(sep1, sep2))
    # Removing the parentheses from the original date piece of the DMV snapshot variable
    data$sep3 <- gsub(pattern="(", replacement="", x=data$sep3, fixed=TRUE)
    data$sep3 <- gsub(pattern=")", replacement="", x=data$sep3, fixed=TRUE)
    data <- data %>%
      rename("snap_date"="sep3")
    # Splitting the now parentheses-less date piece into separate date variables
    data <- separate(data, col=snap_date, into=c("snap_month","snap_day","snap_year"), sep="/",
                     remove=FALSE, convert=TRUE)
    ## Summing up total number of EVs in each county in each month that is in the given state's data
    ## The resulting n_evs variable is already a cumulative count of EVs on the road because
    ## the Atlas data collects snapshots of vehicle registration databases at different points
    ## in time, i.e., snapshots of the total number of EVs on the road at different points in time.
    data <- data %>% 
      group_by(countyfips, snap_year, snap_month) %>%
      filter(dmv_id==max(dmv_id)) %>%
      summarise(n_evs=sum(vehiclecount, na.rm=TRUE)) %>%
      group_by(countyfips) %>%
      mutate(n=n()) %>%
      dplyr::arrange(desc(n), countyfips, snap_year, snap_month) %>%
      ungroup() %>%
      select(!(n))
    ## Converting day, month, and year variables to character for consistency across files
    data$snap_month <- as.character(data$snap_month)
    data$snap_year <- as.character(data$snap_year)
    # Adding in a state variable, rearranging variables, and collecting the dataset
    data$state <- toupper(state)
    data <- data %>%
      select(state, countyfips, snap_month, snap_year, n_evs)
    countydata <- rbind(countydata, data)
  } else if (state=="fl") {
    ## Keeping only necessary variables and renaming them
    data <- data %>%
      select("dmv_id"="dmv.id", "snapshot_date"="dmv.snapshot..date.", "county"="county",
             "reg_date"="registration.valid.date")
    ## Dropping observations w/unknown county or "Other" as a county
    data <- data[data$county!="Unknown",]
    data <- data[data$county!="Other",]
    ## Manually inputting countyfips (raw data only provides county name)
    # Adjusting erroneous county names before using fips function
    data$county <- ifelse(data$county=="Dade", "Miami-Dade", data$county)
    data$county <- ifelse(data$county=="Gadsen", "Gadsden", data$county)
    # Assigning each observation its county FIPs code and removing the 'county' variable
    for (j in 1:dim(data)[1]){
      data$countyfips[j] <- as.numeric(fips(state="florida", county=data$county[j]))
    }
    data <- data %>%
      select(!(county))
    ## Creating separate day, month, and year variables based on DMV snapshot date variable
    # Breaking apart DMV snapshot variable
    data <- separate(data, col=snapshot_date, into=c("sep1","sep2","sep3","sep4","sep5"), sep=" ",
                     remove=TRUE, convert=TRUE)
    # Keeping only the date piece of the DMV snapshot variable
    data <- data %>%
      select(!c(sep1, sep2, sep3, sep4))
    # Removing the parentheses from the original date piece of the DMV snapshot variable
    data$sep5 <- gsub(pattern="(", replacement="", x=data$sep5, fixed=TRUE)
    data$sep5 <- gsub(pattern=")", replacement="", x=data$sep5, fixed=TRUE)
    data <- data %>%
      rename("snap_date"="sep5")
    # Splitting the now parentheses-less date piece into separate date variables
    data <- separate(data, col=snap_date, into=c("snap_month","snap_day","snap_year"), sep="/",
                     remove=FALSE, convert=TRUE)
    ## Summing up total number of EVs in each county in each month
    ## The resulting n_evs variable is already a cumulative count of EVs on the road because
    ## the Atlas data collects snapshots of vehicle registration databases at different points
    ## in time, i.e., snapshots of the total number of EVs on the road at different points in time.
      # FL does not have vehiclecount variable, so each row of data corresponds to a separate vehicle during each snapshot
      # period (month-year). Thus, creating n_evs (# of EVs) variable involves counting then number of rows in each period
      # for each ZIP (after filtering each month for the latest DMV ID, which sequentially counts snapshots, to keep the
      # latest snapshot data in each month for months w/>1 snapshot).
    data <- data %>% 
      group_by(countyfips, snap_year, snap_month) %>%
      filter(dmv_id==max(dmv_id)) %>%
      summarise(n_evs=n()) %>%
      group_by(countyfips) %>%
      mutate(n=n()) %>%
      dplyr::arrange(desc(n), countyfips, snap_year, snap_month) %>%
      ungroup() %>%
      select(!(n))
    ## Converting day, month, and year variables to character for consistency across files
    data$snap_month <- as.character(data$snap_month)
    data$snap_year <- as.character(data$snap_year)
    # Adding in a state variable, rearranging variables, and collecting the dataset
    data$state <- toupper(state)
    data <- data %>%
      select(state, countyfips, snap_month, snap_year, n_evs)
    countydata <- rbind(countydata, data)
  }
}
rm(data)

## Cleaning CA's data separately since CA provides EV registration data through late 2023 but not via Atlas
# Reading in vehicle poplulation data; keeping and renaming select columns, keeping only BEV or PHEV observations, and
# dropping observations with county="Out of State" or county="Out Of State"
setwd("H:/My Drive/Research/EV Charging/Data/Raw Data/CEC")
caregs <- read_xlsx("Vehicle_Population_Last_updated_04-30-2024_ada.xlsx", sheet="County", 
                    col_names=TRUE)
caregs <- caregs %>%
  select("year"="Data Year", "county"="County", "fueltype"="Fuel Type", "make"="Make", "model"="Model", 
         "n"="Number of Vehicles") %>%
  #filter(fueltype%in%c("Battery Electric (BEV)", "Plug-in Hybrid (PHEV)")) %>%
  filter(!county%in%c("Out of State", "Out Of State")) %>%
  arrange(county, year)
# Summing up total BEV/PHEVs by county (raw data has number of BEVs and PHEVs by make and model within
# each county; don't need that information for this paper, just the count)
caregs <- caregs %>% 
  group_by(county, year) %>% 
  summarise(n_evs=sum(n, na.rm=TRUE)) %>%
  ungroup()
# Creating county FIPS variable since raw CEC data has county name, not county FIPS
caregs$countyfips <- NA
for (i in 1:dim(caregs)[1]){
  print(paste0("Working on observation ", i))
  caregs$countyfips[i] <- as.numeric(fips(state="ca", county=caregs$county[i]))
}
caregs$month <- 12 # CA data is annual, so just assigning its month variable 12
caregs <- caregs %>%
  select(year, month, countyfips, n_evs)
caregs <- as.data.frame(caregs)
# Adding state variable to caregs and reordering/renaming variables to match other states' dataframes
caregs <- caregs %>%
  mutate(state="CA") %>%
  select(state, countyfips, month, year, n_evs) %>%
  rename("snap_year"="year", "snap_month"="month") %>%
  mutate(snap_year=as.character(snap_year), snap_month=as.character(snap_month))
# Adding cleaned CA EV registrations data to county states registration dataframe 'countydata;
countydata <- rbind(countydata, caregs)

## Renaming snap_month and snap_year variables to year and month and converting them to numeric, removing obs w/missing
## county, and reordering variables
countydata <- countydata %>%
  rename("month"="snap_month", "year"="snap_year") %>%
  mutate(month=as.numeric(month), year=as.numeric(year)) %>%
  filter(!is.na(countydata$countyfips)) %>%
  select(state, countyfips, year, month, n_evs)

##################################################################################################
### Combining cleaned zip (converted to county) and county level EV registration data and saving
##################################################################################################
evregs <- rbind(countydata, zipdata)
evregs <- as.data.frame(evregs)
rownames(evregs) <- 1:nrow(evregs)

setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(evregs, file="evregs.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())
