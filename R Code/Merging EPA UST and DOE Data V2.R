### PURPOSE: Merge EPA UST gas station data (gasstations) w/DOE charger data (chargerdata)
### in order to identify the gas stations in the DOE charger data and know of all 
### "participants" in the DOE data.
### LAST EDITED: 12/20/2024
### EDITED FROM: Laptop

### NOTE: At this stage, gas stations will have multiple obs due to having a separate obs for each of a station's tanks but
### could also have even more obs due to having a separate obs for matching with each specific CHARGER in the DOE
### data. For example, Chevron NMB could have 2 observations (with the same facility-level data but different
### tank-level data between the two observations) before merging in its charger data. If there are 2 chargers at
### Chevron NMB, then after merging in the charger data, there will be a total of 4 observations for Chevron NMB; 
### one obs for ID-Tank1-Charger1, one obs for ID-Tank1-Charger2, one obs for ID-Tank2-Charger2, and one obs for
### ID-Tank2-Charger2, where ID is Chevron NMB's unique facility ID.

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

############################################################################################################
### Loading current gas station universe data 'gasstations' and cleaned DOE chargers data
############################################################################################################
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
gasstations <- read.csv("gasstations.csv") # This is only gas stations data.
chargerdata <- read.csv("chargerdata.csv") # This is only charger data.

rownames(gasstations) <- 1:nrow(gasstations)
rownames(chargerdata) <- 1:nrow(chargerdata)

############################################################################################################
### Merging the cleaned EPA UST gas stations data and the cleaned DOE charger data
############################################################################################################
length(which(chargerdata$facility_type=="")) # 46,425 chargers have a blank facility_type
### NOTE: Many chargers have a blank facility_type, so the code below devises another way to identify whether those
### chargers are gas stations based on their proximity to UST stations.
## NOTE: Cleaned gas stations (UST) data 'gasstations' already has supermarkets and other non-stations removed.

## Loading charger nearest station neighbor data
chargerneighbor <- read.csv("charger nearest station.csv")

## Merging charger nearest station ID and distance into main charger dataset
chargerdata <- left_join(chargerdata, chargerneighbor[c("id","facility_id","mi_to_nb")], by="id") 
# Recall that 'facility_id' is a station's ID variable and 'id' is a charger's ID variable.

## For chargers with facility_type=="GAS_STATION" or facility_type=="CONVENIENCE_STORE" (that are NOT
## supermarkets!), creating a separate dataset and filtering out observations that are not actually gas 
## stations despite being classified as GAS_STATION, CONVENIENCE_STORE, or TRUCK_STOP (such observations were
## manually identified in prior scripts)
matchedgas <- chargerdata %>%
  filter(facility_type%in%c("GAS_STATION", "CONVENIENCE_STORE", "TRUCK_STOP")) %>%
  filter(!id%in%c(25527,67155,118949,149909,153821,167446,168276,200957,200959,212751,237656,238570,259986,
                  306848,307406,311129,315612,52659))
rownames(matchedgas) <- 1:nrow(matchedgas)
# Some chargers are far from their matched gas stations, so checking out data on the gas stations they
# matched to (i.e., on stations that are closest to the chargers)
hist(matchedgas$mi_to_nb) # This shows lots of chargers >0.03 miles away from their matched stations
check <- left_join(gasstations[c("facility_id","name","address","city","state","lat","lon")], 
                   matchedgas[c("facility_id","id","facility_type","station_name","street_address","city",
                                "state","lat","lon","mi_to_nb")], 
                   by="facility_id")
check <- check %>% 
  filter(!is.na(id)) # Keeping only stations that matched to a charger
check <- check %>% 
  group_by(facility_id) %>% 
  slice_head(n=1) %>%
  ungroup() # Keeping one observation per station instead of gasstations' original tank-level observations
# Creating list of charger IDs of chargers w/facility_type=="CONVENIENCE_STORE", facility_type=="GAS_STATION", 
# or facility_type=="TRUCK_STOP" that matched to the correct station in the UST data BY NAME or IF 
# RIDICULOUSLY CLOSE TO STATION, I.E., CRAZY SMALL mi_to_nb; 
keep <- c(40965,306838,257004,254406,150281,313734,260252,204645,306870,204646,166809,223594,262363,
          302207,302208,194064,233043,153788,189686,297172,167426,260253,185273,310662,117080,320137,
          196520,168041,257002,69735,171469,301710,185198,296735,306869,194066,167984,185209,
          170524,185216,307906,302931,167422,301735,214069,253274,216370,241150,321137,186332,214073,
          166706,301729,212787,227097,222342,116795,257661,155373,154657,220685,154205,190505,237617,
          221098,253759,200616,220195,35841,40998,190473,40996,40997,35840,220694,319954,303730,
          226531,191515,212746,201306,197720,190806,221781,258266,227706,229710,221099,211907,254401,
          187958,261454,226530,206276,153541,201304,196522,256575,261451,306840,189000,163996,318174,
          213146,262347,218013,235596,183592,302666,155991,307840,191096,301723,200859,190466,250741,
          314456,163531,258800,301734,262700,310541,233388,259996,256053,183563,196521,252513,224156,
          201676,205231,183831,183271,183832,222796,170435,311988,202622,212752,241141,296743,233047,
          117725,195782,234962,233088,102329,153792,257675,205810,201679,154440,170565,231695,203262,
          206273,308416,257678,256571,167577,253769,218073,153542,237663,154207,237635,206270,252510,
          257001,201931,117723,170434,296727,206271,194085,217094,237630,261450,262351,308422,311220,
          256052,157974,116814,186333,186179,238209,253752,222335,302199,319952,203259,202936,211905,
          250743,227379,318177,194063,201923,311993,222341,221100,262352,229091,220697,220684,220693,
          253761,303732,306842,238204,150182,259975,225838,259974,259997,191516,320143,201673,
          186334,153789,316167,190480,190503,171470,311992,216366,218654,220198,170433,170523,260347,
          212744,201928,200861,190511,257669,311997,194078,153540,202617,82641,166865,71497,241149,
          170526,42415,312519,233044,321141,214081,200862,196255,185672,154791,185672,230204,298639,298640,
          298642,298643,298644,298645,320688)
# Manually checking each match in keep using Google Maps to confirm that chargers are in fact at
# gas stations and removing non-stations from matchedgas
View(check %>% select("station_name"="name","station_id"="facility_id","charger_name"="station_name",
                      "charger_id"="id",mi_to_nb,"gaslat"="lat.x","gaslon"="lon.x",
                      "chargerlat"="lat.y","chargerlon"="lon.y") %>% 
       filter(charger_id%in%keep) %>% 
       mutate(gascoords=paste(gaslat, gaslon, sep=", "), 
              chargercoords=paste(chargerlat, chargerlon, sep=", ")) %>% 
       select(station_name,charger_name,mi_to_nb,chargercoords,gascoords,station_id,charger_id))
notstations <- c(231695,252510,259975,196255) # These are chargers located at stations other than their match
                                              # in the UST data.
matchedgas <- matchedgas %>%
  filter(id%in%keep) %>%
  filter(!id%in%notstations)
rm(check,keep)
# Note: matchedgas now contains all chargers that were matched to a gas station using the facility_type
# variable in the DOE data.

## For chargers with BLANK facility_type, creating dataset of chargers NOT already in matchedgas and w/nearest 
## UST neighbor within 0.03 miles (~50 yards) and manually* pruning non-stations to try finding some more 
## adapted stations
  # * Only done fully manually once, then automated in subsequent code/data update runs to avoid having to
  # search for nearly 600 stations in Google Maps repetitively. Automation involves reading in old version
  # of "charger nearest actual station" and using it to populate known Y/Ns in the new closeneighbors
  # dataframe.
  # NOTE: Purposely being conservative using the 0.03 miles (~51 yards) limit.
closeneighbors <- chargerneighbor %>%
  filter(mi_to_nb<=0.03) %>%
  filter(!id%in%unique(matchedgas$id))
# Saving dataset of close neighbors to manually prune matches that are not actually located at stations
write.csv(closeneighbors, file="closeneighbors.csv", row.names=FALSE)
# IN UPDATE RUNS ONLY: Reading in old manually identified actual stations data to populate known Y/Ns in new
# closeneighbors dataframe
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data - NCSU Version")
old_actualstations <- read.csv("charger nearest actual station DEPRECATED.csv")
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
# Populating known Y/Ns in new closeneighbors data and, in doing so, creating new actualstations dataframe
actualstations <- left_join(closeneighbors, old_actualstations[c("id","actualstation")], by="id")
newstations <- actualstations %>% 
  filter(is.na(actualstation)) %>% 
  select(id)
actualstations <- actualstations %>%
  mutate(actualstation=case_when(
    id==116804 ~ "N",
    id==155270 ~ "Y",
    id==163876 ~ "Y",
    id==166807 ~ "Y",
    id==171465 ~ "Y",
    id==171467 ~ "Y",
    id==185013 ~ "Y",
    id==187164 ~ "Y",
    id==189354 ~ "Y",
    id==194068 ~ "Y",
    id==201316 ~ "Y",
    id==201678 ~ "Y",
    id==213152 ~ "Y",
    id==218609 ~ "Y",
    id==218933 ~ "Y",
    id==229090 ~ "Y",
    id==229920 ~ "Y",
    id==237619 ~ "Y",
    id==238206 ~ "Y",
    id==238211 ~ "Y",
    id==241155 ~ "Y",
    id==250711 ~ "Y",
    id==251153 ~ "Y",
    id==256995 ~ "Y",
    id==296756 ~ "Y",
    id==302930 ~ "Y",
    id==311222 ~ "Y",
    id==313101 ~ "Y",
    id==314459 ~ "Y",
    id==314471 ~ "Y",
    id==314489 ~ "Y",
    id==315613 ~ "Y",
    id==316164 ~ "Y",
    id==316904 ~ "Y",
    id==319953 ~ "N",
    id==320147 ~ "N",
    id==322836 ~ "Y",
    TRUE ~ actualstation
  )
  )
# Keeping only confirmed stations and dropping unnecessary columns
actualstations <- actualstations %>%
  filter(actualstation=="Y") %>%
  select(id, facility_id, mi_to_nb)
rownames(actualstations) <- 1:nrow(actualstations)
# Creating separate charger dataset 'matchedgasmanual' of only chargers with blank facility_type or a
# facility_type other than GAS_STATION, CONVENIENCE_STORE, or TRUCK_STOP that
# were MANUALLY matched to an actual gas station in the steps and datasets above
matchedgasmanual <- chargerdata %>%
  filter(!id%in%matchedgas$id) %>%
  filter(id%in%actualstations$id) %>%
  filter(!id%in%notstations)
rownames(matchedgasmanual) <- 1:nrow(matchedgasmanual)
rm(newstations, old_actualstations)

## Creating single dataframe 'matched2gas' of all chargers matched to a gas station (either using DOE's 
## facility_type variable or by using chargers' nearest neighbors within .03 miles)
matched2gas <- rbind(matchedgas, matchedgasmanual)
length(unique(matched2gas$id)) # 603 distinct chargers BUT SOME ARE SEPARATED OBS, e.g., separate obs for each
# charger at the same location when each location should have one obs that includes the count of all chargers
# across all levels/types.
length(unique(matched2gas$street_address)) # 534 unique addresses for 562 obs, which means there are definitely
# some locations whose charger info is separated into >1 obs.
num_ids_per_address <- matched2gas %>% 
  group_by(street_address) %>% 
  summarise(N_distinct_obs=n_distinct(id)) %>%
  ungroup()
View(num_ids_per_address)
repeats <- num_ids_per_address[num_ids_per_address$N_distinct_obs>1,]
View(matched2gas[matched2gas$street_address%in%repeats$street_address,][c("station_name",
                                                                          "street_address","city",
                                                                          "state","open_date",
                                                                          "n_level1","n_level2",
                                                                          "n_dcfast")])
rm(num_ids_per_address,repeats)
# Multiple, separated entries for chargers of the same level at the same address don't seem to happening just
# because of different opening dates (some sets of multiple obs have the same opening date).

## Merging charger data (for chargers actually located at stations) into station data & renaming variables
gas_complete <- left_join(gasstations, matched2gas, by="facility_id")
# Removing unnecessary and duplicate variables from the join and renaming some variables
gas_complete <- gas_complete %>% 
  select(!c("street_address","msa_name2020.y","csa_id2020.y","county.y","countyfips.y","tractgeoid2020.y",
            "tract2020.y","msa_name2010.y","csa_id2010.y","tractgeoid2010.y","tract2010.y","city.y","zip.y",
            "state.y","street_address","lat.y","lon.y")) %>%
  rename("city"="city.x", "county"="county.x", "state"="state.x", "zip"="zip.x", "lat"="lat.x", "lon"="lon.x", 
         "msa_name2020"="msa_name2020.x", "csa_id2020"="csa_id2020.x", "countyfips"="countyfips.x", 
         "tractgeoid2020"="tractgeoid2020.x", "tract2020"="tract2020.x", "msa_name2010"="msa_name2010.x", 
         "csa_id2010"="csa_id2010.x", "tractgeoid2010"="tractgeoid2010.x", "tract2010"="tract2010.x")
gas_complete <- as.data.frame(gas_complete)
# Now collapsing the multiple observations of some chargers whose data is separated by port into one
# observation
  # NOTE: In 'consolidated' dataframe, SUMMING n_level1, n_level2, n_dcfast, and totchargers because these do 
  # vary across different port observations for the same charger location.
ogdata <- gasstations %>% 
  group_by(facility_id) %>% 
  summarise(nog=n()) %>%
  ungroup()
newdata <- gas_complete %>% 
  group_by(facility_id) %>% 
  summarise(nnew=n()) %>%
  ungroup()
comparing <- left_join(ogdata, newdata, by="facility_id")
comparing$diff <- ifelse(comparing$nog!=comparing$nnew, "Y", "N") # 60 out of 72,632 facilities have
                                                                  # increased observations due to
                                                                  # multiple chargers matching to them
                                                                  # in matched2gas (this happens because
                                                                  # some chargers have separate obs for
                                                                  # each port instead of one obs for all
                                                                  # ports that summarizes how many of
                                                                  # each level of port (1, 2, DCFC)
                                                                  # is at the charger's location).
multiple <- comparing %>%
  filter(diff=="Y") %>%
  select(facility_id)
repeats <- gas_complete %>%
  filter(facility_id%in%multiple$facility_id)
consolidated <- repeats %>% 
  group_by(facility_id, tank_id) %>% 
  summarise(facility_id=first(facility_id), name=first(name), address=first(address), city=first(city), 
            county=first(county), state=first(state), zip=first(zip), lat=first(lat), lon=first(lon), 
            open_usts=first(open_usts), closed_usts=first(closed_usts), facility_status=first(facility_status),
            tank_id=first(tank_id), tank_status=first(tank_status), capacity=first(capacity), 
            install_date=first(install_date), install_month=first(install_month), 
            install_day=first(install_day), install_year=first(install_year), removal_date=first(removal_date),
            remove_month=first(remove_month), remove_day=first(remove_day), remove_year=first(remove_year),
            no_install_date=first(no_install_date), tank_removed=first(tank_removed),
            msa_name2020=first(msa_name2020), csa_id2020=first(csa_id2020), countyfips=first(countyfips), 
            tractgeoid2020=first(tractgeoid2020), tract2020=first(tract2020), msa_name2010=first(msa_name2010),
            csa_id2010=first(csa_id2010), tractgeoid2010=first(tractgeoid2010), tract2010=first(tract2010), 
            id=first(id), station_name=first(station_name), status_code=first(status_code),
            n_level1=sum(n_level1, na.rm=TRUE), n_level2=sum(n_level2, na.rm=TRUE), 
            n_dcfast=sum(n_dcfast, na.rm=TRUE), totchargers=sum(totchargers, na.rm=TRUE),
            ev_network=first(ev_network), open_date=first(open_date), facility_type=first(facility_type),
            ev_pricing=first(ev_pricing), evse_open_year=first(evse_open_year), 
            evse_open_month=first(evse_open_month), evse_open_day=first(evse_open_day),
            groups_with_access_code=first(groups_with_access_code), access_days_time=first(access_days_time), 
            cards_accepted=first(cards_accepted), ev_other_info=first(ev_other_info), 
            ev_network_web=first(ev_network_web), owner_type_code=first(owner_type_code),
            federal_agency_id=first(federal_agency_id), federal_agency_name=first(federal_agency_name),
            federal_agency_code=first(federal_agency_code), access_code=first(access_code),
            access_detail_code=first(access_detail_code), restricted_access=first(restricted_access),
            ev_workplace_charging=first(ev_workplace_charging), tesla=first(tesla), l1_date=first(l1_date),
            l2_date=first(l2_date), l3ntchademo_date=first(l3ntchademo_date), l3ntccs_date=first(l3ntccs_date),
            l3tesla_date=first(l3tesla_date), level1=first(level1), level2=first(level2), level3=first(level3), 
            mi_to_nb=first(mi_to_nb)) %>%
  ungroup()
consolidated <- as.data.frame(consolidated)
# Now removing all observations of stations who matched to multiple chargers since those chargers'
# had separate obs for each port instead of one obs summarizing all ports at the charger's location
gas_complete <- gas_complete[!gas_complete$facility_id%in%multiple$facility_id,]
# Adding stations who matched to multiple chargers back in with now-consolidated charger data
gas_complete <- rbind(gas_complete, consolidated)

rm(ogdata,newdata,comparing,multiple,repeats)

## Saving merged dataset
setwd("H:/My Drive/Research/EV Charging/Data/Cleaned Data")
write.csv(gas_complete, file="gas_complete.csv", row.names=FALSE)

## Clearing the environment
rm(list=ls())
