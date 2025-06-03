*** PURPOSE: Find each UST facility's nearest neighbor in the cleaned OSM data
*** LAST EDITED: 12/6/2024

*** Setting the working directory and loading the cleaned UST data
clear all
cd "H:\My Drive\Research\EV Charging\Data\Cleaned Data"
use "mergedust.dta"

count if lon==. & lat!=. /* 0 obs */
count if lat==. & lon!=. /* 0 obs */

** Keeping only first obs of each facility_id; they repeat in mergedust due to having multiple tanks per facility
bysort facility_id: keep if _n==1 
keep facility_id lon lat

** Checking that each facility_id only appears once
bysort facility_id: gen N = _N
sum N /* Only 1 obs for all plant_ids, all good */
drop N

** Saving dataset of plant_ids and their respective longitude and latitude
save "ust facility locations.dta", replace

*** Obtaining unique coordinates in OSM data
** Loading the OSM data
use "osm_stations.dta", clear
count if lon==. & lat!=. /* 0 obs */
count if lat==. & lon!=. /* 0 obs */
* Note: no NA longitudes or latitudes in this dataset either.
egen osm_id = group(lon lat)
bysort osm_id: gen N = _N
browse if N>1 /* Some OSM stations have >1 obs, which we identify by seeing observations with EXACTLY the same coordinates. These repeat obs are addressed in the following line that keeps only the first observation of each coordinate pair. */
drop N

bysort osm_id: keep if _n==1
keep osm_id lon lat

** Checking that each osm_id only appears once
bysort osm_id: gen N = _N
sum N /* Only 1 obs for all osm_ids, all good */
drop N

** Saving dataset of location_id2s and their respective longitude and latitude
save "osm station locations.dta", replace

*** Finding each OSM station's nearest neighbor in the UST data
** Loading the OSM station locations data to use as the base dataset
use "osm station locations.dta", clear

** Finding each station's nearest neighbor
geonear osm_id lat lon using "ust facility locations.dta", neighbors(facility_id lat lon) wide nearcount(1) genstub(nb) miles
rename nb facility_id

** Dropping neighbors that are >=0.06 miles away (found several UST facility neighbors that were indeed gas stations and located far-ish from an OSM station's coordinates; it is the UST coordinates that are sometimes not accurate, while the OSM coordinates are quite accurate)
drop if mi_to_nb>=.06

bysort facility_id: gen N = _N
sum N
sort N
browse if N>1 /* Some UST facilities are the nearest neighbor to multiple OSM stations. The coordinates are what they are, so nothing can be done about this. This is ok though, since we are only using the OSM data to identify gas stations in the UST data. Thus, if a UST facility is the nearest neighbor to multiple OSM gas stations, then at least one of those is the UST facilities true counterpart in the OSM data, thereby confirming the UST facility as a gas station. */
drop N

** Saving the dataset containing each UST facility's nearest neighbor
cd "H:\My Drive\Research\EV Charging\Data\Cleaned Data"
sort osm_id
save "osm stations nearest ust facility.dta", replace
export delimited using "osm stations nearest ust facility.csv", replace
