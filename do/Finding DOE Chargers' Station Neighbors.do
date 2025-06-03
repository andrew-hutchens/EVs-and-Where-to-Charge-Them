*** PURPOSE: Find DOE chargers' nearest neighbor in the EPA UST station data in order to match stations to their charger data (if they've adapted).
*** LAST EDITED: 12/11/2024

*** Setting the working directory
clear all
cd "H:\My Drive\Research\EV Charging\Data\Cleaned Data"

*** Creating dataset of just charger IDs and their coordinates
use "chargerdata.dta", clear

bysort id: keep if _n==1
keep id lon lat

** Checking that each id only appears once
bysort id: gen N=_N
su N /* Only 1 obs for all ids, all good */
as r(mean)==1 & r(min)==1 & r(max)==1
drop N

** Saving dataset of just charger IDs and their coordinates
save "charger locations.dta", replace
export delimited "charger locations.csv", replace

*** Creating dataset of just station IDs and their coordinates
use "gasstations.dta", clear
bysort facility_id: keep if _n==1
keep facility_id lon lat

** Checking that each Facility_ID only appears once
bysort facility_id: gen N=_N
su N /* Only 1 obs for all ids, all good */
as r(mean)==1 & r(min)==1 & r(max)==1
drop N

** Saving dataset of just charger IDs and their coordinates
save "station locations.dta", replace
export delimited "station locations.csv", replace

*** Using geonear to locate each charger's match in the EPA UST station data
use "charger locations.dta", clear
geonear id lat lon using "station locations.dta", neighbors(facility_id lat lon) wide nearcount(1) genstub(nb) miles
rename nb facility_id
*drop if mi_to_nb>=.03
*drop mi_to_nb
save "charger nearest station.dta", replace
export delimited "charger nearest station.csv", replace
* "charger nearest station" now contains each charger obs' nearest obs in the EPA UST station data. Those neighbors that are very, very close to a charger are probably that charger's counterpart in the stations data.
* Keeping all observations and not dropping matches farther than ~0.03 miles since some chargers listed as being at gas stations in the DOE data are not that close to the UST coordinates in the UST data. It appears that the UST coordinates data is not as accurate as the DOE chargers data.
* Will classify all chargers with facility_type=="GAS_STATION" and some of the chargers with facility_type=="CONVENIENCE_STORE" (the ones that are not supermarkets!) as being adapted and then will use the nearest neighbors data created in this code to pick up any other adapted stations not identified in the DOE data (the DOE data has ~46k observations with blanks for facility_type, so the nearest neighbors data created in this code will try to find some gas stations in those ~46k observations based on proximity).

* Note: will have to manually prune remaining non-station observations in 'charger nearest station.csv', since some stations matched to chargers that are not actually located at the station (e.g., station matched to a charger located at a car dealership next door).
