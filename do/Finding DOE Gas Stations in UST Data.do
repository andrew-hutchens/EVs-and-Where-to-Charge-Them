*** PURPOSE: Match gas stations in DOE charger data to their corresponding data in the UST data
*** LAST EDITED: 12/11/2024

clear all
cd "H:\My Drive\Research\EV Charging\Data\Cleaned Data"

import delimited using "chargerdata.csv", varnames(1) bindquote(strict) stringc(2 3 4 5 7 12 13 14 15) numericc(1 6 8 9 10 11 16 17 18 19 20) clear
keep if facility_type=="GAS_STATION"|facility_type=="CONVENIENCE_STORE"|facility_type=="TRUCK_STOP"
drop if id==25527|id==67155|id==118949|id==149909|id==153821|id==167446|id==168276|id==200957|id==200959|id==212751|id==237656|id==238570|id==259986|id==306848|id==307406|id==311129|id==315612|id==52659

geonear id lat lon using "mergedust.dta", neighbors(facility_id lat lon) wide nearcount(1) genstub(nb) miles rep(2)
drop if mi_to_nb>=.06
keep id nb
rename nb facility_id

export delimited using "doegasstations.csv", replace

/* IGNORE BELOW, JUST KEEPING FOR RECORDS; COMPARED SPHERICAL DISTANCE CALCULATION TO ELLIPSOIDAL DISTANCE CALCULATION AND CONFIRMED THAT geonear DID A BETTER JOB OF IDENTIFYING DOE STATIONS' TRUE NEAREST NEIGHBOR (AND DISTANCE TO IT) IN THE UST DATA
browse if id==150281 /* Matched UST facility is CA10138405 w/a distance of 0.01273856 miles using spherical calculation */
/* Matched UST facility is CA10138405 w/a distance of 0.01276177 miles using ellipsoidal calculation */

browse if id==39770 /* Matched UST facility is VA4019121 w/a distance of 0.05295909 miles using spherical calculation */
*/
