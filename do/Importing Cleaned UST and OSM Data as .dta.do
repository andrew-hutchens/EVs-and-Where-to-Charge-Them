*** LAST EDITED: 12/6/2024

*** Setting the working directory and loading the cleaned UST and OSM data
clear all
cd "H:\My Drive\Research\EV Charging\Data\Cleaned Data"

** Importing and saving the OSM data as a .dta file
import delimited using "osm_stations.csv", varnames(1) stringc(3 4 5 6) clear
save "osm_stations.dta", replace

** Importing and saving the UST data as a .dta file
import delimited using "mergedust.csv", varnames(1) stringc(1 2 3 4 5 6 12 13 14 16 20) numericc(7 8 9 10 11 15 17 18 19 21 22 23 24 25) bindquote(strict) clear
save "mergedust.dta", replace
