*** PURPOSE: Import cleaned EPA UST station and cleaned DOE charger data files into .dta files
*** LAST EDITED: 12/11/2024

*** Setting the working directory
clear all
cd "H:\My Drive\Research\EV Charging\Data\Cleaned Data"

*** Importing the data and saving it as .dta files
import delimited using "chargerdata.csv", varnames(1) stringc(1 2 3 4 5 7 12 13 14 15 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 45 46 47 48 49) numericc(6 8 9 10 11 16 17 18 19 20 44) bindquote(strict) clear
save "chargerdata.dta", replace

clear all
import delimited using "gasstations.csv", varnames(1) numericcols(7 8 9 10 11 15 17 18 19 21 22 23 24 25) stringcols(1 2 3 4 5 6 12 13 14 16 20) bindquote(strict) clear
save "gasstations.dta", replace