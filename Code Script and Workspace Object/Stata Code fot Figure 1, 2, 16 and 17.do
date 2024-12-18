*********************************************
*** Asset Pricing Group Assignment ***
*********************************************

clear all
 
cd "/Users/siyuanqi/Desktop/AP/APGA"

****************************************************************************************************************************************************************************************
********************************************************************************************

/*import datasets and merge */

import excel "/Users/siyuanqi/Desktop/AP/IB9Y8-2024-Group-Project-DATA-.xlsx", sheet("Portfolio Returns") firstrow

save "PortfolioReturns.dta", replace

import excel "/Users/siyuanqi/Desktop/AP/IB9Y8-2024-Group-Project-DATA-.xlsx", sheet("Corporate Bond Risk Factors") firstrow clear

save "RiskFactors.dta", replace

use "PortfolioReturns.dta", clear
merge 1:1 Date using "RiskFactors.dta"

tab _merge
drop _merge

/*generate monthly font time vairable */

gen Mon = mofd(Date)
format Mon %tm 
tsset Mon

save "PRRF.dta", replace

/*summray and descriptive statistics */

de
su
asdoc su, save(su.doc) replace
asdoc de, save(de.doc) replace

/*graph illustrations of all variables */

tsline Portfolio1 Portfolio2 Portfolio3 Portfolio4 Portfolio6 Portfolio8 Portfolio9 Portfolio10 Portfolio12 Portfolio13 Portfolio14 Portfolio16 Portfolio17 Portfolio18 Portfolio19 Portfolio20 Portfolio21 Portfolio22 Portfolio23 Portfolio24 Portfolio25 Portfolio26 Portfolio27 Portfolio28 Portfolio29 Portfolio30 Portfolio31 Portfolio32 Portfolio33 Portfolio34 Portfolio35 Portfolio36 Portfolio37 Portfolio38 Portfolio39 Portfolio40 Portfolio41 Portfolio42 Portfolio43 Portfolio44 Portfolio45 Portfolio46 Portfolio47 Portfolio48 Portfolio49 Portfolio50 Portfolio51 Portfolio52 Portfolio53 Portfolio54
graph export port.pdf, replace

tsline MKTB MKTDB
graph export mkt.pdf, replace
