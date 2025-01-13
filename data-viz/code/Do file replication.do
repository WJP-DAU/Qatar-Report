/*=====================================================================================================================================
Project:		Qatar Report
Routine:		Country report 
Author(s):		Natalia Rodriguez (nrodriguez@worldjusticeproject.org)
Dependencies:  	World Justice Project
Creation Date:	January, 2025

Description:
Master dofile for the production of the datapoints in country reports. This do-file will replicate the data created in R

=====================================================================================================================================*/

clear
cls


/*=====================================================================================================================================
					Pre-settings
=====================================================================================================================================*/

*--- Stata Version
version 15

*--- Required packages:
* NONE

*--- Defining paths to SharePoint & your local Git Repo copy:

*------ (a) Natalia Rodriguez:
if (inlist("`c(username)'", "nrodriguez")) {
	global path2SP "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\Data Analytics\6. Country Reports\Qatar-Report"
	global path2GH ""
}

*------ (b) Alex Ponce:
else if (inlist("`c(username)'", "")) {
	global path2SP ""
	global path2GH ""
}

*------ (c) Ana Montoya:
else if (inlist("`c(username)'", "")) {
	global path2SP ""
	global path2GH ""
}

*------ (d) Carlos Toruno:
else if (inlist("`c(username)'", "ctoruno")) {
	global path2SP ""
	global path2GH ""
}

*------ (e) Santiago Pardo:
else if (inlist("`c(username)'", "")) {
	global path2SP ""
	global path2GH ""
}


*------ (f) Any other user: PLEASE INPUT YOUR PERSONAL PATH TO THE SHAREPOINT DIRECTORY:
else {
	global path2SP ""
	global path2GH ""
}

*--- Defining path to Data and DoFiles:
global path2data "${path2SP}/data-viz/data"
global path2dos  ""


/*=====================================================================================================================================
					1. Data Import
=====================================================================================================================================*/

/*
if "`c(username)'"=="nrodriguez" {
use "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\General Population Poll\GPP 2023\Merged Files\Historical Files\Merged.dta" , clear 
} 
else {
} 

keep if country=="North Macedonia"

save "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\Data Analytics\6. Country Reports\North-Macedonia\Data\Dataset for replication.dta", replace
*/


***** Upload data

use "$path2data/Qatar_final_2024.dta"


/*=====================================================================================================================================
					2. Exporting the data
=====================================================================================================================================*/


**                       **
******** SECTION I ********
**                       **

***** Chart 1: Trust

dtable i.q1a i.q1h i.q1c i.q1b i.q1g i.q1d i.q1e i.q1f, nosample column(summary(Chart1)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart1) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart1) cell(A1) replace) replace


***** Chart 2: Corruption

dtable i.q2a i.q2c i.q2b i.q2g i.q2d i.q2e i.q2f, nosample column(summary(Chart2)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart2) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart2) cell(A1) modify) replace 


**                        **
******** SECTION II ********
**                        **

***** Chart 3: Criminal Justice


dtable i.q49a i.q49b_G2 i.q49e_G2 i.q49c_G2 i.EXP_q23d_G1 i.q49b_G1, nosample column(summary(Chart3)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart3) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart3) cell(A1) modify) replace 


***** Chart 4: Police

dtable i.q48c_G2 i.q48b_G2 i.q48a_G2 i.EXP_q22h_G2 i.EXP_q22i_G2, nosample column(summary(Chart4)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart4) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart4) cell(A1) modify) replace 


***** Chart 5: Police 2

gen force=EXP_q22e_G1
recode force (1 =4) (2 =3) (3=2) (4=1)
label values force always
label var force "Do not use excessive force"


dtable i.q48a_G1 i.force i.q48b_G1 i.q48c_G1 i.q48d_G2, nosample column(summary(Chart5)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart5) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart5) cell(A1) modify) replace 


***** Chart 6: Police accountability

gen gangs=EXP_q22k_G2 
gen polit=EXP_q22j_G2

recode gangs polit (1 =4) (2 =3) (3=2) (4=1)
label values gangs agree
label values polit agree
label var gangs "Do not serve the interests of gangs"
label var polit "Do not serve the interests of politicians"


dtable i.EXP_q22f_G1 i.q48d_G1 i.EXP_q22h_G1 i.gangs i.polit, nosample column(summary(Chart6)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart6) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart6) cell(A1) modify) replace 


***** Chart 7: Negative bias in the Police

dtable i.q18f i.q18e i.q18d i.q18c i.q18a, nosample column(summary(Chart7)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart7) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart7) cell(A1) modify) replace 


**                         **
******** SECTION III ********
**                         **


***** Chart 8: Fundamental Freedoms


foreach v in q46a_G2 q46h_G2 q46f_G2 q46e_G2 q46e_G1 q46d_G2 q46d_G1 q46c_G2 q46c_G1 q46b_G2 {
	gen `v'_c=(`v'==1 | `v'==2)
	replace `v'_c=. if `v'==.
}

preserve

collapse (mean) q46a_G2_c q46h_G2_c q46f_G2_c q46e_G2_c q46e_G1_c q46d_G2_c q46d_G1_c q46c_G2_c q46c_G1_c q46b_G2_c , by(country)

label var q46c_G2 "People can express opinions against the government"
label var q46f_G2 "Civil society organizations can express opinions against the government"
label var q46c_G1 "Media can express opinions against the government"
label var q46e_G2 "Media can expose cases of corruption"

label var q46d_G2 "People can attend community meetings"
label var q46a_G2 "People can organize around an issue or petition"

label var q46d_G1 "Local government officials are elected through a clean process"
label var q46e_G1 "People can vote freely without feeling harassed or pressured"
label var q46h_G2 "Religious minorities can observe their holy days"
label var q46b_G2 "Workers can freely bargain for their labor rights"


export excel "${path2SP}\Data replication.xlsx", sheet("chart 8") firstrow(varl) cell(A1)

putexcel set "${path2SP}\Data replication.xlsx", sheet("chart 8") modify
putexcel B2:M4, overwri nformat(percent) 
putexcel A1:M1, overwri bold hcenter txtwrap

restore


***** Chart 9: Discrimination experiences

gen discrim=0
foreach v in q16a q16b q16c q16d q16e {
	replace discrim=1 if `v'<5
}

label values discrim yes

dtable i.discrim, nosample column(summary(Chart9)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart9) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart9) cell(A1) modify) replace 


***** Chart 10: Reasons why people feel discriminated

dtable i.q17_4 i.q17_1 i.q17_11 i.q17_2 i.q17_12 i.q17_3 i.q17_5 i.q17_10 i.q17_13 i.q17_14 i.q17_7, nosample column(summary(Chart10)) factor(, statistics( fvpercent)) nformat(%9.0f) name(chart10) export("${path2SP}\Data replication.xlsx", as(xlsx) sheet(chart10) cell(A1) modify) replace 




