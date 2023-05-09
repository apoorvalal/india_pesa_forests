
**********************
* LAND CONFLICT WATCH
**********************
cd "~/dropbox/india_pesa_forests"

* loading pixel data
insheet using "./tmp/vcf_reg_ready.csv", clear


*keeping year 1995-2017
keep if year>=1995
keep if year<=2017
gen t = year-1995

/*
*keep greater than median cover
sum cover_1990, de
keep if cover_1990>`r(p50)'

*checking if I can replicate the findings in the paper with these data
	*reghdfe forest_index c.sch#c.d, absorb(cellid styear i.cellid#c.t) cluster(blk)
*/
* Storing info on all cells
preserve
	keep state cellid x y
	duplicates drop cellid, force
	tempfile cells
	save `cells'
restore

preserve
	* Loading Land Conflict Watch Data
	import excel "./inp/land_conflict_watch/data_20221111.xlsx", sheet("Sheet2") firstrow clear
		
		/*
		1. add variable on whether conflict is on-going
		*/
		
	* cleaning LCW data
	gen con_id = _n
	drop TitleofConflict UploadSourceforLandArea URLtoInformationSourceforLa SummaryofConflict
	rename Latitude con_y
	rename Longitude con_x
	rename StartingYearofConflict year
	rename StateUnionTerritory state
	rename District district
	rename VillageTown con_village
	rename AdditionalLocations con_locations_more
	rename ReasonCauseofConflict con_cause
	rename SectorfromReasonCauseofCon con_sector
	rename LandAreainHectares con_area_ha

	order con_id year con_x con_y

	replace con_sector = "Forestry" if con_sector == "Conservation and Forestry"
	replace con_sector = "LandUse" if con_sector == "Land Use"
	replace state = "Orissa" if state=="Odisha"
		
	*keeping year 1995
	keep if year>=1995
	drop if missing(year) // 20 dropped
	
	tempfile lcw
	save `lcw'
restore


* calculate distances
preserve
	use `cells', clear
	joinby state using `lcw' // creating all pairwise combinations within state


	geodist y x con_y con_x, gen(distance) miles
	replace distance = distance * 1.609344 // converting to KM
	
	* assign conflicts to cells.. assuming 50km assignment
	forvalues i = 30(10)60 {		
		*gen con_Overall`i' = (distance<=`i')
		*gen con_Forestry`i' = (distance<=`i' & con_sector == "Forestry") 
		*gen con_Industry`i' = (distance<=`i' & con_sector == "Industry") 
		*gen con_Infrastructure`i' = (distance<=`i' & con_sector == "Infrastructure") 
		*gen con_LandUse`i' = (distance<=`i' & con_sector == "LandUse") 
		gen con_Mining`i' = (distance<=`i' & con_sector == "Mining") 
		*gen con_Power`i' = (distance<=`i' & con_sector == "Power") 		
		}
		

		
	* collapsing to cellid level
	*collapse (max) con_Overall* con_Forestry* con_Industry* con_Infrastructure* con_LandUse* con_Mining* con_Power*, by(cellid year)
	collapse (max) con_Mining*, by(cellid year)

	tempfile celltoconflict
	save `celltoconflict'
restore

merge 1:1 cellid year using `celltoconflict'
	drop if _merge ==2 // need to expland panel in core data if want to keep beyond 2017
	drop _merge

	
* Analysis
	* ex-ante forest cover
	sum cover_1990, de
	gen forest_cover = (cover_1990>`r(p50)')
	
	* coding variables
	forvalues i = 30(10)60 {	
		*foreach sector in Overall Forestry Industry Infrastructure LandUse Mining Power {
		foreach sector in Mining {
			recode con_`sector'`i' (.=0)
			}
		}
	
	* saving the data
	preserve
	*keep cellid year con_Overall30 con_Mining30 con_Forestry30 con_LandUse30 con_Industry30 con_Infrastructure30 con_Power30
	keep cellid year con_Mining30 con_Mining40 con_Mining50 con_Mining60
	outsheet using "./inp/land_conflict_watch/cleaned_data.csv", names replace comma
	restore

	
forvalues i = 30(10)60 {		
bys cellid year: egen cMining`i' = max(con_Mining`i')	
by cellid: replace cMining`i' = cMining`i'[_n-1] if cMining`i'[_n-1]==1
replace con_Mining`i' = cMining`i'
}
	

* running regressions - appendix
	est clear
		foreach sector in Mining {		
			eststo: reghdfe con_`sector'50  c.sch#c.d, absorb(cellid styear i.cellid#c.t) cluster(blk)
				sum con_`sector'50 if sch == 0
				estadd scalar nschmean = `r(mean)'
				sum con_`sector'50 if sch == 1
				estadd scalar schmean = `r(mean)'
				
			eststo: reghdfe con_`sector'50  c.sch#c.d if forest_cover==1, absorb(cellid styear i.cellid#c.t) cluster(blk)
				sum con_`sector'50 if sch == 0 & forest_cover==1
				estadd scalar nschmean = `r(mean)'
				sum con_`sector'50 if sch == 1 & forest_cover==1
				estadd scalar schmean = `r(mean)'			
			}

		
	estout est1 est2 using "out_sg/LCW_mining.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d "PESA X Scheduled") ///
		mlabels(none) collabels(none) 
		

* event study 

	gen years_since_PESA = year - first_pesa_exposure

	gen iyrPESA1 = (inlist(years_since_PESA,-4))
	gen iyrPESA2 = (inlist(years_since_PESA,-3))
	gen iyrPESA3 = (inlist(years_since_PESA,-2))
	gen iyrPESA4 = (inlist(years_since_PESA,-1))
	gen iyrPESA5 = (inlist(years_since_PESA,0))
	gen iyrPESA6 = (inlist(years_since_PESA,1))
	gen iyrPESA7 = (inlist(years_since_PESA,2))
	gen iyrPESA8 = (inlist(years_since_PESA,3))
	gen iyrPESA9 = (inlist(years_since_PESA,4))
	
reghdfe con_Mining50  c.sch#c.iyrPESA1 c.sch#c.iyrPESA2 c.sch#c.iyrPESA3 c.sch#c.iyrPESA5 c.sch#c.iyrPESA6 c.sch#c.iyrPESA7 c.sch#c.iyrPESA8 c.sch#c.iyrPESA9 sch if forest_cover==1 , absorb(cellid styear i.cellid#c.t) cluster(blk)

	mat A=r(table)
	mat A = A'
	mat b = A[1..8,1]
	mat ll = A[1..8,5]
	mat ul = A[1..8,6]
	
	svmat b
	svmat ll
	svmat ul

	replace b1 = 0 in 9
	replace ll = 0 in 9
	replace ul = 0 in 9
	
	gen time = -4 in 1
	replace time = -3 in 2
	replace time = -2 in 3
	replace time = -1 in 9
	replace time = 0 in 4
	replace time = 1 in 5
	replace time = 2 in 6
	replace time = 3 in 7
	replace time = 4 in 8
	
	graph twoway (scatter b1 time, mcolor(black)) ///
		(rspike ul ll time, color(black)), ///
		scheme(s1mono) legend(off) yline(0, lcolor(gs8)) xline(-1, lpattern(-) lcolor(gs12)) ///
		xtitle(Years since PESA) ytitle(Treatment Effect) ///
		aspectratio(0.4)
		
		
	graph export "out_sg/mining_eventstudy.pdf", replace
	
* running regressions - appendix
	est clear
	forvalues i = 30(10)50 {
		foreach sector in Mining {		
			eststo: reghdfe con_`sector'`i'  c.sch#c.d if forest_cover==1, absorb(cellid styear i.cellid#c.t) cluster(blk)
				sum con_`sector'`i' if sch == 0 & forest_cover==1
				estadd scalar nschmean = `r(mean)'
				sum con_`sector'`i' if sch == 1 & forest_cover==1
				estadd scalar schmean = `r(mean)'
			
			}
		}	
		
	estout est1 est2 est3  using "out_sg/LCW_mining_appendix.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d "PESA X Scheduled") ///
		mlabels(none) collabels(none) 

		
	/*	
**********
** GDELT
**********

insheet using "~/Dropbox/india_pesa_forests/inp/gdelt/GDELT/protests_gdelt.csv", clear

********************
** gen scheduled status for each district (gdelt data is at district level)
********************
keep if inlist(state, "Orissa", "Andhra Pradesh", "Jharkhand", "Gujarat", "Maharashtra", "Chhattisgarh", "Madhya Pradesh", "Rajasthan", "Himachal Pradesh")

gen sch=.
replace district = lower(district)

*orissa
replace sch=(inlist(district,"mayurbhanj", "sundargarh", "koraput") & state=="Orissa")
replace sch=(inlist(district,"sambalpur", "kendujhar","baudh") & state=="Orissa")
replace sch=(inlist(district,"kandhamal","kalahandi","baleshwar") & state=="Orissa")
replace sch=(inlist(district,"gajapati","ganjam") & state=="Orissa")

*andhra pradesh
replace sch=(inlist(district,"visakhapatnam", "east godavari", "adilabad", "warangal", "khammam") & state =="Andhra Pradesh")

*jharkhand
replace sch=(inlist(district,"ranchi", "lohardaga", "gumla", "purbi singhbhum") & state =="Jharkhand")
replace sch=(inlist(district,"pashchimi singhbhum","sahibganj", "pakaur") & state =="Jharkhand")
replace sch=(inlist(district,"dumka","gumla", "palamu", "godda") & state =="Jharkhand")
replace sch=(inlist(district,"dumka", "garhwa") & state =="Jharkhand")

*Gujarat
replace sch=(inlist(district,"the dangs", "bharuch", "dohad", "narmada", "panch mahals") & state =="Gujarat")
replace sch=(inlist(district,"surat", "vadodara", "valsad", "navsari") & state =="Gujarat")

*Maharashtra
replace sch=(inlist(district,"thane","nashik", "dhule", "nandurbar") & state =="Maharashtra")
replace sch=(inlist(district,"jalgaon","pune", "nanded", "yavatmal") & state =="Maharashtra")
replace sch=(inlist(district,"gadchiroli","chandrapur", "ahmadnagar") & state =="Maharashtra")

*Chhattisgarh
replace sch=(inlist(district,"bastar","koriya", "surguja", "kanker") & state =="Chhattisgarh")
replace sch=(inlist(district,"dantewada","korba", "jashpur", "raigarh") & state =="Chhattisgarh")
replace sch=(inlist(district,"bilaspur","durg", "rajnandgaon", "raipur", "dhamtari") & state =="Chhattisgarh")

*Madhya Pradesh
replace sch=(inlist(district,"jhabua","mandla", "shahdol", "dhar", "barwani") & state =="Madhya Pradesh")
replace sch=(inlist(district,"west nimar","east nimar", "ratlam", "betul", "seoni") & state =="Madhya Pradesh")
replace sch=(inlist(district,"balaghat","hoshangabad", "sidhi", "sheopur", "chhindwara") & state =="Madhya Pradesh")

*Rajasthan
replace sch=(inlist(district,"banswara","dungarpur", "udaipur") & state =="Rajasthan")

*Himachal Pradesh
replace sch=(inlist(district,"lahul and spiti","kinnaur", "chamba") & state =="Himachal Pradesh")

********************
** gen post
********************
gen pesa_year = .
replace pesa_year = 2001 if state=="Andhra Pradesh"
replace pesa_year = 2005 if state=="Chhattisgarh"
replace pesa_year = 2001 if state=="Gujarat"
replace pesa_year = 2010 if state=="Jharkhand"
replace pesa_year = 2007 if state=="Maharashtra"
replace pesa_year = 2002 if state=="Orissa"
replace pesa_year = 2000 if state=="Madhya Pradesh"
replace pesa_year = 2000 if state=="Himachal Pradesh"
replace pesa_year = 2000 if state=="Rajasthan"


split monthyear, parse("m")
rename monthyear1 year
rename monthyear2 month
destring year, replace
destring month, replace

gen pesa_exposure = (year>=pesa_year)


********************
** collapsing data at yearly level and generating vars of interest
********************
gen t = year - 1995

encode state, gen(state_id)

collapse (rawsum) protests nummentions numsources numarticles (firstnm) t sch state_id pesa_exposure, by(district_id year)

gen protest_any = (protests>0)

********************
** regressions
********************

reghdfe protests i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year i.district_id##c.t) cluster(district_id)
reghdfe protest_any i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year i.district_id##c.t) cluster(district_id)


reghdfe protests i.sch##i.pesa_exposure , absorb(i.district_id i.year) cluster(district_id)
reghdfe protests i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year) cluster(district_id)
reghdfe protests i.sch##i.pesa_exposure, absorb(i.district_id##c.t i.state_id##i.year) cluster(district_id)
reghdfe protests i.sch##i.pesa_exposure, absorb(i.district_id##c.t i.year) cluster(district_id)

reghdfe protest_any i.sch##i.pesa_exposure , absorb(i.district_id i.year) cluster(district_id)
reghdfe protest_any i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year) cluster(district_id)
reghdfe protest_any i.sch##i.pesa_exposure, absorb(i.district_id##c.t i.state_id##i.year) cluster(district_id)
reghdfe protest_any i.sch##i.pesa_exposure, absorb(i.district_id##c.t i.year) cluster(district_id)


reghdfe nummentions i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year i.district_id##c.t) cluster(district_id)
reghdfe numsources i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year i.district_id##c.t) cluster(district_id)
reghdfe numarticles i.sch##i.pesa_exposure, absorb(i.district_id i.state_id##i.year i.district_id##c.t) cluster(district_id)

reghdfe cprotests_any i.sch##i.pesa_exposure, absorb(i.district_id##c.t i.state_id##i.year) cluster(district_id)


sort district_id year
bysort district_id: gen cprotests = protests[_n] + protests[_n-1]
gen cprotests_any = (cprotests>0)

reghdfe protest_any c.sch#c.t, absorb(i.district i.state_id##i.year) cluster(district_id)		
		
		
		
		
		
		
		
		
		
		
		
		
		
	
/*	
	* running regressions
	est clear
	forvalues i = 10(10)50 {
		foreach sector in Overall Mining Forestry LandUse Industry Infrastructure Power {		
			eststo: reghdfe con_`sector'`i'  c.sch#c.d if forest_cover==1, absorb(cellid styear i.cellid#c.t) cluster(blk)
				sum con_`sector'`i' if sch == 0
				estadd scalar nschmean = `r(mean)'
				sum con_`sector'`i' if sch == 1
				estadd scalar schmean = `r(mean)'
			
			}
		}	
	
	
	* main body table at 30km
	estout est15 est16 est17 est18 est19 est20 est21 using "out_sg/LCW.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d "PESA X Scheduled") ///
		mlabels(none) collabels(none) extracols(2)


	* appendix table with robustness for 10, 20, 40, 50kms
	estout est1 est2 est3 est4 est5 est6 est7 using "out_sg/LCW_appendixA.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations ") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d "\midrule \textbf{Panel A: 10km} \\ \addlinespace PESA X Scheduled") ///
		mlabels(none) collabels(none) extracols(2)
		
	estout est8 est9 est10 est11 est12 est13 est14 using "out_sg/LCW_appendixB.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations ") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d "\midrule \textbf{Panel B: 20km} \\ \addlinespace PESA X Scheduled") ///
		mlabels(none) collabels(none) extracols(2)
		
		
	estout est22 est23 est24 est25 est26 est27 est28 using "out_sg/LCW_appendixC.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations ") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d " \midrule \textbf{Panel C: 40km} \\ \addlinespace PESA X Scheduled") ///
		mlabels(none) collabels(none) extracols(2)
		
		
	estout est29 est30 est31 est32 est33 est34 est35 using "out_sg/LCW_appendixD.tex", ///
		replace cells(b (fmt(%9.4f)) se(par fmt(%9.4f))) style(tex) ///
		stats(nschmean schmean N, layout(@ @ @) ///
		label("\addlinespace Mean Pre-Y (Non-Sch)" "Mean Pre-Y (Sched)" "\# Observations") ///
		fmt(%9.4f %9.4f %9.0f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
		starlevels(* .1 ** .05 *** .01) ///
		keep(c.sch#c.d) order() ///
		varlabels(c.sch#c.d " \midrule \textbf{Panel D: 50km} \\ \addlinespace PESA X Scheduled") ///
		mlabels(none) collabels(none) extracols(2)		
		
	
/*	
**** 
gen years_since_PESA = year - first_pesa_exposure
	binscatter con_Overall30  years_since_PESA if forest_cover==1 & abs(years_since_PESA)<=5, by(sch) absorb(cellid) rd(0)

foreach var in Overall Mining Forestry LandUse Industry Infrastructure Power {
capture drop c`var'30	
bys cellid year: egen c`var'30 = max(con_`var'30)	
by cellid: replace c`var'30 = c`var'[_n-1] if c`var'30[_n-1]==1
reghdfe c`var'30  c.sch#c.d if forest_cover==1, absorb(cellid styear i.cellid#c.t) cluster(blk)	
	}
	
foreach var in Overall Mining Forestry LandUse Industry Infrastructure Power {
binscatter c`var'30  years_since_PESA if forest_cover==1 & abs(years_since_PESA)<=5, by(sch) absorb(cellid) rd(0)
}

bysort cellid year: 

	
/*

