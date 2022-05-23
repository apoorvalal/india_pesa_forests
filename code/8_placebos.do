cd "~/dropbox/india_pesa_forests"


** placebo 1
** randomly assigning year of switch

use "tmp/vcf.dta", clear
clear matrix
set seed 20220522

*keeping year 1995-2017
keep if year>=1995
keep if year<=2017
gen t = year-1995

*keep greater than median cover
sum cover_1990, de
keep if cover_1990>`r(p50)'

*checking if I can replicate the findings in the paper with these data
	*reghdfe forest_index c.sch#c.D, absorb(cellid styear i.cellid#c.t) cluster(blk)


* creating vector of random switches
preserve
keep if sch==1
keep state year D
duplicates drop
sort state year D

forvalues i = 1(1)1000 {
	gen rnd = runiform()
	sort state rnd
	by state: gen D`i' = (_n==1)
	sort state year
	by state: replace D`i' = 1 if D`i'[_n-1] == 1
	capture drop rnd
	}

sort state year D
tempfile rndD
save `rndD'
restore

merge m:1 state year using `rndD'
	assert _merge == 3
	drop _merge
	
* running fake regressions
forvalues i = 1(1)1000 {
	reghdfe forest_index c.sch#c.D`i', absorb(cellid styear i.cellid#c.t) cluster(blk)
		mat brnd = nullmat(brnd) \ _b[c.sch#c.D`i']
	}

svmat brnd

* graphing distribution
gen tag = (brnd1 < .3624) if ~missing(brnd1)
sum tag
local label = round(`r(mean)', .01)
local label = label*100
kdensity brnd1, xline(.3624) scheme(s1color) ///
	xtitle(Treatment Effect) title(" ") ///
	text(1 .345  "Est. effect > `label'%", size(small) orientation(vertical))

graph export "tmp/placebo_D.pdf", replace


** placebo 2
** randomly assigning scheduled status
use "tmp/vcf.dta", clear
clear matrix
set seed 20220522

*keeping year 1995-2017
keep if year>=1995
keep if year<=2017
gen t = year-1995

*keep greater than median cover
sum cover_1990, de
keep if cover_1990>`r(p50)'

* creating vector of random sch assignments
preserve
keep state sch cellid
duplicates drop state cellid, force

bys state: egen schfrac = mean(sch)
drop sch

forvalues i = 1(1)1000 {
	gen rnd = runiform()
	sort state rnd
	by state: gen sch`i'=((_n/_N)<=schfrac) 
	capture drop rnd
	}
	
tempfile rndsch
save `rndsch'
restore


merge m:1 state cellid using `rndsch'
	assert _merge == 3
	drop _merge
	
* gen post
preserve
keep if sch==1
keep state year D
duplicates drop
rename D post
tempfile post
save `post'
restore	

merge m:1 state year using `post'
	assert _merge == 3
	drop _merge

* running fake regressions
forvalues i = 1(1)1000 {
	reghdfe forest_index c.sch`i'#c.post, absorb(cellid styear i.cellid#c.t) cluster(blk)
		mat brnd = nullmat(brnd) \ _b[c.sch`i'#c.post]
	}

svmat brnd

* graphing distribution
gen tag = (brnd1 < .3624) if ~missing(brnd1)
sum tag
local label = round(`r(mean)', .01)
local label = label*100
kdensity brnd1, xline(.3624) scheme(s1color) ///
	xtitle(Treatment Effect) title(" ") ///
	text(4 .345  "Est. effect > `label'%", size(small) orientation(vertical)) ///
	xscale(range(-0.75 0.75)) xlabel(-0.5(0.25)0.5)

graph export "tmp/placebo_sch.pdf", replace
