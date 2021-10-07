clear all

	
 * Ensure graph windows are defaults
	graph set window fontface default
	graph set window fontfacemono default
	graph set window fontfacesans default
	graph set window fontfaceserif default
	graph set window fontfacesymbol default


cd "~/Dropbox/india_pesa_forests/inp/Jharkhand Minerals/Data/"

insheet using "jharkhand_blocks.csv", names clear

expand 8
bysort nameb_index: gen year = _n+2005

preserve
insheet using "Master_Data_v2.csv", names clear
	
*local var = "appl_date"
*local var = "leas_deed_date_of_exec"
*local var = "mini_plan_appl_date"
*local var = "mini_plan_appr_date"
local var = "mini_plan_appr_date"
split `var', parse(`var', " ")
drop `var'
destring `var'3, replace

rename `var'2 month
rename `var'3 year
drop if missing(year)

drop if year > 2013
drop if year ==2015 & inlist(month,"Oct","Nov","Dec")
drop if year <2006

keep if ~missing(appl_deta_date) //keeping only those leases that had to get forest clearance 

drop month

replace appl_deta_area = "" if appl_deta_area=="--"
destring appl_deta_area, replace
drop if appl_deta_area==388550 
gen mine_forest = (appl_deta_area>0) & ~missing(appl_deta_area)
*keep if   mine_forest==1
	
	tempfile leases
	save `leases'

restore

merge 1:m nameb_index year using `leases'
keep if _merge !=2
gen count = (_merge==3)
drop _merge


// collapsing to block year level
collapse (first) sch (sum) count appl_deta_area, by(named nameb_index year)

tsset nameb_index year
tsfill, full

gen post = (year>=2010)
gen schpost = sch*post

bys nameb_index: gen t = _n

sort nameb_index year
bys nameb_index: gen cu_count = count[_n]
bys nameb_index: replace cu_count = cu_count[_n] + cu_count[_n-1] if _n > 1

gen mine = (count>0)
sort nameb_index year
bys nameb_index: gen cu_mine = count[_n]
bys nameb_index: replace cu_mine = cu_mine[_n] + cu_mine[_n-1] if _n > 1

replace appl_deta_area = 0 if missing(appl_deta_area)
gen lminearea = ln(appl_deta_area+1)

bys nameb_index: gen cu_area = appl_deta_area[_n]
bys nameb_index: replace cu_area = cu_area[_n] + cu_area[_n-1] if _n > 1
gen lcu_area = ln(cu_area+1)

stop
*Figure

binscatter cu_mine year, by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-)) ytitle("A mine is approved in the block") name(mine, replace) legend(rows(1) label(1 "Non-Scheduled Areas") label(2 "Scheduled Areas")) scheme(plotplainblind)
binscatter lcu_area year , by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-))  ytitle("Total forest clearance allowed in Hectares")  name(area, replace) scheme(plotplainblind)

grc1leg mine area, scheme(plotplainblind)
graph export "licenses.pdf", replace

*Table
est clear
eststo: reghdfe cu_mine schpost post sch, absorb(year nameb_index) cluster(nameb_index)
	sum cu_mine if post == 0 & sch==0
	local ymean = `r(mean)'
	estadd local ymean = round(`r(mean)', 0.001) 
	estadd local yearfe = "Yes"
	estadd local blockfe = "Yes"
eststo: reghdfe lcu_area schpost post sch, absorb(year nameb_index) cluster(nameb_index)
	sum cu_mine if post == 0 & sch==0
	estadd local ymean = round(`r(mean)', 0.001) 
	estadd local yearfe = "Yes"
	estadd local blockfe = "Yes"

		
esttab using licenses.tex, replace booktabs label b se ///
			nomtitles noomitted ///
			keep(schpost) ///
			coeflabels(schpost "Scheduled Area X Post") ///
			stats(yearfe blockfe ymean N_clust N, labels("Year FE" "Block FE" "Dep. Var Mean" "\# Blocks" "\# Observations") ///
			fmt(%9.3f %9.3f %9.3f %9.0f %9.0f)) ///
			starlevels(* .1 ** .05 *** .01) substitute(_ \_) /// 
			title("Mining licensses in Jharkhand") ///
			prehead(	"\begin{table}[htbp]\centering"	///
						"%\addtocounter{table}{-1}" ///
						"%\renewcommand{\thetable}{\arabic{table}a}" ///
						"%\renewcommand{\theHtable}{\thetable B}% To keep hyperref happy" ///
						"\scalebox{0.9}{"	///
						"\begin{threeparttable}[b]" ///
						"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"	///
						"\caption{@title}"	///
						"\begin{tabular}{l*{@span}{c}}"	///
						"\toprule" ) ///
			postfoot(	"\bottomrule"	///
						"\end{tabular}"	///
						"Standard errors are clustered at the block level." ///
						"\\ \emph{Levels of significance}: *\$ p<0.1\$ , **\$ p<0.05\$ , ***\$ p<0.01\$ " ///
						"%\begin{tablenotes}"	///
						"%\item [1] " ///
						"%\end{tablenotes}"	///				
						"\end{threeparttable}"	///
						"}"				///	
						"\end{table}"	) ///
			mlabel("Block has mine" "Allowed Forest clearance (HA)")



/*
binscatter cu_count year , by(sch) linetype(none) absorb(nameb_index)  xline(2009.5, lcolor(gs10) lpattern(-))
binscatter cu_count year , by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-))

binscatter cu_mine year, by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-))
binscatter cu_mine year, by(sch) linetype(none) absorb(nameb_index)  xline(2009.5, lcolor(gs10) lpattern(-))

binscatter lcu_area year , by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-)) 
binscatter lcu_area year , by(sch) linetype(none) absorb(nameb_index)  xline(2009.5, lcolor(gs10) lpattern(-)) 

binscatter mine year, by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-))
binscatter mine year, by(sch) linetype(none) absorb(nameb_index)  xline(2009.5, lcolor(gs10) lpattern(-))


reghdfe cu_count schpost post sch, absorb(year nameb_index) cluster(nameb_index)
reghdfe cu_count schpost post sch, absorb(year nameb_index i.nameb_index#c.t) cluster(nameb_index)
reghdfe cu_mine schpost post sch, absorb(year nameb_index) cluster(nameb_index)
reghdfe cu_mine schpost post sch, absorb(year nameb_index i.nameb_index#c.t) cluster(nameb_index)
reghdfe lcu_area schpost post sch, absorb(year nameb_index) cluster(nameb_index)
reghdfe lcu_area schpost post sch, absorb(year nameb_index i.nameb_index#c.t) cluster(nameb_index)
reghdfe mine schpost post sch, absorb(year nameb_index) cluster(nameb_index)
reghdfe mine schpost post sch, absorb(year nameb_index i.nameb_index#c.t) cluster(nameb_index)


stops
binscatter count year, by(sch) linetype(none) absorb(nameb_index)  xline(2009.5, lcolor(gs10) lpattern(-))
binscatter count year, by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-)) 

reghdfe count schpost post sch, absorb(year nameb_index) cluster(nameb_index)
reghdfe count schpost post sch, absorb(year nameb_index i.nameb_index#c.t) cluster(nameb_index)

binscatter lminearea year , by(sch) linetype(none)  xline(2009.5, lcolor(gs10) lpattern(-)) 
binscatter lminearea year , by(sch) linetype(none) absorb(nameb_index)  xline(2009.5, lcolor(gs10) lpattern(-)) 
reghdfe lminearea schpost post sch, absorb(year nameb_index) cluster(nameb_index)
reghdfe lminearea schpost post sch, absorb(year nameb_index i.nameb_index#c.t) cluster(nameb_index)

