**********************************************************************
***** Program: regression_tables_LP.do
***** By: Greg Casey Stephie Fried, Ethan Goode
***** Date: 1/13/2023
**********************************************************************

clear
set type double
set matsize 800

*Set own dir
loc cdir = "/path/to/project"
cd "`cdir'"

*Choose maximum forecast horizon for local projections
loc horizons = 20 // 20-year horizon for LP
loc temps = "8 13 18"

*subdirs
loc datadir "`cdir'/data/output"
loc tempfiledir "`cdir'/data/tempfiles"
loc tabledir "`cdir'/results/tables"

*Import TFP data frame from R
use "`datadir'/df_tfpv2.dta"

*Set time and year dimensions
egen countryid = group(countryname)
xtset countryid year
encode(countryname), gen(cname)

*gen lag variables (and name them)
forvalues i = 1/10 {
g lag`i'_delta_log_tfp = L`i'.delta_log_tfp
label var lag`i'_delta_log_tfp "\$\Delta Ln TFP_{t-`i'}: \rho $"
g lag`i'_delta_log_gdppc = L`i'.delta_log_gdppc
label var lag`i'_delta_log_gdppc "\$ \Delta Ln GDPPC_{t-`i'}: \rho $"
g lag`i'_temp = L`i'.temp
label var lag`i'_temp "\$Temp_{t-`i'}: \eta_`i'$"
g lag`i'_temp2 = L`i'.temp2
label var lag`i'_temp2 "\$Temp^2_{t-`i'}: \nu_`i'$"
}

** Make LHS variable
forvalues h=0/`horizons' {
g dlog_tfp`h'= (F`h'.delta_log_tfp) - (L1.delta_log_tfp) 
g dlog_gdppc`h' = (F`h'.delta_log_gdppc) - (L1.delta_log_gdppc)
}

*Set sample
ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year,  bw(2) ro nocons
gen samp1 = e(sample)

********************************************************************************
* IRF of TFP
********************************************************************************
foreach T in `temps' {
loc delta_t2 = `T'^2 - (`T'-1)^2
preserve
forvalues h = 0/`horizons' {
	
	ivreg2 dlog_tfp`h' lag1_delta_log_tfp dtemp dtemp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
	
	lincom dtemp + (dtemp2*`delta_t2')
	
	g beta`h' = r(estimate)
	g se`h' = r(se)
	
}


g id = 1
collapse beta* se*, by(id)
reshape long beta se, i(id) j(horizon)

export delimited "`datadir'/tfp`T'_irf_lp.csv", replace
restore
}





