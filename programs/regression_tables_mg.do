**********************************************************************
***** Program: regression_tables_baseline.do
***** By: Greg Casey Stephie Fried, Ethan Goode
***** Date: 1/13/2023
**********************************************************************
clear
set type double
set matsize 800

*Set own dir
loc cdir = "/path/to/project"
cd "`cdir'"

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
label var lag`i'_delta_log_tfp "\$\Delta Ln TFP_{t-`i'}: \rho_`i' $"
g lag`i'_delta_log_gdppc = L`i'.delta_log_gdppc
label var lag`i'_delta_log_gdppc "\$ \Delta Ln GDPPC_{t-`i'}: \rho $"
g lag`i'_temp = L`i'.temp
label var lag`i'_temp "\$Temp_{t-`i'}: \eta_`i'$"
g lag`i'_temp2 = L`i'.temp2
label var lag`i'_temp2 "\$Temp^2_{t-`i'}: \nu_`i'$"
}

********************************************************************************
* Table B7: Mean Group Estimator Results
********************************************************************************

*Set sample
ivreg2 delta_log_tfp L1.delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 ///
 i.countryid i.year,  bw(2) ro nocons
gen samp1 = e(sample)

*Reset country id on sample from baseline
keep if samp1 == 1
drop countryid
egen countryid = group(countryname)
xtset countryid year

*Count number of countries
su countryid
loc numc = `r(max)' 

*Demean variables
foreach var of varlist delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 {
		su `var' if samp1 == 1
		egen anmn`var' = mean(`var'), by(year)
		g `var'_bar = `var' - anmn`var' // Demeaned variable
}

forvalues i =1/`numc'{
	preserve
	
    *Growth Effects Specification
	ivreg2 delta_log_tfp_bar lag1_delta_log_tfp_bar temp_bar temp2_bar ///
	dprecip_bar dprecip2_bar if countryid == `i',  bw(2) ro nocons
	
	*Store Estimates and SEs
	foreach var of varlist lag1_delta_log_tfp temp_bar temp2_bar dprecip_bar dprecip2_bar {
		g b_ge`var' = _b[`var']
		g se_ge`var' = _se[`var']
	}
	
	*Level Effects Specification
	ivreg2 delta_log_tfp_bar lag1_delta_log_tfp_bar dtemp_bar dtemp2_bar ///
	dprecip_bar dprecip2_bar if countryid == `i',  bw(2) ro nocons
	
	*Store Estimates and SEs
	foreach var of varlist lag1_delta_log_tfp dtemp_bar dtemp2_bar dprecip_bar dprecip2_bar {
		g b_le`var' = _b[`var']
		g se_le`var' = _se[`var']
	}
	
	*Level and Growth Effects Specification
	ivreg2 delta_log_tfp_bar lag1_delta_log_tfp_bar temp_bar temp2_bar dtemp_bar dtemp2_bar ///
	dprecip_bar dprecip2_bar if countryid == `i',  bw(2) ro nocons
	
	*Store Estimates and SEs
	foreach var of varlist lag1_delta_log_tfp temp_bar temp2_bar dtemp_bar dtemp2_bar dprecip_bar dprecip2_bar {
		g b_be`var' = _b[`var']
		g se_be`var' = _se[`var']
	}
	
	keep if countryid == `i'
	collapse (max) b_* se_*, by(countryid countryname)
	
	tempfile mg`i'
	save `mg`i''
	
	restore
}

*Append estimates
use `mg1', clear
forvalues j = 2/`numc' {
	append using `mg`j''
}



foreach regtype in be le ge {

	*Get Mean, Median, Standard Errors
	foreach var in lag1_delta_log_tfp dprecip_bar dprecip2_bar dtemp_bar dtemp2_bar temp_bar temp2_bar  {
	capture {
	*Mean Estimates
	rreg b_`regtype'`var', gen(mnwt_`regtype'`var')
	g mn_`regtype'`var' = _b[_cons] // Recover Beta from robust regression
	g mnse_`regtype'`var' = _se[_cons] // Recover SE from robust regression	
	egen sumwt`regtype'`var' = sum(mnwt_`regtype'`var')
	replace mnwt_`regtype'`var' = mnwt_`regtype'`var'/sumwt`regtype'`var'
	gen tv`regtype'`var'=(se_`regtype'`var')*(mnwt_`regtype'`var'^2)
	egen stv`regtype'`var'=sum(tv`regtype'`var')
	gen rse`regtype'`var'=sqrt(stv`regtype'`var')
	gen coeff`regtype'`var'= _b[_cons]
	gen t_lvl`regtype'`var'=coeff`regtype'`var'/rse`regtype'`var'
	gen se_wt`regtype'`var'=b_`regtype'`var'/t_lvl`regtype'`var'
	
	*Median Estimates
	qreg b_`regtype'`var', q(50)
	g md_`regtype'`var' = _b[_cons]
	}
	}
}

keep mn_* md_* t_lvl* se_wt*

g id = 1

collapse (max) mn_belag1_delta_log_tfp-md_getemp2_bar, by(id)
reshape long mn_le mn_ge mn_be ///
	md_le md_ge md_be ///
	t_lvlle t_lvlge t_lvlbe ///
	se_wtle se_wtge se_wtbe, i(id) j(var) string
	
foreach var of varlist mn_* md_* se_* {
	replace `var' = round(`var',0.00001)
}

tostring mn* md_* se_*, force replace

foreach regtype in be ge le {
	replace se_wt`regtype' = "" if se_wt`regtype' == "."
	replace mn_`regtype' = "" if mn_`regtype' == "."
	replace md_`regtype' = "" if mn_`regtype' == "."

	replace mn_`regtype' = mn_`regtype' + "*" if t_lvl`regtype' >= 1.645 & t_lvl`regtype' < 1.960 & mn_`regtype' != ""
	
	replace mn_`regtype' = mn_`regtype' + "**" if t_lvl`regtype' >= 1.960 & t_lvl`regtype' < 2.576 & mn_`regtype' != ""
	
	replace mn_`regtype' = mn_`regtype' + "***" if t_lvl`regtype' >= 2.576  & mn_`regtype' != ""
	
}

drop t_*

export delimited "`datadir'/mgtable.csv", replace




