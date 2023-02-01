**********************************************************************
***** Program: regression_tables.do
***** By: Greg Casey Stephie Fried, Ethan Goode
***** Date: Spring 2022
***** Purpose: Bootstrap regressions and export results to csv
**********************************************************************
clear
set type double
set matsize 800

* Set own directory
loc cdir "/path/to/project"

cd "`cdir'"

*subdirs
loc datadir "`cdir'/data/output"
loc tabledir "`cdir'/tables"

*Set number of iterations
loc bootend = 1000

********************************************************************************
**** Bootstrap TFP (level effects)
****	
********************************************************************************

*Import TFP data frame from R
use "`datadir'/df_tfp.dta", clear

*Set time and year dimensions
egen countryid = group(countryname)
xtset countryid year
encode(countryname), gen(cname)

*gen lag variables (and name them)
forvalues i = 1/4{
g lag`i'_delta_log_tfp = L`i'.delta_log_tfp
label var lag`i'_delta_log_tfp "\$\Delta Ln TFP_{t-`i'} $"
g lag`i'_delta_log_gdppc = L`i'.delta_log_gdppc
label var lag`i'_delta_log_gdppc "\$ \Delta Ln GDPPC_{t-`i'} $"
g lag`i'_temp = L`i'.temp
label var lag`i'_temp "\$Temp_{t-`i'}$"
g lag`i'_temp2 = L`i'.temp2
label var lag`i'_temp2 "\$Temp^2_{t-`i'}$"
}

*Set sample
set seed 8675309

ivreg2 delta_log_tfp  lag1_delta_log_tfp  temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year,  bw(2) ro nocons
gen samp3 = e(sample)
egen max_samp3 = max(samp3), by(countryname)
keep if max_samp3 == 1

ivreg2 delta_log_tfp  lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year,  bw(2) ro nocons

*Perform boot strapping
forvalues nn = 1/`bootend' {
preserve

bsample, cl(cname)

egen country_count = count(cname), by(cname)
replace country_count = country_count/70

export delimited cname country_count using "`datadir'/bootstrap_files/bootstrap_tfp_le_countryCount`nn'.csv", replace

ivreg2 delta_log_tfp lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year, ro nocons

*Store result matrix
matrix b = get(_b)
matrix list b
matrix c = b'
svmat double c, name(bootmat)

*Store names
local names : colnames e(b)

local enum = 1
g name = ""
foreach name in `names' {
	replace name = "`name'" in `enum'
	local enum = `enum' + 1
}

g index = `nn'
keep bootmat name index

tempfile t`nn'
save `t`nn'', replace

restore
}

use `t1', clear
forvalue i = 2/`bootend' {
append using `t`i''
}

export delimited using "`datadir'/bootstrap_files/bootstrap_tfp_le.csv", replace
