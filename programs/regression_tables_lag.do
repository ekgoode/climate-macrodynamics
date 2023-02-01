**********************************************************************
***** Program: regression_tables_robustness.do
***** By: Greg Casey Stephie Fried, Ethan Goode
***** Date: 1/13/2023
***** Purpose: Estimates regressions and producs tables for paper
**********************************************************************

clear
set type double
set matsize 800

*Set own dir
loc cdir = "/path/to/project"
cd "`cdir'"

*subdirs
local datadir "`cdir'/data/output"
local tempfiledir "`cdir'/data/tempfiles"
local tabledir "`cdir'/results/tables"

*Import TFP data frame from R
use "`datadir'/df_tfpv2.dta"

*Set time and year dimensions
egen countryid = group(countryname)
xtset countryid year
encode(countryname), gen(cname)

*gen lag variables (and name them)
forvalues i = 1/10 {
gen lag`i'_delta_log_emp = L`i'.delta_log_emp
label var lag`i'_delta_log_emp "\$\Delta Ln L_{t-`i'}: \rho $"
gen lag`i'_delta_log_kpc = L`i'.delta_log_kpc
label var lag`i'_delta_log_kpc "\$ \Delta Ln KPC_{t-`i'}: \rho $"
gen lag`i'_temp = L`i'.temp
g lag`i'_delta_log_tfp = L`i'.delta_log_tfp
label var lag`i'_delta_log_tfp "\$\Delta Ln TFP_{t-`i'}: \rho $"
g lag`i'_delta_log_gdppc = L`i'.delta_log_gdppc
label var lag`i'_delta_log_gdppc "\$ \Delta Ln GDPPC_{t-`i'}: \rho $"
label var lag`i'_temp "\$Temp_{t-`i'}: \eta_`i'$"
gen lag`i'_temp2 = L`i'.temp2
label var lag`i'_temp2 "\$Temp^2_{t-`i'}: \nu_`i'$"
}

*Get interaction of temperature and lag temperature
gen temp_int = temp * lag1_temp
gen temp2_int = temp2 * lag1_temp2

encode(Region), gen(region_factor)

**** Start Regression tables ***************************************************
eststo clear
*** Label vars for tables ******************************************************
label var delta_log_kpc "$\Delta \log KPC: \rho $"
label var delta_log_emp "$\Delta \log L: \rho $"
label var temp "\$Temp.: \gamma_1$"
label var temp2 "\$ Temp.^2: \gamma_2$"
label var dtemp "\$\Delta Temp.: \beta_1$"
label var dtemp2 "\$\Delta Temp.^2: \beta_2$"
label var precip "\$Precip.$"
label var precip2 "\$Precip.^2$"
label var dprecip "\$\Delta Precip.: \xi_1$"
label var dprecip2 "\$\Delta Precip.^2: \xi_2$"
label var temp2_int "\$Temp.^2_{t}$ x \$Temp.^2_{t-1}$"
label var temp_int "\$Temp_{t}$ x \$Temp_{t-1}$"

********************************************************************************
* Table B12: Results with More Lags of Temperature
********************************************************************************
*Set sample
ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 ///
lag1_temp lag1_temp2 ///
lag2_temp lag2_temp2 ///
lag3_temp lag3_temp2 ///
lag4_temp lag4_temp2 ///
lag5_temp lag5_temp2 ///
dprecip dprecip2 i.cname i.year,  bw(2) ro nocons
gen samp1 = e(sample)

*2 lag
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 ///
lag1_temp lag1_temp2 ///
lag2_temp lag2_temp2 dprecip dprecip2 i.cname i.year if samp1 == 1,  bw(2) ro nocons

test (temp + lag1_temp +lag2_temp=0)
estadd scalar p1=r(p),: ols1
test (temp2 + lag1_temp2 +lag2_temp2=0)
estadd scalar p2=r(p),: ols1

*3 lag
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 ///
lag1_temp lag1_temp2 ///
lag2_temp lag2_temp2 ///
lag3_temp lag3_temp2 ///
dprecip dprecip2 i.cname i.year if samp1 == 1,  bw(2) ro nocons

test (temp + lag1_temp +lag2_temp + lag3_temp=0)
estadd scalar p1=r(p),: ols2
test (temp2 + lag1_temp2 +lag2_temp2 + lag3_temp2=0)
estadd scalar p2=r(p),: ols2

*4 lag
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 ///
lag1_temp lag1_temp2 ///
lag2_temp lag2_temp2 ///
lag3_temp lag3_temp2 ///
lag4_temp lag4_temp2 ///
dprecip dprecip2 i.cname i.year if samp1 == 1,  bw(2) ro nocons

test (temp + lag1_temp +lag2_temp + lag3_temp + lag4_temp=0)
estadd scalar p1=r(p),: ols3
test (temp2 + lag1_temp2 +lag2_temp2 + lag3_temp2 + lag4_temp2=0)
estadd scalar p2=r(p),: ols3

*5 lag
eststo ols4: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 ///
lag1_temp lag1_temp2 ///
lag2_temp lag2_temp2 ///
lag3_temp lag3_temp2 ///
lag4_temp lag4_temp2 ///
lag5_temp lag5_temp2 ///
dprecip dprecip2 i.cname i.year if samp1 == 1,  bw(2) ro nocons

test (temp + lag1_temp +lag2_temp + lag3_temp + lag4_temp + lag5_temp=0)
estadd scalar p1=r(p),: ols4
test (temp2 + lag1_temp2 +lag2_temp2 + lag3_temp2 + lag4_temp2 + lag5_temp2=0)
estadd scalar p2=r(p),: ols4

//Produce table for paper
esttab using "`tabledir'/regtable_lagtemp.tex", ///
	se(4) b(4) ///
	order(temp temp2 lag1_temp lag1_temp2 lag2_temp lag2_temp2 lag3_temp lag3_temp2 lag4_temp lag4_temp2 lag5_temp lag5_temp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dprecip dprecip2 lag1_delta_log_tfp lag1_temp lag1_temp2 lag2_temp lag2_temp2 lag3_temp lag3_temp2 lag4_temp lag4_temp2 lag5_temp lag5_temp2) ///
	stats(N r2_a p1 p2,fmt(%9.0fc %9.2fc %9.4fc %9.4fc)) label tex replace ///
	mlabels("2 lags" "3 lags" "4 lags" "5 lags", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\sum_i \eta_i=0$  (p-value)" "p2" "$\sum_i \nu_i=0$ (p-value)") ///
	compress nonotes ///
		nogaps ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With More Lags of Temperature}" "\label{tab:regtable_lagtemp}"  "\begin{tabular}{l*{4}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear


