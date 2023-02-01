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

*Set subdirectories
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

**** Start Regression tables ***************************************************
eststo clear
*** Label vars for tables ******************************************************
label var delta_log_tfp "$\Delta \log TFP: \rho $"
label var delta_log_kpc "$\Delta \log KPC: \rho $"
label var delta_log_emp "$\Delta \log L: \rho $"
label var temp "\$Temp.: \gamma_1$"
label var temp2 "\$ Temp.^2: \gamma_2$"
label var dtemp "\$\Delta Temp.: \beta_1$"
label var dtemp2 "\$\Delta Temp.^2: \beta_2$"
label var precip "\$Precip.$"
label var precip2 "\$Precip.^2$"
label var precip2 "\$Precip.^2$"
label var dprecip "\$\Delta Precip.: \xi_1$"
label var dprecip2 "\$\Delta Precip.^2: \xi_2$"

encode(Region), gen(region_factor)

********************************************************************************
* Table 1: Main Results
* DV: log_gr_tfp, 1 lag, OLS
********************************************************************************

*Set sample
ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year,  bw(2) ro nocons
gen samp1 = e(sample)

preserve
keep if samp1==1
export delimited "`datadir'/regSampleTfp.csv", replace
restore

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 
abar, lags(5)


*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2
abar, lags(5)

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3
abar, lags(5)



*Export results for R
esttab using "`datadir'/regtable1.csv", replace


//Produce table for paper
esttab using "`tabledir'/regtableOLS1.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Main Results}" "\label{tab:regtableOLS1}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table 2: GDP per capita results
* DV: delta_log_gdppc
********************************************************************************
*OLS, growth effects only
eststo ols1: ivreg2 delta_log_gdppc lag1_delta_log_gdppc temp temp2 dprecip dprecip2 i.cname i.year if samp1==1, ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_gdppc lag1_delta_log_gdppc dtemp dtemp2 dprecip dprecip2 i.cname i.year  if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_gdppc lag1_delta_log_gdppc temp temp2 dtemp dtemp2 dprecip dprecip2   i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

esttab using "`datadir'/regtable2.csv", replace
eststo clear

*Make Table 2

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_gdppc temp temp2 dprecip dprecip2 i.cname i.year if samp1==1, ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1


eststo ols2: ivreg2 delta_log_gdppc lag1_delta_log_gdppc temp temp2 dprecip dprecip2 i.cname i.year if samp1==1, ro  nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols2
estadd scalar p1=r(p),: ols2

*OLS, level effects only
eststo ols3: ivreg2 delta_log_gdppc  dtemp dtemp2 dprecip dprecip2 i.cname i.year  if samp1==1,  bw(2) ro  nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols3
estadd scalar p2=r(p),: ols3

eststo ols4: ivreg2 delta_log_gdppc lag1_delta_log_gdppc dtemp dtemp2 dprecip dprecip2 i.cname i.year  if samp1==1,  bw(2) ro  nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols4
estadd scalar p2=r(p),: ols4

*OLS, level and growth effects
eststo ols5: ivreg2 delta_log_gdppc temp temp2 dtemp dtemp2 dprecip dprecip2   i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols5
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols5

eststo ols6: ivreg2 delta_log_gdppc lag1_delta_log_gdppc temp temp2 dtemp dtemp2 dprecip dprecip2   i.cname i.year if samp1==1,  bw(2)  ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols6
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols6


esttab using "`tabledir'/regtableOLS2.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_gdppc) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_gdppc) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("" "" "" "" "" "", nonumbers lhs(Dep. Variable: $\Delta Ln GDPPC$)  ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{GDP per Capita Results}" "\label{tab:regtableOLS2}"  "\begin{tabular}{l*{6}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B2: Results Without the Lagged Dependent Variable
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp  temp temp2 dprecip dprecip2 i.cname i.year  if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp   dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp  temp temp2 dtemp dtemp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_nolag.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 ) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 ) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results Without the Lagged Dependent Variable}" "\label{tab:regtableOLS1_nolag}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear


********************************************************************************
* Table B3: Results with TFP Innovations
********************************************************************************

ivreg2 delta_log_tfp lag1_delta_log_tfp
predict delta_log_tfp_resid, residuals

*OLS, growth effects only
eststo ols1: ivreg2  delta_log_tfp_resid temp temp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2  delta_log_tfp_resid  dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2  delta_log_tfp_resid temp temp2 dtemp dtemp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3


//Produce table for paper
esttab using "`tabledir'/regtableOLS1_resids.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$ innovations) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With TFP Innovations}" "\label{tab:regtableOLS1_resids}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B8: Results with Linear Time Trends
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dprecip dprecip2 i.cname i.year X_yi* if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year X_yi* if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2  i.cname i.year X_yi* if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

*Export results for R
esttab using "`datadir'/regtable1_linear_trends.csv", replace

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_lintrends.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Linear Time Trends}" "\label{tab:regtableOLS1_lintrends}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B9: Results with Quadratic Time Trends
********************************************************************************
*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2  i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

*Export results for R
esttab using "`datadir'/regtable1_quadratic_trends.csv", replace

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_trends.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Quadratic Time Trends}" "\label{tab:regtableOLS1_trends}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B10: Results With Region-by-Year Fixed Effects
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2  delta_log_tfp lag1_delta_log_tfp temp temp2 dprecip dprecip2 i.cname i.year i.year#i.region_factor if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2  delta_log_tfp lag1_delta_log_tfp dtemp dtemp2 dprecip dprecip2 i.cname i.year i.year#i.region_factor if samp1==1,  bw(2) ro nocons 
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2  delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year i.year#i.region_factor if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

*Export results for R
esttab using "`datadir'/regtable1_region_fe.csv", replace

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_region.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Region-by-Year Fixed Effects}" "\label{tab:regtableOLS1_region}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B20: Results with Capital as the Dependent Variable
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_kpc temp temp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_kpc   dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_kpc temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_kpc.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln KPC$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Capital as the Dependent Variable}" "\label{tab:regtableOLS1_KPC}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear


********************************************************************************
* Table B21: Results with Labor as the Dependent Variable
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_emp  temp temp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]), : ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_emp dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_emp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_emp.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln L$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Labor as the Dependent Variable}" "\label{tab:regtableOLS1_emp}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B19: GDP per capita Results with Quadratic Time Trends
********************************************************************************
*OLS, growth effects only
eststo ols1: ivreg2 delta_log_gdppc temp temp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1, ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

eststo ols2: ivreg2 delta_log_gdppc lag1_delta_log_gdppc temp temp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols2
estadd scalar p1=r(p),: ols2

*OLS, level effects only
eststo ols3: ivreg2 delta_log_gdppc dtemp dtemp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols3
estadd scalar p2=r(p),: ols3

eststo ols4: ivreg2 delta_log_gdppc lag1_delta_log_gdppc  dtemp dtemp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols4
estadd scalar p2=r(p),: ols4

*OLS, level and growth effects
eststo ols5: ivreg2 delta_log_gdppc temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols5
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols5

eststo ols6: ivreg2 delta_log_gdppc lag1_delta_log_gdppc temp temp2 dtemp dtemp2 dprecip dprecip2  X_yi* X_y2* i.cname i.year X_yi* X_y2* if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols6
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols6

esttab using "`tabledir'/regtableOLS2_quadratictrends.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_gdppc) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_gdppc) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Growth" "Level" "Level" "Both" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln GDPPC$)  ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{GDP per Capita Results With Quadratic Time Trends}" "\label{tab:regtableOLS2_quadratictrends}"  "\begin{tabular}{l*{6}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B6: Results with Additional Dependent Var. Lags
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp ///
	lag2_delta_log_tfp lag3_delta_log_tfp lag4_delta_log_tfp ///
	temp temp2 dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
	
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 
abar, lags(5)

*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp ///
	lag2_delta_log_tfp lag3_delta_log_tfp lag4_delta_log_tfp ///
	dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
	
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2
abar, lags(5)

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp ///
	lag2_delta_log_tfp lag3_delta_log_tfp lag4_delta_log_tfp ///
	temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
	
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3
abar, lags(5)

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_4lagstfp.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp lag2_delta_log_tfp lag3_delta_log_tfp lag4_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp lag2_delta_log_tfp lag3_delta_log_tfp lag4_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Additional Dependent Var. Lags}" "\label{tab:regtableOLS1_4lagstfp}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B4/5: Dynamic Panel Results
********************************************************************************
*OLS, growth effects only, IV w/ 2cnd lag of DV
eststo iv1: ivreg2 delta_log_tfp  ///
	temp temp2 dprecip dprecip2 i.cname i.year ///
	(lag1_delta_log_tfp = lag2_delta_log_tfp) if samp1 == 1, bw(2) ro nocons ///
	first ffirst savefirst savefprefix(st1)
	
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: iv1
estadd scalar p1=r(p),: iv1 
abar, lags(5)

*OLS, growth effects only, IV w/ 2cnd and 3rd lag of DV
eststo iv2: ivreg2 delta_log_tfp  ///
	temp temp2 dprecip dprecip2 i.cname i.year ///
	(lag1_delta_log_tfp = lag2_delta_log_tfp lag3_delta_log_tfp) if samp1 == 1, bw(2) ro nocons ///
	first ffirst savefirst savefprefix(st2)
	
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: iv2
estadd scalar p1=r(p),: iv2 
abar, lags(5)

*OLS, level effects only, IV w/ 2cnd lag of DV
eststo iv3: ivreg2 delta_log_tfp  ///
	dtemp dtemp2 dprecip dprecip2 i.cname i.year ///
	(lag1_delta_log_tfp = lag2_delta_log_tfp) if samp1 == 1, bw(2) ro nocons ///
	first ffirst savefirst savefprefix(st3)
	
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : iv3
estadd scalar p2=r(p),: iv3
abar, lags(5)

*OLS, level effects only, IV w/ 2cnd and 3rd lag of DV
eststo iv4: ivreg2 delta_log_tfp  ///
	dtemp dtemp2 dprecip dprecip2 i.cname i.year ///
	(lag1_delta_log_tfp = lag2_delta_log_tfp lag3_delta_log_tfp) if samp1 == 1, bw(2) ro nocons ///
	first ffirst savefirst savefprefix(st4)
	
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : iv4
estadd scalar p2=r(p),: iv4
abar, lags(5)

*OLS, level and growth effects, IV w/ 2cnd lag of DV
eststo iv5: ivreg2 delta_log_tfp  ///
	temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year ///
	(lag1_delta_log_tfp = lag2_delta_log_tfp) if samp1 == 1, bw(2) ro nocons ///
	first ffirst savefirst savefprefix(st5)
	
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: iv5
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: iv5
abar, lags(5)

*OLS, level and growth effects, IV w/ 2cnd and 3rd lag of DV
eststo iv6: ivreg2 delta_log_tfp  ///
	temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year ///
	(lag1_delta_log_tfp = lag2_delta_log_tfp lag3_delta_log_tfp) if samp1 == 1, bw(2) ro nocons ///
	first ffirst savefirst savefprefix(st6)
	
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: iv6
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: iv6
abar, lags(5)

//Produce first-stage table
esttab st* using "`tabledir'/regtableIV_firststage.tex", ///
	se(4) b(4) ///
	order(lag2_delta_log_tfp lag3_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag2_delta_log_tfp lag3_delta_log_tfp) ///
	stats(N,fmt(%9.0fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Growth" "Level" "Level" "Both" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP_{t-1}$) ) ///
		substitute("rkf" "Kleibergen-Papp F-Stat" "jp" "Hansen J-test"  "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Dynamic Panel First Stage Results}" "\label{tab:regtableIV_firststage}"  "\begin{tabular}{l*{6}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
	
//Produce second-stage table
esttab using "`tabledir'/regtableOLS1_lagsIV.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N rkf jp p1 p2,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Growth" "Level" "Level" "Both" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("rkf" "Kleibergen-Papp F-Stat" "jp" "Hansen J-test"  "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Dynamic Panel Results}" "\label{tab:regtableOLS1_lagsIV}"  "\begin{tabular}{l*{6}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
	


