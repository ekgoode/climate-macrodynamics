**********************************************************************
***** Program: regression_tables_clustering.do
***** By: Greg Casey Stephie Fried, Ethan Goode
***** Date: 01/13/2023
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
label var lag`i'_delta_log_tfp "\$\Delta Ln TFP_{t-`i'}: \rho $"
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


********************************************************************************
* Table B1: Main Results with Clustered Standard Errors
********************************************************************************

*Set sample
ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year,  ro cluster(countryid) nocons
gen samp1 = e(sample)

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dprecip dprecip2  i.cname i.year if samp1==1,  cluster(countryid) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  cluster(countryid) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year if samp1==1,  cluster(countryid) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_ro_cluster.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Main Results With Clustered Standard Errors}" "\label{tab:regtableOLS1_ro_cluster}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear
