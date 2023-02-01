**********************************************************************
***** Program: regression_tables_het.do
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
drop poor

*Set time and year dimensions
egen countryid = group(countryname)
xtset countryid year
encode(countryname), gen(cname)

*gen lag variables (and name them)
forvalues i = 1/10 {
g lag`i'_delta_log_tfp = L`i'.delta_log_tfp
label var lag`i'_delta_log_tfp "\$\Delta Ln TFP_{t-`i'}: \rho_`i' $"
g lag`i'_delta_log_gdppc = L`i'.delta_log_gdppc
label var lag`i'_delta_log_gdppc "\$ \Delta Ln GDPPC_{t-`i'}: \rho_`i' $"
g lag`i'_temp = L`i'.temp
label var lag`i'_temp "\$Temp_{t-`i'}: \eta_`i'$"
g lag`i'_temp2 = L`i'.temp2
label var lag`i'_temp2 "\$Temp^2_{t-`i'}: \nu_`i'$"
}

*Create indicator variable for rich and poor countries
preserve
keep if year ==2010
gen rich =.
egen median_gdppc = median(gdppc)
replace rich = 1 if gdppc >= median_gdppc & year ==2010
replace rich = 0 if gdppc < median_gdppc & year ==2010
keep countryname rich
save ./data/output/rich_ind.dta, replace
restore

merge m:1 countryname using ./data/output/rich_ind.dta, nogen
gen poor = 1 - rich

gen temprich = temp*rich
gen temppoor = temp*poor
gen temp2rich = temp2*rich
gen temp2poor = temp2*poor
gen dtemprich = dtemp*rich
gen dtemppoor = dtemp*poor
gen dtemp2rich = dtemp2*rich
gen dtemp2poor = dtemp2*poor

*Get interaction of temperature and lag temperature
gen temp_int = temp * lag1_temp
gen temp2_int = temp2 * lag1_temp2

encode(Region), gen(region_factor)

*Generate Ag country variables
gen non_ag_country = 1- ag_country

foreach var of varlist temp temp2 dtemp dtemp2 {
    gen `var'ag_country = `var' * ag_country
	gen `var'non_ag_country = `var' * non_ag_country
}

*Generate interaction for average temperature
egen mean_temp = mean(temp), by(countryname)

foreach var of varlist temp temp2 dtemp dtemp2 {
	g `var'Tbar = `var' * mean_temp
}

**** Start Regression tables ***************************************************
eststo clear
*** Label vars for tables ******************************************************
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
label var tempTbar "\$Temp. \times \bar{T}: \gamma_1^{\bar{T}}$"
label var temp2Tbar "\$ Temp.^2 \times \bar{T}: \gamma_2^{\bar{T}}$"
label var dtempTbar "\$\Delta Temp. \times \bar{T}: \beta_1^{\bar{T}}$"
label var dtemp2Tbar "\$\Delta Temp.^2 \times \bar{T}: \beta_2^{\bar{T}}$"

*Label income interactions
label var temprich "\$Temp. \times Rich: \gamma_1^R$"
label var temppoor "\$Temp. \times Poor: \gamma_1^P$"
label var temp2rich "\$Temp.^2 \times Rich: \gamma_2^R$"
label var temp2poor "\$Temp.^2 \times Poor: \gamma_2^P$"
label var dtemprich "\$\Delta Temp. \times Rich: \beta_1^R$"
label var dtemppoor "\$\Delta Temp. \times Poor: \beta_1^P$"
label var dtemp2rich "\$\Delta Temp.^2 \times Rich: \beta_2^R$"
label var dtemp2poor "\$\Delta Temp.^2 \times Poor: \beta_2^P$"

*Label Agriculture value-added interactions
label var tempag_country "\$Temp. \times Ag.: \gamma_1^A$"
label var tempnon_ag_country "\$Temp. \times NonAg.: \gamma_1^N$"
label var temp2ag_country "\$Temp.^2 \times Ag.: \gamma_2^A$"
label var temp2non_ag_country "\$Temp.^2 \times NonAg.: \gamma_2^N$"
label var dtempag_country "\$\Delta Temp. \times Ag.: \beta_1^A$"
label var dtempnon_ag_country "\$\Delta Temp. \times NonAg.: \beta_1^N$"
label var dtemp2ag_country "\$\Delta Temp.^2 \times Ag.: \beta_2^A$"
label var dtemp2non_ag_country "\$\Delta Temp.^2 \times NonAg.: \beta_2^N$"

********************************************************************************
* Table B11: Results with Post-1990 Dummies
********************************************************************************
*Set sample
ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year,  bw(2) ro nocons
gen samp1 = e(sample)

g post90 = (year >= 1990)

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dprecip dprecip2  i.cname i.year i.cname#post90 if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar optTemp = -(_b[temp])/(2*_b[temp2]),: ols1
estadd scalar p1=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2 delta_log_tfp lag1_delta_log_tfp  dtemp dtemp2 dprecip dprecip2 i.cname i.year i.cname#post90 if samp1==1,  bw(2) ro nocons ffirst
test (dtemp=0) (dtemp2=0)
estadd scalar optTemp = -(_b[dtemp])/(2*_b[dtemp2]), : ols2
estadd scalar p2=r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 dprecip dprecip2 i.cname i.year i.cname#post90 if samp1==1,  bw(2) ro nocons
test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3

*Export results for R
esttab using "`datadir'/regtable1_1990_dummy.csv", replace

//Produce table for paper
esttab using "`tabledir'/regtableOLS1_dum.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1 p2 optTemp,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "optTemp" "Optimal Temperature") ///
	compress nonotes ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Post-1990 Dummies}" "\label{tab:regtableOLS1_dum}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear


********************************************************************************
* Table B14: Heterogeneity by Level of Development
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2  delta_log_tfp lag1_delta_log_tfp temprich temp2rich temppoor temp2poor dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (temprich=0) (temp2rich=0)
estadd scalar p1rich=r(p),: ols1 
test (temppoor=0) (temp2poor=0)
estadd scalar p1poor=r(p),: ols1 
test temprich - temppoor =0
estadd scalar gamma1_rp=r(p),: ols1 
test temp2rich - temp2poor =0
estadd scalar gamma2_rp=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2  delta_log_tfp lag1_delta_log_tfp dtemprich dtemp2rich dtemppoor dtemp2poor dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtemprich=0) (dtemp2rich=0)
estadd scalar p2rich=r(p),: ols2
test (dtemppoor=0) (dtemp2poor=0)
estadd scalar p2poor=r(p),: ols2 
test (dtemprich = dtemppoor)
estadd scalar beta1_rp=r(p),: ols2 
test (dtemp2rich = dtemp2poor)
estadd scalar beta2_rp=r(p),: ols2 


*OLS, level and growth effects
eststo ols3: ivreg2  delta_log_tfp lag1_delta_log_tfp temprich temp2rich temppoor temp2poor dtemprich dtemp2rich dtemppoor dtemp2poor dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (temprich=0) (temp2rich=0)
estadd scalar p1rich=r(p),: ols3
test (temppoor=0) (temp2poor=0)
estadd scalar p1poor=r(p),: ols3
test (dtemprich=0) (dtemp2rich=0)
estadd scalar p2rich=r(p),: ols3 
test (dtemppoor=0) (dtemp2poor=0)
estadd scalar p2poor=r(p),: ols3 
test temprich - temppoor =0
estadd scalar gamma1_rp=r(p),: ols3 
test temp2rich - temp2poor =0
estadd scalar gamma2_rp=r(p),: ols3
test (dtemprich = dtemppoor)
estadd scalar beta1_rp=r(p),: ols3 
test (dtemp2rich = dtemp2poor)
estadd scalar beta2_rp=r(p),: ols3 


//Produce table for paper
esttab using "`tabledir'/regtableOLS1_hetero.tex", ///
	se(4) b(4) ///
	order(temprich temp2rich temppoor temp2poor dtemprich  dtemp2rich dtemppoor dtemp2poor dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temprich temp2rich temppoor temp2poor dtemprich  dtemp2rich dtemppoor dtemp2poor dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1rich p1poor p2rich p2poor gamma1_rp gamma2_rp beta1_rp beta2_rp ,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.4fc %9.4fc  %9.2fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
		substitute("r2_a" "Adj. R-squared" "p1rich" "$\gamma_1^R = \gamma_2^R =0$ (p-value)" "p1poor" "$\gamma_1^P = \gamma_2^P =0$ (p-value)" "p2rich" "$\beta_1^R = \beta_2^R=0$ (p-value)" "p2poor" "$\beta_1^P = \beta_2^P=0$ (p-value)"  "gamma1_rp" "$\gamma_1^P = \gamma_1^R$ (p-value)" "gamma2_rp" "$\gamma_2^P= \gamma_2^R$ (p-value)" "beta1_rp" "$\beta_1^R = \beta_1^P$ (p-value)" "beta2_rp" "$\beta_2^R = \beta_2^P$ (p-value)") ///
	compress nonotes ///
	nogaps ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Heterogeneity by Level of Development}" "\label{tab:regtableOLS1_hetero}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear


********************************************************************************
* Table B15: Heterogeneity by Economic Structures
********************************************************************************

*OLS, growth effects only
eststo ols1: ivreg2  delta_log_tfp lag1_delta_log_tfp tempag_country temp2ag_country tempnon_ag_country temp2non_ag_country dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (tempag_country=0) (temp2ag_country=0)
estadd scalar p1ag_country=r(p),: ols1 
test (tempnon_ag_country=0) (temp2non_ag_country=0)
estadd scalar p1non_ag_country=r(p),: ols1 
test tempag_country - tempnon_ag_country =0
estadd scalar gamma1_rp=r(p),: ols1 
test temp2ag_country - temp2non_ag_country =0
estadd scalar gamma2_rp=r(p),: ols1 

*OLS, level effects only
eststo ols2: ivreg2  delta_log_tfp lag1_delta_log_tfp dtempag_country dtemp2ag_country dtempnon_ag_country dtemp2non_ag_country dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons ffirst
test (dtempag_country=0) (dtemp2ag_country=0)
estadd scalar p2ag_country=r(p),: ols2
test (dtempnon_ag_country=0) (dtemp2non_ag_country=0)
estadd scalar p2non_ag_country=r(p),: ols2 
test (dtempag_country = dtempnon_ag_country)
estadd scalar beta1_rp=r(p),: ols2 
test (dtemp2ag_country = dtemp2non_ag_country)
estadd scalar beta2_rp=r(p),: ols2 


*OLS, level and growth effects
eststo ols3: ivreg2  delta_log_tfp lag1_delta_log_tfp tempag_country temp2ag_country tempnon_ag_country temp2non_ag_country dtempag_country dtemp2ag_country dtempnon_ag_country dtemp2non_ag_country dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons
test (tempag_country=0) (temp2ag_country=0)
estadd scalar p1ag_country=r(p),: ols3
test (tempnon_ag_country=0) (temp2non_ag_country=0)
estadd scalar p1non_ag_country=r(p),: ols3
test (dtempag_country=0) (dtemp2ag_country=0)	
estadd scalar p2ag_country=r(p),: ols3 
test (dtempnon_ag_country=0) (dtemp2non_ag_country=0)
estadd scalar p2non_ag_country=r(p),: ols3 
test tempag_country - tempnon_ag_country =0
estadd scalar gamma1_rp=r(p),: ols3 
test temp2ag_country - temp2non_ag_country =0
estadd scalar gamma2_rp=r(p),: ols3
test (dtempag_country = dtempnon_ag_country)
estadd scalar beta1_rp=r(p),: ols3 
test (dtemp2ag_country = dtemp2non_ag_country)
estadd scalar beta2_rp=r(p),: ols3 


//Produce table for paper
esttab using "`tabledir'/regtableOLS1_hetero_agriculture.tex", ///
	se(4) b(4) ///
	order(tempag_country temp2ag_country tempnon_ag_country temp2non_ag_country dtempag_country  dtemp2ag_country dtempnon_ag_country dtemp2non_ag_country dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(tempag_country temp2ag_country tempnon_ag_country temp2non_ag_country dtempag_country  dtemp2ag_country dtempnon_ag_country dtemp2non_ag_country dprecip dprecip2 lag1_delta_log_tfp) ///
	stats(N r2_a p1ag_country p1non_ag_country p2ag_country p2non_ag_country gamma1_rp gamma2_rp beta1_rp beta2_rp ,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.4fc %9.4fc  %9.2fc %9.2fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
		substitute("r2_a" "Adj. R-squared" "p1ag_country" "$\gamma_1^A = \gamma_2^A =0$ (p-value)" "p1non_ag_country" "$\gamma_1^N = \gamma_2^N =0$ (p-value)" "p2ag_country" "$\beta_1^A = \beta_2^A=0$ (p-value)" "p2non_ag_country" "$\beta_1^N = \beta_2^N=0$ (p-value)"  "gamma1_rp" "$\gamma_1^N = \gamma_1^A$ (p-value)" "gamma2_rp" "$\gamma_2^N= \gamma_2^A$ (p-value)" "beta1_rp" "$\beta_1^A = \beta_1^N$ (p-value)" "beta2_rp" "$\beta_2^A = \beta_2^N$ (p-value)") ///
	compress nonotes ///
	nogaps ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Heterogeneity by Economic Structure}" "\label{tab:regtableOLS1_hetero_ag}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

********************************************************************************
* Table B17: Results with Temperature Interactions
********************************************************************************
label var temp "\$Temp_{t}$"
label var temp2 "\$ Temp^2_{t}$"
label var lag1_temp "\$Temp_{t-1}$"
label var lag1_temp2 "\$Temp^2_{t-1}$"
label var dprecip "\$\Delta Precip.$"
label var dprecip2 "\$\Delta Precip.^2$"
label var lag1_delta_log_tfp "\$\Delta Ln TFP_{t-1}$"

*OLS, growth effects only
eststo ols1: ivreg2 delta_log_tfp lag1_delta_log_tfp temp temp2 lag1_temp lag1_temp2 temp_int temp2_int  dprecip dprecip2  i.cname i.year if samp1==1,  bw(2) ro nocons
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols1 

//Produce table for paper
esttab using "`tabledir'/regtable_tempint.tex", ///
	se(4) b(4) ///
	order(lag1_temp lag1_temp2 temp temp2 temp_int temp2_int dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dprecip dprecip2 lag1_temp lag1_temp2 temp_int temp2_int lag1_delta_log_tfp) ///
	stats(N r2_a,fmt(%9.0fc %9.2fc %9.4fc %9.4fc)) label tex replace ///
	mlabels("OLS", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared") ///
	compress nonotes ///
		nogaps ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Results With Temperature Interactions}" "\label{tab:regtable_tempint}"  "\begin{tabular}{l*{2}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear



********************************************************************************
* Table B16: Heterogeneity by Temperature
********************************************************************************
*OLS, growth effects only
eststo ols1: ivreg2  delta_log_tfp lag1_delta_log_tfp temp temp2 tempTbar temp2Tbar dprecip dprecip2 i.cname i.year if samp1==1,  bw(2) ro nocons

test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols1 
test (tempTbar=0) (tempTbar=0)
estadd scalar p3=r(p),: ols1

*OLS, level effects only
eststo ols2: ivreg2  delta_log_tfp lag1_delta_log_tfp dtemp dtemp2 dtempTbar dtemp2Tbar i.cname i.year if samp1==1,  bw(2) ro nocons ffirst

test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols2
test (dtempTbar=0) (dtemp2Tbar=0)
estadd scalar p4= r(p),: ols2

*OLS, level and growth effects
eststo ols3: ivreg2  delta_log_tfp lag1_delta_log_tfp temp temp2 dtemp dtemp2 tempTbar temp2Tbar dtempTbar dtemp2Tbar dprecip dprecip2 i.cname i.year if samp1 == 1, bw(2) ro nocons

test (dtemp=0) (dtemp2=0)
estadd scalar p2=r(p),: ols3
test (temp=0) (temp2=0)
estadd scalar p1=r(p),: ols3
test (tempTbar=0) (tempTbar=0)
estadd scalar p3=r(p),: ols3
test (dtempTbar=0) (dtemp2Tbar=0)
estadd scalar p4= r(p),: ols3

//Produce table for paper
esttab using "`tabledir'/regtable_tbar.tex", ///
	se(4) b(4) ///
	order(temp temp2 dtemp dtemp2 tempTbar temp2Tbar dtempTbar dtemp2Tbar dprecip dprecip2 lag1_delta_log_tfp) ///
	keep(temp temp2 dtemp dtemp2 dprecip dprecip2 tempTbar temp2Tbar dtempTbar dtemp2Tbar lag1_delta_log_tfp) ///
stats(N r2_a p1 p2 p3 p4,fmt(%9.0fc %9.2fc %9.4fc %9.4fc %9.4fc %9.4fc)) label tex replace ///
	mlabels("Growth" "Level" "Both", nonumbers lhs(Dep. Variable: $\Delta Ln TFP$) ) ///
	substitute("r2_a" "Adj. R-squared" "p1" "$\gamma_1 = \gamma_2 =0$ (p-value)" "p2" "$\beta_1 = \beta_2=0$ (p-value)" "p3" "$\gamma_1^{\bar{T}} = \gamma_2^{\bar{T}}=0$ (p-value)" "p4" "$\beta_1^{\bar{T}} = \beta_2^{\bar{T}}=0$ (p-value)") ///
	compress nonotes ///
		nogaps ///
	prehead("\centering" "\singlespace" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Heterogeneity by Temperature}" "\label{tab:regtable_tbar}"  "\begin{tabular}{l*{3}{c}}" "\hline\hline") ///
	postfoot("\hline\hline" "\end{tabular}")
eststo clear

