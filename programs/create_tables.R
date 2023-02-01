################################################################################
# This program creates all model and simulation result tables
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/11/2022
################################################################################

################################################################################
# Section 1: Produce Table Appendix A
################################################################################

#Load in samples
df_samp <- read.csv(here('data','output','regSampleTfp.csv')) %>%
  select('delta_log_tfp', 'delta_log_gdppc', 'temp', 
         'dtemp',  'dprecip',
         'year','countryname')

#Generate summary stats
sumtable <- stargazer(df_samp,
                          omit = c("year", "countryname"),
                          covariate.labels = c("$\\Delta Ln TFP$","$\\Delta Ln GDPPC$", "$Temp.$", "$\\Delta Temp.$", "$\\Delta Precip.$"),
                          type = "latex",
                          title = "Summary Statistics",
                          table.placement = "htp!",
                          font.size = "normalsize"
)

footer <- c('\\multicolumn{6}{l}{Note:  Summary statistics for regression sample in Tables 1 and 2} \\',
  '\\end{tabular}',
  '\\end{table}')

sumtable <- sumtable[-seq(19,20,1)]
sumtable <- c(sumtable,footer)

cat(sumtable, sep = '\n', file =here('results','tables', 'sumtable.tex'))



