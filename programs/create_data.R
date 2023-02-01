################################################################################
# This program creates the datasets to be used from raw data files
#
# The following files are required as inputs to this program:
#   1. pwt100.xlsx : The 10.0 vintage of the Penn World Tables
#   2. GrowthClimateDataset.csv: BHM data (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LSDCAY)
#   3. API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_4258562.csv: World Bank Ag series.
#   4. etd-release2021.xlsx: Vietnamese Agriculture Data from the Economic Transformation Database.
#   5. projections_crosswalk.csv: Country cross-walk.
#   6. regional_classification_tblDJO.xlsx: To classify countries into regions.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/12/2022
#
################################################################################
################################################################################
# Section 1 - Load Data and define missing countries
################################################################################

#Import PWT 10.0
df_pwt10 <- read_excel(here('data','raw','pwt100.xlsx'), 'Data')

#Import GrowthClimateDataset
df_GrowthClimateDataset <- read.csv(here('data','raw','GrowthClimateDataset.csv'))

#Import World Bank Ag Series
df_ag <- read.csv(here('data','raw','API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_4258562.csv'), header=FALSE)
df_ag_vietnam <- read_excel(here('data','raw','etd-release2021.xlsx'), 'Data')

#Load Crosswalk
crosswalk <- read.csv(here('data','raw','projections_crosswalk.csv'), col.names=c('countryname','directory')) %>%
  mutate(countryname = as.character(countryname),
         directory = as.character(directory))
#Fix South Korea
crosswalk[15,1] <- "Korea, Rep."

#Define missing countries
missing_countries <- c('Angola', 'Belize', 'Benin', 'Bhutan',
                       'Bosnia and Herzegovina', 'Botswana', 'Burkina Faso', 'Burundi',
                       'Central African Republic', 'Chad', 'Comoros', 'Costa Rica',
                       'Djibouti', 'Greece', 'Hong Kong SAR, China', 'Oman',
                       'Senegal', 'Trinidad and Tobago', 'St. Vincent and the Grenadines', 'Taiwan', 'St. Lucia', 
                       'Congo_(Brazzaville)', 'Congo_(Kinshasa)', 'Gabon', 'Hong_Kong', 'Curacao', 'Anguilla',
                       'Antigua_and_Barb.','Aruba','Bahrain','Barbados','Bermuda','British_Virgin_Is.','Cayman_Is.',
                       'Curacao', 'Dominica', 'Macau','Maldives','Malta','Montenegro', 'Montserrat',
                       'Seychelles', 'Singapore', 'Sint Maarten (Dutch part)', 'St. Kitts and Nevis', 'Swaziland','Turkmenistan','Turks_and_Caicos_Is.',
                       'West Bank and Gaza', "Albania", 'Grenada', 'Myanmar')

################################################################################
# Section 2 - Clean/Process Data
################################################################################

########
# Sub Section - 2.1 Clean df_pwt10
########
df_pwt10 <- df_pwt10 %>%
  rename('countryname' = 'country',
         'iso' = 'countrycode')

df_pwt10$countryname <- recode(df_pwt10$countryname,
                               "Côte d'Ivoire" = "Cote d'Ivoire",
                               "D.R. of the Congo" = "Congo, Dem. Rep.",
                               "Bolivia (Plurinational State of)" = "Bolivia",
                               "Bahamas" = "Bahamas, The",
                               "Egypt" = "Egypt, Arab Rep.",
                               "Gambia" = "Gambia, The",
                               "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
                               "Republic of Moldova" = "Moldova",
                               "Republic of Korea" = "Korea, Rep.",
                               "Kyrgyzstan" = "Kyrgyz Republic",
                               "Lao People's DR" = "Lao PDR",
                               "Slovakia" = "Slovak Republic",
                               "Venezuela (Bolivarian Republic of)" = "Venezuela, RB",
                               "Viet Nam" = "Vietnam",
                               "Yemen" = "Yemen, Rep.",
                               "Congo" = "Congo, Rep.",
                               "China, Hong Kong SAR" = "Hong Kong SAR, China",
                               "China, Macao SAR" = "Macao SAR, China",
                               "U.R. of Tanzania: Mainland" = "Tanzania",
                               "State of Palestine" = "West Bank and Gaza",
                               "Saint Kitts and Nevis"  = "St. Kitts and Nevis",
                               "Saint Lucia" = "St. Lucia",
                               "North Macedonia" = "Macedonia, FYR")

df_pwt10$iso <- recode(df_pwt10$iso,
                               "ROU" = "ROM")



########
# Sub Section - 2.3 Merge df_pwt10 and df_GrowthClimateDataset 
# -> df_tfp with new variables of interest
########
df_tfp <- right_join(df_GrowthClimateDataset, df_pwt10, by = c('countryname', 'year')) %>%
  distinct(countryname, year, .keep_all = TRUE) %>%
  arrange(countryname, year) %>%
  mutate(gdppc = rgdpna/pop,
         log_gdppc = log(gdppc),
         alpha_jt = 0.33,
         investment_share = csh_i,
         capital = rnna,
         kpcap = (rnna/pop),
         log_kpc = log(kpcap),
         tfp = rgdpna/((rnna^alpha_jt)*pop^(1-alpha_jt)), 
         log_tfp = log(tfp),
         temp = UDel_temp_popweight,
         UDel_precip_popweight = UDel_precip_popweight/1000,
         precip = UDel_precip_popweight,
         precip2 = precip^2,
         log_tfp = log(tfp),
         log_emp = log(emp),
         log_pop = log(pop),
         temp2 = temp^2,
         zeroline = 0
  ) %>%
  group_by(countryname) %>%
  mutate(lagtemp = lag(temp),
         lagtemp2 = lag(temp2),
         dtemp = temp - lag(temp),
         dtemp2 = temp2 - lag(temp2),
         dprecip = precip - lag(precip),
         dprecip2 = precip2 - lag(precip2),
         lagdtemp = lag(dtemp),
         lagdtemp2 = lag(dtemp2),
         delta_log_gdppc = (log_gdppc - lag(log_gdppc)),
         delta_log_tfp = (log_tfp - lag(log_tfp)), #Dependent variable
         delta_log_emp = (log_emp - lag(log_emp)),
         delta_log_kpc = (log_kpc - lag(log_kpc)),
         gr_pop = (pop - lag(pop))/pop,
  )

########
# Sub Section - 2.2 Clean World bank Agriculture Value-Added % of GDP data
########
  
#General Cleaning (renaming columns + removing unnecessary rows + columns)
df_ag <- df_ag[-c(1,2),]
colnames(df_ag) <- unlist(df_ag[1,])
df_ag <- df_ag[-1,]
df_ag <- df_ag[,-67]

#Reshape long
df_ag <- pivot_longer(as_tibble(df_ag),cols=5:ncol(df_ag),names_to = "year",values_to = "pct_ag_va")
df_ag <- df_ag %>% 
  rename('countryname' = 'Country Name') %>% 
  mutate(year=as.numeric(year)) %>%
  select(countryname, year, pct_ag_va)

#Recode countries for merging
df_ag$countryname <- recode(df_ag$countryname,
                            'North Macedonia' = 'Macedonia, FYR',
                            'Turkiye' = 'Turkey')

#Get values for Vietnam
df_ag_vietnam <- df_ag_vietnam %>% 
  filter(country == "Viet Nam" & var == "VA") %>%
  mutate(pct_ag_va = (Agriculture/Total) * 100) %>%
  select(year, country, pct_ag_va) %>%
  rename('countryname'='country')
df_ag_vietnam$countryname <- recode(df_ag_vietnam$countryname,
                                    'Viet Nam' = 'Vietnam')
df_ag <- df_ag %>% filter(countryname != 'Vietnam')
df_ag <- full_join(df_ag,df_ag_vietnam, by=c('countryname','year','pct_ag_va'))

#Use 2013 & 2012 values for Djibouti & Armenia respectively 
df_ag[(df_ag$year == 2010 & df_ag$countryname == "Djibouti"),]$pct_ag_va <- df_ag[(df_ag$year == 2013 & df_ag$countryname == "Djibouti"),]$pct_ag_va
df_ag[(df_ag$year == 2010 & df_ag$countryname == "Armenia"),]$pct_ag_va <- df_ag[(df_ag$year == 2012 & df_ag$countryname == "Armenia"),]$pct_ag_va

#Merge in dataframe
df_tfp <- right_join(df_ag, df_tfp, by=c('countryname','year')) %>%
  distinct(countryname, year, .keep_all = TRUE) %>%
  arrange(countryname, year) %>%
  group_by(countryname)

median_pct_ag_va_2010 <- median(df_tfp[df_tfp$year == 2010,]$pct_ag_va, na.rm=TRUE)

df_ag_country <- df_tfp %>% 
  filter(year == 2010) %>% 
  mutate(ag_country = case_when(pct_ag_va >= median_pct_ag_va_2010 ~ 1,
                                                   pct_ag_va < median_pct_ag_va_2010 ~ 0,
                                                   TRUE ~ NA_real_)
         ) %>%
  select(countryname, ag_country)

df_tfp <- left_join(df_tfp,df_ag_country, by='countryname')

########
# Sub Section - 3 Get simulation data frame
########

crosswalk$countryname[crosswalk$countryname == 'CuraÃƒÂ§ao'] <- 'Curaçao'
df_sim <- full_join(df_tfp, crosswalk, by="countryname")
df_sim$countryname2 <- df_sim$countryname
df_sim$countryname[!(is.na(df_sim$directory))] <- df_sim$directory[!(is.na(df_sim$directory))]
df_sim <- df_sim %>%
  mutate(kmni_countries = gsub(" ", "_",countryname))

#Create balanced panel
balanced_panel <- df_tfp %>%
  mutate(count_missing = ifelse(is.na(delta_log_tfp),1,0)
  ) %>%
  add_count(countryname) %>%
  group_by(countryname) %>%
  mutate(total_missing = sum(count_missing)
  ) %>%
  filter(total_missing <= 1)

########
# Sub Section - Define Regional variable
########
region_crosswalk <- read_excel(here('data','raw','regional_classification_tblDJO.xlsx'))
df_tfp <- left_join(df_tfp,region_crosswalk, by='countryname')

# Write data to be read into Stata
write.dta(df_tfp, file = here('data','output','df_tfpv2.dta'))
