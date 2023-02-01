################################################################################
# This program runs the analysis for the climate regressions project.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/12/2022
################################################################################

################################################################################
# Section 0: Define run options
################################################################################

#Set specification(s)
regs <- c(1,2,3)

#Get list of countries from data
country_list <- df_sim %>%
  distinct(countryname)
country_list <- as.list(unique(df_sim$countryname))
country_list <- gsub("Curaçao","Curacao",country_list)

#Remove missing countries
country_list <- country_list[!(country_list %in% missing_countries)]

#Use crosswalk
country_vec <- unlist(country_list)
for (i in 1:nrow(crosswalk)) {
  incorrect_name = as.character(crosswalk[i,1])
  index = which(country_vec == incorrect_name)
  correct_name = as.character(crosswalk[i,2])
  country_vec[index] <- correct_name
}

country_vec <- gsub(" ","_",country_vec)
################################################################################
# Section 1: Load and Reformat Estimates
################################################################################

#Load in estimates
gdppc_regs <- read.csv(here('data','output','regtable2.csv'))

#Load in cid cross-walk (convert from encoded countryname var to countrynames)
cname_cw <- read_xlsx(here('data','raw','stata_cid_crosswalk.xlsx'))

#Delete unnecessary rows
gdppc_regs = gdppc_regs[-seq(1,nrow(gdppc_regs),2),]

#Remove weird characters
gdppc_regs[] <- lapply(gdppc_regs, function(x) gsub("=", "", x, fixed = TRUE))
gdppc_regs[] <- lapply(gdppc_regs, function(x) gsub("\"", "", x, fixed = TRUE))
gdppc_regs[] <- lapply(gdppc_regs, function(x) gsub("*", "", x, fixed = TRUE))

#rename columns
gdppc_regs <- gdppc_regs %>%
  rename(
    'var' = 'X.',
    'reg1' = 'X..1.',
    'reg2' = 'X..2.',
    'reg3' = 'X..3.',
  )

cname_cw <- cname_cw %>%
  mutate(cid = paste0(cid,'.cname')) %>%
  rename(var = cid)

#Find which countries are in regression but not in our country vector
c1_ind <- which(gdppc_regs$var == "1.cname")
cend_ind <- which(gdppc_regs$var == "183.cname")
cindex_list <- as.data.frame(gdppc_regs[c1_ind:cend_ind,1])
cindex_list <- cindex_list %>%
  rename('var' = "gdppc_regs[c1_ind:cend_ind, 1]")

#This is the list of countries included in our regression
reg_countries <- left_join(cindex_list, cname_cw, by = 'var')

#Create list of countries to loop over
clist3 <- as.list(unique(reg_countries$countryname))
clist4 <- as.list(unique(df_sim$countryname2[df_sim$kmni_countries %in% country_vec]))

#This is the final list of all countries that we use in the regression but don't have temperature data for
reg_countries2 <- as.data.frame(unlist(clist3[!(clist3 %in% clist4)]))
reg_countries2 <- reg_countries2 %>%
  rename(countryname2 = "unlist(clist3[!(clist3 %in% clist4)])")
missing_countries2 <- as.list(reg_countries2$countryname2)

#This is the list of temperature changes from BHM
temporary_projections <- read.csv(here('data','raw','CountryTempChange_RCP85_NA.csv'))
temporary_projections <- temporary_projections %>%
  select(Tchg, countryname2)
temporary_projections$countryname2 <- recode(temporary_projections$countryname2,
                                             'Democratic Republic of the Congo' = 'Congo, Dem. Rep.',
                                             'Republic of Congo' = 'Congo, Rep.',
                                             'Saint Vincent' = 'St. Vincent and the Grenadines',
                                             'Tobago' = 'Trinidad and Tobago')

missing_dtemps <- left_join(reg_countries2, temporary_projections, by='countryname2')

#Get average of the time fixed effects
year1960_ind <- which(gdppc_regs$var == '1961.year')
year2010_ind <- which(gdppc_regs$var == '2010.year')
gdppc_tfe <- gdppc_regs[seq(year1960_ind,year2010_ind,1),]

################################################################################
# Section 3: Loop over estimates
################################################################################
for (reg in regs) {
  
  #Get average time fixed effects
  mean_tfe <- mean(as.numeric(gdppc_tfe[-1,reg+1]))

  #Get coefficients from regression
  alpha_1 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'lag1_delta_log_gdppc'])
  gamma_1 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'temp'])
  gamma_2 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'temp2'])
  beta_1 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'dtemp'])
  beta_2 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'dtemp2'])
  rho_1 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'dprecip'])
  rho_2 <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == 'dprecip2'])
  
  #This corrects for unestimated coefficients appearing as NA rather than 0
  if (is.na(gamma_1))  {
    gamma_1 <- 0
    gamma_2 <- 0 
  }
  if (is.na(beta_1))  {
    beta_1 <- 0
    beta_2 <- 0
  }

    for (country in clist3){
    
    #Print country 
    print(country)
    
    #Fix south korea
    if (country =="Korea, Rep") {
      country = "Korea, Rep."
    }
    
    #Get common name of country
    country2 <- df_sim$kmni_countries[df_sim$countryname2 == country]
    country2 <- country2[[1]]

    #Get initial values for simulation (y, dy, and 3 lags of dY)
    y_o <- df_sim$log_gdppc[(df_tfp$countryname == country & df_tfp$year == 2010)]
    dy_o <- df_sim$delta_log_gdppc[(df_tfp$countryname == country & df_tfp$year == 2010)]
    Y_o <- df_tfp$gdppc[(df_tfp$countryname == country & df_tfp$year == 2010)]
    

    #Get historic growth rate of precipitation
    mean_dprecip <- mean(df_sim[df_sim$kmni_countries == country2,]$dprecip, na.rm = TRUE)
    mean_dprecip2 <- mean(df_sim[df_sim$kmni_countries == country2,]$dprecip2, na.rm = TRUE)
    
    ############################################################################
    #Get temperature projection
    ############################################################################
    
    #Pull for missing countries
    if (country %in% missing_countries2) {
      #Linear interpolation between two end-points
      t_2010 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      t_2100 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)] + missing_dtemps$Tchg[(missing_dtemps$countryname2 == country)]
      slope <- (t_2100-t_2010)/(2100-2010)
      temp <- c(t_2010)
      temp_nocc <- c(t_2010)
      dtemp <- c(NA)
      dtemp_nocc <- c(NA)
      for (i in 1:90) {
        year <- 2010 +i
        tempnew <- temp[i]+slope
        temp <- c(temp, tempnew)
        temp_nocc <- c(temp_nocc, t_2010)
        
        dtemp <- c(dtemp,slope)
        dtemp2 <- c(NA)
        for (i in 2:length(temp)) {
          dtemp2 <- c(dtemp2,((temp[i]^2)-(temp[i-1]^2)))
        }
        dtemp_nocc <- c(dtemp_nocc,0)
        
      }
    } else {
      #Pull for non-missing countries
      factor_name = paste0("factor(countryname)", country2)
      files <- list.files(path = here('data','climProj',country2,'atlas', 'series','CMIP5',
                               'rcp85','monthly_dump1',country2), pattern = "*.txt")
      
      df_list <- list()
      
      for (i in 1:length(files)){
        iter <- i
        res <- try(silent = TRUE, {
          df_list[[1]] <- read.table(here('data','climProj',country2,'atlas', 'series','CMIP5',
                                          'rcp85','monthly_dump1',country2, files[i]
                                          )
                                     )
        }
        )
        if(inherits(res, "try-error" )& (!(iter < 40))) {
          next
        }
      }
      
      CMIP5 <- bind_rows(df_list, .id = "projection_type")
      
      #Average temperature projection
      CMIP5avg <- CMIP5 %>%
        mutate(year = V3) %>%
        group_by(year) %>%
        mutate(tavg = mean(V2),
               kmni_countries = country2,
        ) %>%
        filter(projection_type == 1) %>%
        ungroup()   %>% 
        filter(year >= 2009) %>% 
        select(year, tavg)
      
      CMIP5avg <- CMIP5avg %>%
        mutate(tproj_2010 = CMIP5avg[(CMIP5avg$year == 2010),]$tavg,
               tdiff_2010 = CMIP5avg$tavg - tproj_2010, 
               #Historical 2010 temperature
               thist_2010 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)],
               #Project future temperature
               temp = thist_2010 + tdiff_2010
        )
      
      #Use historical temperature for 2009
      CMIP5avg[(CMIP5avg$year == 2009),]$temp <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2009)]

      #Find dtemp and dtemp2
      CMIP5avg <- CMIP5avg %>%
        mutate(temp2 = temp^2,
               dtemp = temp - lag(temp),
               dtemp2 = temp^2 - lag(temp^2))%>%
        #Adjust range to go from 2010-2010
        filter(year >=2010)
      
      temp <- CMIP5avg$temp  
      temp2 <- CMIP5avg$temp2
      dtemp <-CMIP5avg$dtemp   
      dtemp2 <-CMIP5avg$dtemp2   
      
      noccvars <- CMIP5avg %>%
        mutate(temp_nocc = temp[1],
               dtemp_nocc = 0
        )
      temp_nocc <- c(noccvars[['temp_nocc']])
      dtemp_nocc <- c(noccvars[['dtemp_nocc']])
    }
    ############################################################################
    # END get temperature projection
    ############################################################################
    
    #Get country fixed effect
    cid <- cname_cw$var[cname_cw$countryname == country]
    cfe <- as.numeric(gdppc_regs[[reg+1]][gdppc_regs$var == cid])  

    
    #Initial dY and Y for Climate Change scenario (cc)
    dY_occ <- c(dy_o)
    Y_occ <- c(y_o)
    
    #Initial dY and Y for No Climate Change scenario (nocc)
    dY_onocc <- c(dy_o)
    Y_onocc <- c(y_o)
    
    ############################################################################
    # Simulate output forward
    ############################################################################
    Y_cc <- regSim(steps=90, mean_tfe, mean_dprecip, mean_dprecip2, 
                        temp, dtemp, dtemp2, cfe, alpha_1,
                        gamma_1, gamma_2, beta_1, beta_2, rho_1, rho_2,
                        dY_occ, Y_occ)
    
    Y_nocc <- regSim(steps=90, mean_tfe, mean_dprecip, mean_dprecip2, 
                        temp_nocc, dtemp_nocc, dtemp_nocc, cfe, alpha_1,
                        gamma_1, gamma_2, beta_1, beta_2, rho_1, rho_2,
                        dY_onocc, Y_onocc)

    Ydiff <- (((Y_cc[91])-(Y_nocc[91]))/((Y_nocc[91])))*100
    
    year_vec <- 2010
    for (c in 2011:2100) {
      year_vec <- c(year_vec, c)
    }
    
    #Save results
    if (country == clist3[1]) {
      results_rf_reg <- c(country, Ydiff, Y_cc[91], Y_nocc[91], temp[1], temp[91], Y_o)
      resultsAll_rf_reg <- cbind(country,Y_cc,Y_nocc, year_vec)
    } else {
      newrow <- c(country,Ydiff, Y_cc[91], Y_nocc[91], temp[1],temp[91], Y_o)
      results_rf_reg <- rbind(results_rf_reg, newrow)
      resultsAll_rf_new <- cbind(country,Y_cc,Y_nocc, year_vec)
      resultsAll_rf_reg <- rbind(resultsAll_rf_reg,resultsAll_rf_new)
    }
  }
  
  #Calculate results for the world
  results_rf_reg <- as.data.frame(results_rf_reg)
  Yworld_2100_cc <- sum(as.numeric(results_rf_reg$V3))
  Yworld_2100_nocc <- sum(as.numeric(results_rf_reg$V4))
  Ydiff <- ((Yworld_2100_cc - Yworld_2100_nocc)/Yworld_2100_nocc)*100
  
  #Calculate time-path of Y for the world
  resultsAll_rf_reg <- as.data.frame(resultsAll_rf_reg) %>%
    mutate(Y_diff = ((as.numeric(Y_cc)-as.numeric(Y_nocc))/as.numeric(Y_nocc))*100) %>%
    group_by(year_vec) %>%
    mutate(world_Y_cc = sum(as.numeric(Y_cc)),
           world_Y_nocc = sum(as.numeric(Y_nocc)),
           world_Y_diff = ((world_Y_cc - world_Y_nocc)/world_Y_nocc)*100
           ) %>%
    filter(country %in% c('Brazil', 'China', 'India', 'United States', 'Ethiopia', 'France'))  
  
  #Find GDP-Weighted world temp
  Yworld_2010<-sum(as.numeric(results_rf_reg$V7))
  results_rf_reg <- results_rf_reg %>% 
    mutate(yfrac_2010 = as.numeric(V7)/Yworld_2010,
           T2010_yfrac = as.numeric(V5)*yfrac_2010,
           T2100_yfrac = as.numeric(V6)*yfrac_2010)
  
  T2010_world <-sum(as.numeric(results_rf_reg$T2010_yfrac))
  T2100_world <-sum(as.numeric(results_rf_reg$T2100_yfrac))
  
  #Drop extra columns
  results_rf_reg$yfrac_2010 <- NULL
  results_rf_reg$T2010_yfrac <- NULL
  results_rf_reg$T2100_yfrac <- NULL
  results_rf_reg$V7 <- NULL
  
  #Add row for world to data frame
  newrow <- c("World",Ydiff, Yworld_2100_cc, Yworld_2100_nocc, T2010_world,T2100_world)
  results_rf_reg<- as.data.frame(rbind(results_rf_reg, newrow)) %>%
    rename(country = V1, Ydiff=V2, y2100_cc = V3, y2100_nocc = V4, T2010 = V5, T2100 = V6)
  
  #Assign result matrix to new object, unique to regression
  newresultsname <- paste0('results_rf_reg',as.character(reg))
  assign(newresultsname, as.data.frame(results_rf_reg))
  newresultsnameAll <- paste0('resultsAll_rf_reg',as.character(reg))
  assign(newresultsnameAll, resultsAll_rf_reg)
  rm(results_rf_reg)
  rm(resultsAll_rf_reg)

} #End of reg loop

