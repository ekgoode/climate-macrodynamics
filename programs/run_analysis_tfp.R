################################################################################
# This program runs the analysis for the climate regressions project.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/12/2022
################################################################################

################################################################################
# Section 0: Define run options
################################################################################

#Set specification(s) ((1): growth effects, (2), level effects, (3), growth +level)
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
tfp_regs <- read.csv(here('data','output','regtable1.csv'))

#Load in cid cross-walk (convert from encoded countryname var to countrynames)
cname_cw <- read_xlsx(here('data','raw','stata_cid_crosswalk.xlsx'))

#Delete unnecessary rows
tfp_regs = tfp_regs[-seq(1,nrow(tfp_regs),2),]

#Remove weird characters
tfp_regs[] <- lapply(tfp_regs, function(x) gsub("=", "", x, fixed = TRUE))
tfp_regs[] <- lapply(tfp_regs, function(x) gsub("\"", "", x, fixed = TRUE))
tfp_regs[] <- lapply(tfp_regs, function(x) gsub("*", "", x, fixed = TRUE))

#rename columns
tfp_regs <- tfp_regs %>%
  rename(
    'var' = 'X.',
    'reg1' = 'X..1.',
    'reg2' = 'X..2.',
    'reg3' = 'X..3.'
  )

cname_cw <- cname_cw %>%
  mutate(cid = paste0(cid,'.cname')) %>%
  rename(var = cid)

#Find which countries are in regression but not in our country vector
c1_ind <- which(tfp_regs$var == "1.cname")
cend_ind <- which(tfp_regs$var == "183.cname")
cindex_list <- as.data.frame(tfp_regs[c1_ind:cend_ind,1])
cindex_list <- cindex_list %>%
  rename('var' = "tfp_regs[c1_ind:cend_ind, 1]")

#This is the list of countries included in our regression
reg_countries <- left_join(cindex_list, cname_cw, by = 'var')
clist3 <- as.list(unique(reg_countries$countryname))
clist4 <- as.list(unique(df_sim$countryname2[df_sim$kmni_countries %in% country_vec]))

#This is the final list of all countries that we use in the regression but don't have temperature data for
reg_countries2 <- as.data.frame(unlist(clist3[!(clist3 %in% clist4)]))
reg_countries2 <- reg_countries2 %>%
  rename(countryname2 = "unlist(clist3[!(clist3 %in% clist4)])")
missing_countries2 <- reg_countries2$countryname2

present_countries <- as.data.frame(unlist(clist3[clist3 %in% clist4])) %>%
  rename(countryname2 = "unlist(clist3[clist3 %in% clist4])")
present_countries2 <- present_countries$countryname2


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
year1960_ind <- which(tfp_regs$var == '1961.year')
year2010_ind <- which(tfp_regs$var == '2010.year')
tfp_tfe <- tfp_regs[seq(year1960_ind,year2010_ind,1),]

################################################################################
# Section 2: Loop over estimates
################################################################################
for (reg in regs) {
  
  #Get average time fixed effects
  mean_tfe <- mean(as.numeric(tfp_tfe[-1,reg+1]))
  
  #Get coefficients from regression
  alpha_1 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'lag1_delta_log_tfp'])
  gamma_1 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'temp'])
  gamma_2 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'temp2'])
  beta_1 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'dtemp'])
  beta_2 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'dtemp2'])
  rho_1 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'dprecip'])
  rho_2 <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == 'dprecip2'])
  
  if(reg == 1) {
    optTemp_model_tfp1 <- -(gamma_1)/(2*gamma_2)
    
  }
  if (reg == 2) {
    optTemp_model_tfp2 <- -(beta_1)/(2*beta_2)
  }
  if (reg == 3) {
    optTemp_model_tfp3v1 <- -(gamma_1)/(2*gamma_2)
    optTemp_model_tfp3v2 <- -(beta_1)/(2*beta_2)
  }
  
  #This corrects for unestimated coefficients appearing as NA rather than 0
  if (is.na(gamma_1)) {
    gamma_1 <- 0
    gamma_2 <- 0 
  }
  if (is.na(beta_1)) {
    beta_1 <- 0
    beta_2 <- 0
  }

  #Get results for each country
  for (country in clist3) {
    
    #Print country 
    print(country)
    
    #Fix south korea
    if (country =="Korea, Rep"){
      country = "Korea, Rep."
    }
    
    #Get common name of country
    country2 <- df_sim$kmni_countries[df_sim$countryname2 == country]
    country2 <- country2[[1]]
    
    #Get initial values for simulation (a, da in 2010)
    a_o <- df_sim$log_tfp[(df_tfp$countryname == country & df_tfp$year == 2010)]
    da_o <- df_sim$delta_log_tfp[(df_tfp$countryname == country & df_tfp$year == 2010)]

    #Get historical values of dprecip and dprecip2
    mean_dprecip <- mean(df_sim[df_sim$kmni_countries == country2,]$dprecip, na.rm = TRUE)
    mean_dprecip2 <- mean(df_sim[df_sim$kmni_countries == country2,]$dprecip2, na.rm = TRUE)
    
    ############################################################################
    #Get temperature projection
    ############################################################################
    
    #Linearly interpolate temperature projections for countries with missing data
    if (country %in% missing_countries2) {
      t_2009 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2009)]
      t_2010 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      t_2100 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)] + missing_dtemps$Tchg[(missing_dtemps$countryname2 == country)]
      slope <- (t_2100-t_2010)/(2100-2010)
      temp <- c(t_2010)
      temp_nocc <- c(t_2010)
      dtemp <- c(t_2010-t_2009)
      dtemp_nocc <- c(0)
      for (i in 1:90) {
        year <- 2010 +i
        tempnew <- temp[i]+slope
        temp <- c(temp, tempnew)
        temp_nocc <- c(temp_nocc, t_2010)
        
        dtemp <- c(dtemp,slope)
        dtemp2 <- c(t_2010^2 - t_2009^2)
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
          df_list[[i]] <- read.table(here('data','climProj',country2,'atlas', 'series','CMIP5',
                                          'rcp85','monthly_dump1',country2,files[i]))
        }
        )
        if(inherits(res, "try-error" )& (!(iter < 40))) {
          next
        }
      }
      
      #All projections
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
          CMIP5avg[(CMIP5avg$year == 2009),]$temp <-df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2009)]
        
          #Find dtemp and dtemp2
          CMIP5avg <- CMIP5avg %>%
            mutate(dtemp = temp - lag(temp),
                 dtemp2 = temp^2 - lag(temp^2))%>%
            #Adjust range to go from 2010-2010
            filter(year >=2010)
        
        temp <-CMIP5avg$temp  
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
    #END get temperature projection
    ############################################################################
    
    #Get country fixed effect
    cid <- cname_cw$var[cname_cw$countryname == country]
    cfe <- as.numeric(tfp_regs[[reg+1]][tfp_regs$var == cid])
    
    #Initial da and a for Climate Change scenario (cc)
    da_occ <- c(da_o)
    a_occ <- c(a_o)
    
    #Initial da and a for No Climate Change scenario (nocc)
    da_onocc <- c(da_o)
    a_onocc <- c(a_o)
    
    ############################################################################
    #Start simulations
    ############################################################################
    
    #Simluate TFP forward with and without climate change
    A_cc <- regSim(steps=90, mean_tfe, mean_dprecip, mean_dprecip2, 
                        temp, dtemp, dtemp2, cfe, alpha_1,gamma_1, gamma_2, 
                        beta_1, beta_2, rho_1, rho_2,
                        da_occ, a_occ)
    
    A_nocc <- regSim(steps=90, mean_tfe, mean_dprecip, mean_dprecip2, 
                          temp_nocc, dtemp_nocc, dtemp_nocc, cfe, alpha_1, 
                          gamma_1, gamma_2, beta_1, beta_2, rho_1, rho_2,
                          da_onocc, a_onocc)
    
    PWT_model_data <- df_sim %>%
      select(gr_pop, delta_log_tfp, log_tfp, tfp, investment_share, year, countryname, kmni_countries, delta, rnna, rgdpna) %>%
      filter(kmni_countries == country2) %>%
      mutate(delta_avg = mean(delta, na.rm = TRUE),
             n = mean(gr_pop, na.rm = TRUE),
             s_avg = mean(investment_share, na.rm = TRUE)
      ) %>%
      filter(year == 2010)
    
    # Set initial conditions
    Y_o <- df_tfp$gdppc[(df_tfp$countryname == country & df_tfp$year == 2010)]
    K_o <- df_tfp$rnna[(df_tfp$countryname == country & df_tfp$year == 2010)]
    L_o <- df_tfp$pop[(df_tfp$countryname == country & df_tfp$year == 2010)]

    #Set parameters
    alpha <- (1/3)
    s <- (PWT_model_data[['s_avg']])
    n <- PWT_model_data[['n']]
    delta <- PWT_model_data[['delta_avg']]
    
    #Solow model simulations
    #Run climate change (cc) and no climate change (nocc) scenarios
    solow_cc <- solowv2(steps=91, K_o, A_cc, L_o, alpha, s, n, delta)
    solow_nocc <- solowv2(steps=91, K_o, A_nocc, L_o, alpha, s, n, delta)
    solow_cc <- as.data.frame(solow_cc)
    solow_nocc <- as.data.frame(solow_nocc)
    
    year_vec <- 2010
    for (c in 2011:2100) {
      year_vec <- c(year_vec, c)
    }
    
    #Get percent difference between scenarios in year 2100
    y2100_cc <- solow_cc$Y[(solow_cc$year==2100)]
    y2100_nocc <- solow_nocc$Y[(solow_nocc$year==2100)]
    a2100_cc <- solow_cc$A[(solow_cc$year==2100)]
    a2100_nocc <- solow_nocc$A[(solow_nocc$year==2100)]
    kn2100_cc <- (solow_cc$V5[(solow_cc$year==2100)])/(solow_cc$V6[(solow_cc$year==2100)])
    kn2100_nocc <- (solow_nocc$V5[(solow_nocc$year==2100)])/(solow_nocc$V6[(solow_nocc$year==2100)])
    Ydiff<- ((y2100_cc-y2100_nocc)/(y2100_nocc))*100
    Adiff <- (A_cc[91]-A_nocc[91])/A_nocc[91]*100
    Adiff_panel <- (A_cc-A_nocc)/A_nocc*100
    
    #We want to decompose our main result into TFP & Capital
    dlnY <- log(y2100_cc) - log(y2100_nocc)
    dlnA <- log(a2100_cc) - log(a2100_nocc)
    dlnK.N <- alpha*(log(kn2100_cc) - log(kn2100_nocc))
    
    Kcontribution <- dlnY - dlnA
    Acontribution <- dlnY - dlnK.N
    Yaccounting <- dlnA + dlnK.N
    Ycheck <- Kcontribution + Acontribution
    print(Ydiff)
    
    #Save results
    if (country == clist3[1]) {
      results_model_reg <- c(country, Ydiff, y2100_cc, y2100_nocc, temp[1], temp[91], Adiff, Y_o, Kcontribution, Acontribution, Yaccounting, Ycheck)
      resultsAll_model_reg <- cbind(country,solow_cc$Y,solow_nocc$Y, year_vec,solow_cc$V5,solow_cc$V6,solow_nocc$V5,solow_nocc$V6,solow_cc$A,solow_nocc$A,Adiff_panel)
      
    } else {
      newrow <- c(country,Ydiff, y2100_cc, y2100_nocc, temp[1],temp[91], Adiff, Y_o, Kcontribution, Acontribution, Yaccounting, Ycheck)

     results_model_reg<- rbind(results_model_reg, newrow)
     resultsAll_model_new <- cbind(country,solow_cc$Y,solow_nocc$Y, year_vec,solow_cc$V5,solow_cc$V6,solow_nocc$V5,solow_nocc$V6,solow_cc$A,solow_nocc$A,Adiff_panel)
     resultsAll_model_reg <- rbind(resultsAll_model_reg,resultsAll_model_new)
    }
  } #End Of country loop
  
  #Calculate results for the world
  results_model_reg <- as.data.frame(results_model_reg)
  Yworld_2100_cc <- sum(as.numeric(results_model_reg$V3))
  Yworld_2100_nocc <- sum(as.numeric(results_model_reg$V4))
  Ydiff <- ((Yworld_2100_cc - Yworld_2100_nocc)/Yworld_2100_nocc)*100
  
  #Calculate time-path of Y for the world
  resultsAll_model_reg <- as.data.frame(resultsAll_model_reg) %>%
    mutate(Y_diff = ((as.numeric(V2)-as.numeric(V3))/as.numeric(V3))*100) %>%
    group_by(year_vec) %>%
      mutate(
        world_Y_cc = sum(as.numeric(V2)),
        world_Y_nocc = sum(as.numeric(V3)),
        world_Y_diff = ((world_Y_cc - world_Y_nocc)/world_Y_nocc)*100,
        wKcc = sum(as.numeric(V5)),
        wKncc = sum(as.numeric(V7)),
        wN = sum(as.numeric(V8))
      ) 
    
    #World dataframe
    results_world_reg <- as.data.frame(resultsAll_model_reg) %>%
      group_by(year_vec) %>%
      mutate(
        wYcc = sum(as.numeric(V2)),
        wYncc = sum(as.numeric(V3)),
        wKcc = sum(as.numeric(V5)),
        wKncc = sum(as.numeric(V7)),
        wN = sum(as.numeric(V8))
      ) %>%
      mutate(
        wAcc = wYcc/((wKcc^alpha)*(wN^(1-alpha))),
        wAncc = wYncc/((wKncc^alpha)*(wN^(1-alpha))),
        
        wKNcc = wKcc/wN,
        wKNncc = wKncc/wN,
        
        dlnY = log(wYcc) - log(wYncc), 
        dlnK.N = alpha*(log(wKNcc)-log(wKNncc)),
        dlnA = log(wAcc) - log(wAncc),
        
        Acontribution = dlnY - dlnK.N,
        Kcontribution = dlnY - dlnA,
        Yaccounting = dlnA + dlnK.N
        
      ) %>%
      filter(country %in% c('Brazil'))
  
  #Find GDP-Weighted world temp
  Yworld_2010<-sum(as.numeric(results_model_reg$V8))
  results_model_reg <- results_model_reg %>% 
    mutate(yfrac_2010 = as.numeric(V8)/Yworld_2010,
           T2010_yfrac = as.numeric(V5)*yfrac_2010,
           T2100_yfrac = as.numeric(V6)*yfrac_2010)

  T2010_world <-sum(as.numeric(results_model_reg$T2010_yfrac))
  T2100_world <-sum(as.numeric(results_model_reg$T2100_yfrac))
  
  #Drop extra columns
  results_model_reg$yfrac_2010 <- NULL
  results_model_reg$T2010_yfrac <- NULL
  results_model_reg$T2100_yfrac <- NULL
  results_model_reg$V8 <- NULL
  

  #Add row for world to data frame
  newrow <- c("World",Ydiff, Yworld_2100_cc, Yworld_2100_nocc, T2010_world,T2100_world, 0, 0, 0, 0,0)
  results_model_reg<- as.data.frame(rbind(results_model_reg, newrow)) %>%
    rename(country = V1, Ydiff=V2, y2100_cc = V3, y2100_nocc = V4, T2010 = V5, T2100 = V6, Adiff = V7, Acontribution = V10, Kcontribution = V9, Yaccounting = V11, Ycheck = V12)

  #Assign result matrix to new object, unique to regression
  newresultsname <- paste0('results_model_reg',as.character(reg))
  assign(newresultsname, as.data.frame(results_model_reg))
  newresultsnameAll <- paste0('resultsAll_model_reg',as.character(reg))
  assign(newresultsnameAll, resultsAll_model_reg)
  newresultsnameWorld <- paste0('results_world_reg',as.character(reg))
  assign(newresultsnameWorld, results_world_reg)
  
} #End of reg loop



