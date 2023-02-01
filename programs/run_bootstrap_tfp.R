################################################################################
# This program simulates the time-path of GDPPC implied by
# regressing tfp on change in temperature, among other weather
# variables, obtained via estimation over 1000 bootstrap samples.
#
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 5/20/2022
#
################################################################################

#Set specification(s) ((1): growth effects, (2), level effects, (3), growth +level)
regs <- c(3)
bootNames <- c('bootstrap_tfp_ge','bootstrap_tfp_le','bootstrap_tfp_both')

for (reg in regs) {
  
  #Load in bootstrap results
  df_boot <- read.csv(here('data','output','bootstrap_files',paste0(bootNames[reg],'.csv'))) %>%
    filter(name != "") %>%
    mutate(name = gsub('o.','.',name),
           name = gsub('b.','.',name))

  #Time boot loop
  start.time <- Sys.time()
  
  for (boots in 1:1000) {
    
    #Pull coefficients for particular run
    boot_coefs <- df_boot %>% filter(index == boots)
    alpha_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'lag1_delta_l._tfp')]
    gamma1_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'temp')]
    gamma2_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'temp2')]
    beta1_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'dtemp')]
    beta2_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'dtemp2')]
    rho1_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'dprecip')]
    rho2_boot <- boot_coefs$bootmat1[(boot_coefs$name == 'dprecip2')]
    
    #Get mean time fixed effect
    year1961_ind <- which(boot_coefs$name == '1961.year')
    year2010_ind <- which(boot_coefs$name == '2010.year')
    boot_tfe <- boot_coefs[seq(year1961_ind,year2010_ind,1),]
    
    boot_mean_tfe <- mean(as.numeric(boot_tfe[-1,1]))
    
    #This prevents simulation function from breaking when it bumps into NULL/NA values
    if (length(gamma1_boot)==0) {
      gamma1_boot <- 0
      gamma2_boot <- 0 
    }
    if (length(beta1_boot)==0) {
      beta1_boot <- 0
      beta2_boot <- 0
    }
    
    #This pulls in the 'country profile' for the bootstrapped sample
    country_profile <- read.csv(here('data','output','bootstrap_files',paste0(bootNames[reg],'_countryCount',as.character(boots),'.csv'))) %>%
      group_by(cname) %>%
      summarize(max(country_count)) %>%
      rename(ccount='max(country_count)')
    
    #Create clist
    clist_boot <- as.list(country_profile$cname)
    
    #This is the final list of all countries that we use in the regression but don't have temperature data for
    reg_countries2 <- as.data.frame(unlist(clist3[!(clist3 %in% clist4)]))
    reg_countries2 <- reg_countries2 %>%
      rename(countryname2 = "unlist(clist3[!(clist3 %in% clist4)])")
    missing_countries2 <- reg_countries2$countryname2
    
    for (country in clist_boot) {
      
      #Get country count
      ccount <- country_profile$ccount[(country_profile$cname == country)]
      
      #Get common name of country
      if (country == "CuraÃ§ao") {
        country <- 'Curaçao'
      }
      
      #Get country fixed effect(country-specific fixed effect + country-specific, post-1990 fixed effect)
      cid <- cname_cw$var[cname_cw$countryname == country]
      cfe <- as.numeric(boot_coefs$bootmat1[boot_coefs$name == cid]) 
      
      country2 <- df_sim$kmni_countries[df_sim$countryname2 == country]
      country2 <- country2[[1]]
      
      #Get initial values for simulation (Y, dY, and 3 lags of dY)
      y_o <- df_sim$log_tfp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      dy_o <- df_sim$delta_log_tfp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      dy_m1 <- df_sim$delta_log_tfp[(df_tfp$countryname == country & df_tfp$year == 2009)]
      
      #Get historic avg precipitation
      mean_dprecip <- mean(df_sim[df_sim$kmni_countries == country2,]$dprecip, na.rm = TRUE)
      mean_dprecip2 <- mean(df_sim[df_sim$kmni_countries == country2,]$dprecip2, na.rm = TRUE)
      
      #Get 2010 precip & precip2
      dprecip_2010 <- df_sim[(df_sim$kmni_countries == country2 & df_sim$year == 2010),]$dprecip
      dprecip2_2010 <- df_sim[(df_sim$kmni_countries == country2 & df_sim$year == 2010),]$dprecip2
      
      ############################################################################
      #Get temperature projection
      ############################################################################
      #Linearly interpolate temperature projections for countries with missing data
      if (country %!in% present_countries2) {
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
          res <- try(silent=TRUE, {
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
      
      a_o <- df_sim$log_tfp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      da_o <- df_sim$delta_log_tfp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      da_1 <- df_sim$delta_log_tfp[(df_tfp$countryname == country & df_tfp$year == 2011)]
      
      #Initial da and a for Climate Change scenario (cc)
      da_occ <- c(da_o)
      a_occ <- c(a_o)
      
      #Initial da and a for No Climate Change scenario (nocc)
      da_onocc <- c(da_o)
      a_onocc <- c(a_o)
      
      #Run simulation
      resultsCCboot <- bootSimv2(steps=90, boot_mean_tfe, mean_dprecip, mean_dprecip2, 
                               temp, dtemp, dtemp2, cfe, alpha_boot,
                               gamma1_boot, gamma2_boot, beta1_boot, beta2_boot, rho1_boot, rho2_boot,
                               da_occ, a_occ)
      
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
      
      #Run climate change (cc) and no climate change (nocc) scenarios
      solow_cc <- solowv2(steps=91, K_o, resultsCCboot$X, L_o, alpha, s, n, delta)
      solow_nocc <- solowv2(steps=91, K_o, resultsCCboot$X_nocc, L_o, alpha, s, n, delta)
      solow_cc <- as.data.frame(solow_cc)
      solow_nocc <- as.data.frame(solow_nocc)
      
      year_vec <- 2010
      for (c in 2011:2100) {
        year_vec <- c(year_vec, c)
      }
      
      #Get percent difference between scenarios in year 2100
      y2100_cc <- (solow_cc$Y[(solow_cc$year==2100)])*ccount
      y2100_nocc <- (solow_nocc$Y[(solow_nocc$year==2100)])*ccount

      if (country == clist_boot[1]) {
        resultsAll_model_reg_boot <- cbind(country,(solow_cc$Y*ccount),(solow_nocc$Y*ccount), year_vec, boots)
      } else {
        resultsAll_model_new <- cbind(country,(solow_cc$Y*ccount),(solow_nocc$Y*ccount), year_vec, boots)
        resultsAll_model_reg_boot <- rbind(resultsAll_model_reg_boot, resultsAll_model_new)
      }
    } # end of country loop
    
    print(paste0("Run ", boots, " completed..."))
    if (boots == 1) {
      diff_all_boot <- resultsAll_model_reg_boot
    } else {
      diff_all_boot <- rbind(diff_all_boot, resultsAll_model_reg_boot)
    }
  } # end of boot loop
  
  #Time the simulation
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  resultsAll <- as.data.frame(diff_all_boot) %>%
    group_by(boots, year_vec) %>%
    mutate(world_boot_cc = sum(as.numeric(V2)),
           world_boot_nocc = sum(as.numeric(V3))) %>%
    filter(country %in% c('United States', 'France', 'Ethiopia', 'India', 'China', 'Brazil')) %>%
    ungroup() %>%
    mutate(world_diff = ((world_boot_cc-world_boot_nocc)/world_boot_nocc)*100,
           diff = ((as.numeric(V2)-as.numeric(V3))/as.numeric(V3))*100) %>%
    group_by(country, year_vec) %>%
    mutate(lower95 = quantile(diff,probs = 0.025, na.rm=TRUE),
           upper95 = quantile(diff,probs = 0.975, na.rm=TRUE),
           world_lower95 = quantile(world_diff,probs = 0.025, na.rm=TRUE),
           world_upper95 = quantile(world_diff,probs = 0.975, na.rm=TRUE))

  write.csv(resultsAll, here('data','output',paste0('Result_',bootNames[reg],'.csv')))
} #End of reg loop



