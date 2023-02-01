################################################################################
# This program houses the Solow Growth model.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 12/7/2021
#
################################################################################

#Build model object
solowv2 <- function(steps, K_o, A, L_o, alpha, s, n, delta) {
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # This function solves the Solow Growth Model with TFP as a function of 
  # temperature for a given country. Below is a brief description of inputs:
  #
  # steps: Number of years in the future to simulate, cannot exceed temperature
  # projection data length.
  #
  # K_o: Initial condition capital stock.
  # L_o: Initial condition labor stock (equal to the population)
  # A_o: Initial condition TFP.
  #
  # alpha: Share of capital relative to labor.
  # s: savings rate
  # n: population
  # gamma_tildae: Non-temperature component of TFP/TFP growth.
  # delta: Capital Stock depreciation rate.
  # temp: Temperature
  # dtemp: Temperature growth rate
  # precip: average historic precipitation
  #
  # gamma1, gamma2, beta1, beta2: Coefficients retrieved from climate regressions,
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Set Initial conditions for K and L (year 2010)
  L = c(L_o)
  K = c(K_o)
  
  for (t in 1:steps) {
   
    # Output in period t
    Y_t <- A[t]*(K[t]^alpha)*(L[t])^(1-alpha)
    
    #Investment in period t
    I_t <-s*Y_t
    
    #Capital in period t+1
    Kprime <- K[t]*(1-delta) + I_t
    Lprime <- (1+n)*L[t]
    
    #Store results
    K = c(K, Kprime)
    L = c(L, Lprime)
    
    if (t ==1) {
      Y <- Y_t
      year <-2010
   } else { 
     Y = c(Y, Y_t)
     year <- c(year, year[t-1] + 1)
   }
  }
  I = s*Y
  C = Y- I
  
  solow_output <- cbind(A, Y, C, I, K[1:91], L[1:91], year)
  return(solow_output)
}
    
  
