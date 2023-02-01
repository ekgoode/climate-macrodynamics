################################################################################
# This function simulates forward the time-path of GDPPC/TFP implied by
# estimated coefficients.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 5/3/2022
################################################################################

regSim <- function(steps, mean_tfe, mean_precip, mean_precip2, temp, dtemp, dtemp2,
                   cfe, alpha_1, gamma_1, gamma_2, 
                   beta_1, beta_2, rho_1, rho_2, dx_o, x_o
                   ) {
  
  #Values in 2010 (first year of the simulation)
  dx = c(dx_o)
  
  #x = logged GDP or logged TFP
  x = c(x_o)
  
  for (i in 1:steps){
    
    #Find da and next period 
    dxprime <- cfe + mean_tfe + alpha_1*dx[i] + gamma_1*temp[i+1] +
      gamma_2*(temp[i+1]^2) + beta_1*dtemp[i+1] + beta_2*dtemp2[i+1] + rho_1*mean_dprecip + rho_2*mean_dprecip2
    
    dx <- c(dx, dxprime)
    x <- c(x, x[i]+dxprime) #Log of x
  }
  X =  exp(x) #Un-log x
  return(X)
}
