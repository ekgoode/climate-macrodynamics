################################################################################
# This function simulates forward the time-path of GDPPC/TFP implied by
# estimates obtained on bootstrap samples of the dataset.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 5/3/2022
################################################################################

bootSimv2 <- function(steps, mean_tfe, mean_dprecip, mean_dprecip2, temp, dtemp, dtemp2, cfe,
                    alpha_boot, gamma1_boot, gamma2_boot, 
                    beta_1, beta_2, rho1_boot, rho2_boot, dx_o, x_o) {
  
  #Values in 2010 (first year of the simulation)
  dx = c(dx_o)
  dx_nocc = c(dx_o)
  
  #x = logged GDP or logged TFP
  x = c(x_o)
  x_nocc = c(x_o)
  
  for (i in 1:steps){
    
    #Find da and next period 
    dxprime_nocc <- cfe + mean_tfe + alpha_boot*dx_nocc[i] + gamma1_boot*temp[1] + 
      gamma2_boot*(temp[1]^2) + rho1_boot*mean_dprecip + rho2_boot*mean_dprecip2
    
    dxprime <- cfe + mean_tfe + alpha_boot*dx[i] + gamma1_boot*temp[i+1] +
      gamma2_boot*(temp[i+1]^2) + beta_1*dtemp[i+1] + beta_2*dtemp2[i+1] + rho1_boot*mean_dprecip + rho2_boot*mean_dprecip2
    
    dx <- c(dx, dxprime)
    dx_nocc <- c(dx_nocc, dxprime_nocc)
    
    x <- c(x, x[i]+dxprime) #Log of x
    x_nocc <- c(x_nocc, x_nocc[i]+dxprime_nocc)
  }
  X =  exp(x) #Un-log x
  X_nocc = exp(x_nocc)
  
  xdf <- as.data.frame(cbind(X,X_nocc))
  return(xdf)
}
