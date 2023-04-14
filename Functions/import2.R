# Define new import function in which the likelihood is rescaled in order to handle
# larger number of importation events
import2 <- function(y_obs, rho, profile,
                    threshold_z =  1e3, threshold_import = 5e3, CI = 0.95){
  
  z <- matrix(seq(1,threshold_z),threshold_z,1)
  g0 <- dbinom(0,matrix(z,1,threshold_z,byrow=TRUE),rho)
  
  profile$import <- 0:threshold_import
  profile$Lk_import <- rep(0,threshold_import+1)
  
  ML <- max(profile$Likelihood) # to rescale the likelihood
  
  
  for (i in 1:length(profile$theta)){
    R_eff <- R_eff_poisson(profile$theta[i])
    f <- f_z_poiss(z,1,R_eff$R_effective)*R_eff$P_extinction
    p_obs<- (1-g0 %*% f)
    
    
    temp <- dnbinom(profile$import, length(y_obs), p_obs, log = TRUE) +
      profile$Likelihood[i] - ML # rescaling (useful when large number of importations)
    profile$Lk_import <- profile$Lk_import + exp(temp)
  }
  profile$Lk_import <- log(profile$Lk_import)
  
  max_likelihood <- theta_max_likelihood(profile$import,profile$Lk_import,CI)
  
  return(list(theta_max_likelihood = max_likelihood$theta,
              max_likelihood = max_likelihood$likelihood,
              lower_theta =  max_likelihood$lower_theta,
              upper_theta = max_likelihood$upper_theta) )
}
