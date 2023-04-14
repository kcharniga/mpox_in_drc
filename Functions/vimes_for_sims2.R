vimes_for_sims2 <- function(sim_dat, pi, q, perfect){
  
  library(dplyr)
  library(vimes)
  library(incidence)
  
  sim1 <- sim_dat
  
  # Computing pairwise differences in cases in time and space
  
  # Distances between dates are computed as # of days
  d_dates <- dist(sim1$date_onset)
  
  # Need to use Vicenty inverse formula for ellipsoids rather than
  # Euclidean distances because locations are very far apart
  sim1$new_id <- paste("patient",1:nrow(sim1), sep = "_")
  
  locations <- dplyr::select(sim1, new_id, Lat, Lon) %>%
    dplyr::rename(name = new_id, lat = Lat, lon = Lon)
  #d_geo <- round(GeoDistanceInMetresMatrix(locations) / 1000) # gives km
  #d_geo <- unname(d_geo) # Remove attributes
  
  # These distances are way to big (max is 20,000 km!). Also get a warning
  # that something did not converge
  
  # Try calculating Euclidean distance
  locations2 <- dplyr::select(locations, -name)
  d_geo <- fields::rdist(locations2)
  max(d_geo) # Max distance is 593 km which is fine bc 
  # it would lie in the square area used to simulate data
  # (hypotenuse of triangle is 607 with l and w of 446 and 413)
  
  # Format, match, and plot distance data
  d_all <- vimes_data(dates = d_dates, geo = d_geo)
  
  # Defining cutoff distances above which cases are considered not linked 
  # by transmission 
  
  # Extract the upper limit of the pairwise differences
  max(d_dates) # Find out what the upper value needs to be
  date_vect <- seq(0,max(d_dates), 1) # Make a vector of days from 0 up to maximum value 
  
  # Specify the parameters of the serial interval distribution
  # used observed serial interval data for smallpox
  # From Nishiura 2007 
  gamma_shape <- 18.505
  gamma_rate <- 1.159
  gamma_scale <- 1/gamma_rate
  
  # Spatial kernel parameters
  # From WHO contact tracing data
  rayleigh_mean <- 0.13
  
  ## find Rayleigh parameters to match this mean
  rayleigh_scale <- rayleigh_mean / sqrt(acos(-1)/2)
  
  # Distance functions for the two types
  f_temporal <- fpaircase(type = "temporal", gamma_shape = gamma_shape,
                          gamma_scale = gamma_scale)
  
  f_spatial <- fpaircase(type = "spatial", sd_spatial = rayleigh_scale)
  
  ###########
  # Run vimes
  ###########
  
  # Identify clusters of cases linked by transmission, for various
  # cutoff choices and reporting rates. We plot the results by coloring
  # cases identified as belonging to the same outbreak cluster, i.e. 
  # cases identified as being linked by local transmission. Cases shown
  # in grey are singletons who are not linked by transmission to any other
  # observed case.
  
  ### function used to get results for a certain cutoff and reporting rate
  get_res <- function(d_all, q, pi, f_temporal, f_spatial,
                      type = c("all", "temporal","spatial")) {
    
    type <- match.arg(type)
    
    ## get the cutoffs
    cuts <- c(temporal = get_quantiles(f_temporal, q, pi = pi), 
              spatial = get_quantiles(f_spatial, q, pi = pi))
    
    if (type == "all") {
      ## use vimes
      out <- vimes(d_all, cutoff = cuts,
                   graph.opt = vimes.graph.opt(col.pal = funky))
    } else if (type == "temporal") {
      out <- vimes(vimes_data(dates = d_all$dates), cutoff = cuts["temporal"],
                   graph.opt = vimes.graph.opt(col.pal = funky))
    } else if (type == "spatial") {
      out <- vimes(vimes_data(geo = d_all$geo), cutoff = cuts["spatial"],
                   graph.opt = vimes.graph.opt(col.pal = funky))
    }
    
    return(out)
    
  }
  
  ### use the function above to generate results for several combinations of p and
  ### pi We assume a reporting rate of 25% in our main
  ### analyses, and reporting rates of 10 and 50% respectively in two extreme
  ### scenarios considered in sensitivity analyses.
  combi <- expand.grid(p = q,
                       pi = pi)
  
  quantile_pretty <- signif(combi$p*100, 4)
  quantile_pretty <- paste0(quantile_pretty, "%")
  res <- vector(nrow(combi), mode = "list")
  
  for (i in 1:nrow(combi)) {
    res[[i]] <- get_res(d_all, combi[i, 1],
                        combi[i, 2], f_temporal,
                        f_spatial)
  }
  
  ##########################
  # Take results from vimes and calc TP and TN rates
  ##########################
  
  if (perfect == 1){
    # No re-numbering of clusters needed for perfect reporting
    sim_clust_membership <- sim_dat$cluster_member
  } else{
    # Cluster membership as simulated
    sim_clust_membership <- sim_dat$clust_renumbered
  }
  
  # function to compute the true positive and true negative rates
  get_TPR_TNR <- function(est_clust_membership, sim_clust_membership) {
    ### look at pairs of individuals and whether they are in the same cluster or not
    ### in simulation and estimation
    
    sim_same_clust <- as.matrix(dist(sim_clust_membership, method="manhattan")) == 0
    est_same_clust <- as.matrix(dist(est_clust_membership, method="manhattan")) == 0
    
    sim_same_clust[lower.tri(sim_same_clust, diag = TRUE)] <- NA
    est_same_clust[lower.tri(est_same_clust, diag = TRUE)] <- NA
    
    sim_same_clust_vect <- na.omit(as.vector(sim_same_clust))
    est_same_clust_vect <- na.omit(as.vector(est_same_clust))
    ## see
    ## https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12968
    ## for the following param definitions
    
    ## The true positive rate (TPR) defined as the proportion of individuals
    ## belonging to the same population which were indeed clustered together by
    ## the method.
    
    TPR <- sum(est_same_clust_vect[which(sim_same_clust_vect)]) / sum(sim_same_clust_vect)
    
    ## The true negative rate (TNR) defined as the proportion of individuals
    ## which did not belong to the same population and were adequately placed in
    ## different groups by the method
    TNR <- sum(!est_same_clust_vect[which(!sim_same_clust_vect)]) / sum(!sim_same_clust_vect)
    
    return(c(TPR = TPR, TNR = TNR))
  }
  
  ## use this function to compute the TPR and TNR for all the cutoffs we
  ## considered
  all_TPR_TNR <- as.data.frame(t(sapply(1:length(res),
                                        function(i) get_TPR_TNR(as.vector(res[[i]]$clusters$membership),
                                                                sim_clust_membership))))
  
  all_TPR_TNR$mean <- rowMeans(all_TPR_TNR)
  
  all_TPR_TNR$quantile <- combi$p
  
  max_mean <- max(all_TPR_TNR$mean) 
  optimal_cutoff <- all_TPR_TNR[which(all_TPR_TNR$mean == max_mean), ] # What if there is a tie?? In Cori et al., they kept all the cutoffs with highest performance so a single simulation can contribute to multiple cutoffs
  optimal_cutoff <- optimal_cutoff$quantile
  ##########################################
  
  # Estimate the underlying reproduction number and number of imported cases
  library(branchr)
  
  compute_R  <- function(cl_size, rho) {
    profile <- profile_likelihood(y_obs = cl_size, 
                                  rho = rho, 0.01, 20)
    
    R_estimate <- theta_max_likelihood(profile$theta,
                                       profile$Likelihood, 
                                       0.95)
    
    R <- c(central = R_estimate$theta_max_likelihood, 
           low = R_estimate$lower_theta, 
           up = R_estimate$upper_theta)
    
    import2 <- import2(y_obs = cl_size, 
                       rho = rho, 
                       profile, 
                       threshold_z =  1e3, 
                       threshold_import = 5e3, 
                       CI = 0.95)
    
    unobs <- c(central = import2$theta_max_likelihood,
               low = import2$lower_theta,
               up = import2$upper_theta)
    
    return(list(R, unobs))
  }
  
  clust_size <- lapply(res, function(i) i$clusters$size)
  rho <- combi[, 2]
  R_estimate_and_imports <- lapply(1:length(clust_size),
                                   function(i) compute_R(clust_size[[i]], rho[i]))
  R_estimates <- sapply(1:length(R_estimate_and_imports), 
                        function(i) R_estimate_and_imports[[i]][[1]])
  N_unobs_estimates <- sapply(1:length(R_estimate_and_imports), 
                              function(i) R_estimate_and_imports[[i]][[2]])
  N_tot_estimates <- N_unobs_estimates + 
    matrix(rep(lengths(clust_size), 3), 
           nrow = 3, byrow = TRUE)
  n_days <- diff(range(sim_dat$date_onset))
  rate_import_tot_days <- N_tot_estimates / as.numeric(n_days)
  rate_import_tot_year <- rate_import_tot_days*365
  
  #######
  # Add all results to a list and save
  
  all_res <- list(all_TPR_TNR = all_TPR_TNR,
                  optimal_cutoff = optimal_cutoff,
                  R_estimates = R_estimates,
                  rate_import_tot_year = rate_import_tot_year)
  
  return(all_res)
  
}
