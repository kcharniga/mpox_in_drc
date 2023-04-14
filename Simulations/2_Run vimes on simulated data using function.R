# Run vimes on simulated MPX dataset
# Look at baseline with different cutoffs as well as low and high
# reporting
# 21 Dec 2022

library(dplyr)
library(vimes)
library(incidence)

# Load functions
source("Analysis/Functions/vimes_for_sims2.R")
source("Analysis/Functions/import2.R") # This function is a modified version of the one in branchr and has a rescaled likelihood so it works with a larger number of clusters

library(parallel) 

# Load simulated data
dat <- readRDS("Analysis/Simulations/mpx_sims_21dec2022.RDS")

# Baseline reporting with different cutoffs
sim_dat <- dat[[3]] # Baseline, corresponds to reporting rate of 25%

# Try first 4 sims to make sure it works
#sim_dat <- sim_dat[1:4]

myCluster <- makeCluster(3) # Cluster with 3 nodes

tictoc::tic() # takes 1.7 hours - update

# the libraries are loaded in the vimes_for_sims2 function
clusterExport(cl = myCluster, 
              varlist = c("sim_dat",
                          "vimes_for_sims2", 
                          "import2"), 
              envir=environment())

result <- parLapply(myCluster,                     
                    sim_dat, 
                    fun = vimes_for_sims2,
                    pi = 0.25,
                    q = c(0.5, .90, .95, .95^(1/3), 0.999),
                    perfect = 0) 

stopCluster(myCluster)
tictoc::toc()

# Save results
check <- result[[1]]
#saveRDS(result, "Analysis/Simulations/reconstruction_res1_21dec2022.RDS")

# OUTPUT is a list of 200 items. Each item has R, spillover rate, TPR, TNR, and mean of TPR and TNR, and optimal cutoff

########################

# Baseline
sim_dat <- dat[[3]] # Baseline, corresponds to reporting rate of 25%

myCluster <- makeCluster(3) # Cluster with 3 nodes

tictoc::tic() # Took 46 min.

clusterExport(cl = myCluster, 
              varlist = c("sim_dat",
                          "vimes_for_sims2", 
                          "import2"), 
              envir=environment())


result <- parLapply(myCluster,                     
                    sim_dat, 
                    fun = vimes_for_sims2,
                    pi = c(0.1, 0.4), 
                    q = 0.95,
                    perfect = 0) 

stopCluster(myCluster)
tictoc::toc()

#saveRDS(result, "Analysis/Simulations/reconstruction_res2_21dec2022.RDS")

########################

# Run simulation scenarios using the best cutoff (assuming 95% but could be diff based on above)
sim_dat <- dat[[1]] # Perfect reporting

myCluster <- makeCluster(3) # Cluster with 3 nodes

tictoc::tic() # takes 9 hrs

clusterExport(cl = myCluster, 
              varlist = c("sim_dat",
                          "vimes_for_sims2", 
                          "import2"), 
              envir=environment())


result <- parLapply(myCluster,                     
                    sim_dat, 
                    fun = vimes_for_sims2,
                    pi = 0.25,# 1.0 used to simulate data. See what happens when we mis-specify
                    q = .95^(1/3),
                    perfect = 1) # was perfect reporting used?

stopCluster(myCluster)
tictoc::toc() 

#saveRDS(result, "Analysis/Simulations/sim_res_perfect_reporting_23dec2022.RDS")

########################

# Run simulation scenarios using the best cutoff (assuming 95% but could be diff based on above)
sim_dat <- dat[[2]] # 50% reporting (high)

myCluster <- makeCluster(3) # Cluster with 3 nodes

tictoc::tic() # takes 2 hrs

clusterExport(cl = myCluster, 
              varlist = c("sim_dat",
                          "vimes_for_sims2", 
                          "import2"), 
              envir=environment())


result <- parLapply(myCluster,                     
                    sim_dat, 
                    fun = vimes_for_sims2,
                    pi = 0.25, # 0.5 used to simulate data. See what happens when we mis-specify
                    q = .95^(1/3),
                    perfect = 0) # was perfect reporting used for simulated data?

stopCluster(myCluster)
tictoc::toc()

#saveRDS(result, "Analysis/Simulations/sim_res_high_reporting_23dec2022.RDS")

########################

# Run simulation scenarios using the best cutoff (assuming 95% but could be diff based on above)
sim_dat <- dat[[4]] # 10% reporting (low)

myCluster <- makeCluster(3) # Cluster with 3 nodes

tictoc::tic() # 7 min

clusterExport(cl = myCluster, 
              varlist = c("sim_dat",
                          "vimes_for_sims2", 
                          "import2"), 
              envir=environment())


result <- parLapply(myCluster,                     
                    sim_dat, 
                    fun = vimes_for_sims2,
                    pi = 0.25, # 0.1 used to simulate data. See what happens when we mis-specify
                    q = .95^(1/3),
                    perfect = 0) 

stopCluster(myCluster)
tictoc::toc()

#saveRDS(result, "Analysis/Simulations/sim_res_low_reporting_23dec2022.RDS")

