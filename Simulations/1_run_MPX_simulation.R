# Run MPX simulation to identify optimal cutoffs
# Running sim for 5 years
# Dec 21, 2022

# Load packages
library(vimes)
library(ape)
library(dplyr)
library(EpiEstim)
library(epicontacts)
library(ggplot2)

# Load in functions
source("Analysis/Functions/MPX_simulation.R")
source("Analysis/Functions/custom_kernel_tshuapa.R")
source("Analysis/Functions/simulate_reporting.R")

# Number of simulations to run
n_sims <- 200

# Specify parameters for serial interval distribution (gamma)
gamma_shape <- 18.505
gamma_rate <- 1.159

gamma_mean <- gamma_shape/gamma_rate
gamma_std <- sqrt(gamma_shape)/gamma_rate

si_dist <- discr_si(seq(0, 50), gamma_mean, gamma_std)
plot(seq(0, 50), si_dist, type = "h",
     lwd = 10, lend = 1, xlab = "time (days)", ylab = "frequency")

###########################################
# Simulate the epidemic (perfect reporting)
###########################################

my_list <- list()
tictoc::tic() # Takes about 4 min.
for (i in 1:n_sims){
  res <- my_simOutbreak(R0 = 0.81,
                        infec_curve = si_dist,
                        custom_kernel = custom_kernel_tshuapa)
  
  # Save each result to a list
  my_list[[i]] <- res
}
tictoc::toc()

########################################
# Simulate different levels of reporting
########################################

# Draw observed cases from all cases 
res10 <- lapply(my_list, FUN = obs_process, p_report = 0.10)
res25 <- lapply(my_list, FUN = obs_process, p_report = 0.25)
res50 <- lapply(my_list, FUN = obs_process, p_report = 0.50)
res100 <- lapply(my_list, FUN = obs_process, p_report = 1)

##################################################
# Plot number of cases for each reporting scenario
# Use boxplots as in S9 Fig in Cori et al.
##################################################

# Get number of cases in each simulated dataset
n_cases100 <- vector()
for (i in 1:n_sims){
  res <- res100[[i]]
  n_cases100[i] <- nrow(res)
}

n_cases10 <- vector()
for (i in 1:n_sims){
  res <- res10[[i]]
  n_cases10[i] <- nrow(res)
}

n_cases25 <- vector()
for (i in 1:n_sims){
  res <- res25[[i]]
  n_cases25[i] <- nrow(res)
}

n_cases50 <- vector()
for (i in 1:n_sims){
  res <- res50[[i]]
  n_cases50[i] <- nrow(res)
}

# Combine n cases into dataframe
plot_dat <- data.frame(n_cases100, n_cases10, n_cases25, n_cases50)

# Transform data from wide to long
plot_dat_long <- reshape2::melt(plot_dat)

# Plot
plot_dat_long$x_order <- NA
plot_dat_long$x_order[plot_dat_long$variable == "n_cases25"] <- 1
plot_dat_long$x_order[plot_dat_long$variable == "n_cases10"] <- 2
plot_dat_long$x_order[plot_dat_long$variable == "n_cases50"] <- 3
plot_dat_long$x_order[plot_dat_long$variable == "n_cases100"] <- 4

labs <- c("Baseline", "Low reporting", "High reporting", "Perfect reporting")

library(NatParksPalettes)
my_cols <- c(natparks.pals("Volcanoes"))
my_cols <- my_cols[-1]
my_cols <- rev(my_cols)

ggplot(plot_dat_long, aes(x = reorder(variable, x_order), y = value, fill = variable)) + 
  geom_boxplot() +
  theme_classic() +
  xlab("") +
  ylab("Number of cases in simulated dataset") +
  scale_fill_manual(values = my_cols) +
  scale_x_discrete(labels = labs) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13, color = "black")) # save as pdf 6 x 5 or 625 x 500 jpeg

#########################
# Re-number clusters in simulated data (not needed for perfect reporting)
#########################

source("Analysis/Functions/re_number_clusters.R")

res50 <- lapply(res50, FUN = re_number_clusters)
res25 <- lapply(res25, FUN = re_number_clusters)
res10 <- lapply(res10, FUN = re_number_clusters)

#########################
# Save simulated datasets 
#########################

# Save as a list to run on vimes
mpx_sims <- list(res100, res50, res25, res10)

#saveRDS(mpx_sims, "Analysis/Simulations/mpx_sims_21dec2022.RDS")
