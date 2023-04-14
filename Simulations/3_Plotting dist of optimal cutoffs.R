# Plot S12
# Distribution of the optimal cutoff choice (defined as that yielding the highest average between sensitivity and specificity), for the 200 baseline simulations.
# Note that for certain simulations, several cutoffs yielded the same performance, so a single simulation may be contributing to several cutoffs.
# Dec 23, 2022

# Read in reconstruction results (baseline with different cutoffs)
result <- readRDS("Analysis/Simulations/reconstruction_res1_21dec2022.RDS")

n_sims <- 200

my_list <- list()
for (i in 1:n_sims){
  
  optimal_q <- result[[i]]
  optimal_q <- optimal_q[[2]]
  
  my_list[[i]] <- optimal_q
}

optimal_q <- unlist(my_list)

# Need to count how many of each cutoff there are
table(optimal_q)

optimal_q <- c(0, 0, 24, 147, 117)

quantile_pretty <- c("50%","90%","95%","98.3%","99.9%")

barplot(optimal_q, ylab = "Number of simulations with optimal cutoff",
        xlab = "Cutoff used for pruning", main = "", space = c(0,0), col = "#ED9390")
axis(side = 1, at = c(0.5,1.5,2.5,3.5,4.5),
     labels = quantile_pretty, cex = 0.75)
# 530 x 480 jpeg
