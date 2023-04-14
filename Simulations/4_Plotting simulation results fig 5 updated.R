# Plotting figure 5: simulation results
# Dec 23, 2022

library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)

R_used_to_gen_sim <- 0.81 # R used to generate the simulations
import_rate_sim <- 145 # importation rate used to generate simulations

# Load simulation results for panels A and C
# (reconstruction scenarios with different cutoffs
# as well as under- and over-estimated reporting)
dat1 <- readRDS("Analysis/Simulations/reconstruction_res1_21dec2022.RDS")
dat2 <- readRDS("Analysis/Simulations/reconstruction_res2_21dec2022.RDS")

# Check one
check <- dat1[[1]]
check <- check[[1]]
check$sim_no <- 1
check <- rename(check, sim_name = quantile)
check_long <- melt(check, id.vars = c("sim_no", "sim_name"))

# Loop to extract information for plotting
my_list <- list()
for (i in 1:length(dat1)){
  sim <- dat1[[i]]
  sim <- sim[[1]]
  sim$sim_no <- i
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "quantile"))
}

df <- do.call(rbind, my_list)

df <- rename(df, sim_name = quantile)

df$sim_name <- as.character(df$sim_name)

df$sim_name[df$sim_name == "0.5"] <- "Cutoff\n50%"
df$sim_name[df$sim_name == "0.9"] <- "Cutoff\n90%"
df$sim_name[df$sim_name == "0.95"] <- "Control\n(cutoff\n95%)"
df$sim_name[df$sim_name == "0.983047572491558"] <- "Cutoff\n98.3%"
df$sim_name[df$sim_name == "0.999"] <- "Cutoff\n99.9%"

# Now do a loop for under- and over-estimated reporting

# Check one
check <- dat2[[1]]
check <- check[[1]]
check$sim_no <- 1
check$sim_name <- c("Under-\nesti-\nmated\nreporting","Over-\nesti-\nmated\nreporting")
check <- select(check, -quantile)
check_long <- melt(check, id.vars = c("sim_no", "sim_name"))

my_list <- list()
for (i in 1:length(dat2)){
  sim <- dat2[[i]]
  sim <- sim[[1]]
  sim$sim_no <- i
  sim$sim_name <- c("Under-\nesti-\nmated\nreporting","Over-\nesti-\nmated\nreporting")
  sim <- select(sim, -quantile)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

df2 <- do.call(rbind, my_list)

# Combine all data
all <- rbind(df, df2)

all$variable <- as.character(all$variable)

all$variable[all$variable == "mean"] <- "Mean"


p1 <- ggplot(all, aes(x = variable, y = value, fill = sim_name)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Ability of model to correctly identify outbreak clusters") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#A4036F","#048BA8","#16DB93","#EFEA5A","#F29E4C","#FF495C","#FF99C8")) +
  facet_wrap(~sim_name, nrow = 1) +
  theme (legend.position="none",
        strip.background=element_rect(colour="black",
                                    fill="white")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#####
#####
# Get relative error for R and spillover rate
check <- dat1[[1]]
check[[3]][1,] # central estimates for R
check[[4]][1,] # central estimates for import rate

datt <- data.frame(sim_no = rep(i, length(check[[1]]$quantile)), 
                   sim_name = check[[1]]$quantile,
                   R_err = (check[[3]][1,] - R_used_to_gen_sim)/R_used_to_gen_sim,
                   import_err = (check[[4]][1,] - import_rate_sim)/import_rate_sim)

# Loop
my_list <- list()
for (i in 1:length(dat1)){
  sim <- dat1[[i]]
   
  sim <- data.frame(sim_no = rep(i, length(sim[[1]]$quantile)), 
                     sim_name = sim[[1]]$quantile,
                     R_err = (sim[[3]][1,] - R_used_to_gen_sim)/R_used_to_gen_sim,
                     import_err = (sim[[4]][1,] - import_rate_sim)/import_rate_sim)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))

}

df3 <- do.call(rbind, my_list)

df3$sim_name[df3$sim_name == "0.5"] <- "Cutoff\n50%"
df3$sim_name[df3$sim_name == "0.9"] <- "Cutoff\n90%"
df3$sim_name[df3$sim_name == "0.95"] <- "Control\n(cutoff\n95%)"
df3$sim_name[df3$sim_name == "0.983047572491558"] <- "Cutoff\n98.3%"
df3$sim_name[df3$sim_name == "0.999"] <- "Cutoff\n99.9%"

df3$variable <- as.character(df3$variable)
df3$variable[df3$variable == "R_err"] <- "R"
df3$variable[df3$variable == "import_err"] <- "Spillover\nrate"

# Now do a loop for under- and over-estimated reporting

# Check one
check <- dat2[[1]]
check[[3]][1,] # central estimates for R
check[[4]][1,] # central estimates for import rate

# Loop
my_list <- list()
for (i in 1:length(dat2)){
  sim <- dat2[[i]]
  
  sim <- data.frame(sim_no = rep(i, length(sim[[1]]$quantile)), 
                             sim_name = c("Under-\nesti-\nmated\nreporting","Over-\nesti-\nmated\nreporting"),
                             R_err = (sim[[3]][1,] - R_used_to_gen_sim)/R_used_to_gen_sim,
                             import_err = (sim[[4]][1,] - import_rate_sim)/import_rate_sim)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

df4 <- do.call(rbind, my_list)

df4$variable <- as.character(df4$variable)
df4$variable[df4$variable == "R_err"] <- "R"
df4$variable[df4$variable == "import_err"] <- "Spillover\nrate"

all2 <- rbind(df3, df4)

all2$variable[all2$variable == "R"] <- "Rt"

# Plot panel B
p2 <- ggplot(all2, aes(x = variable, y = value, fill = sim_name)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Relative error in estimated parameters") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(-0.6,1.1), expand = c(0, 0)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#A4036F","#048BA8","#16DB93","#EFEA5A","#F29E4C","#FF495C","#FF99C8")) +
  facet_wrap(~sim_name, nrow = 1) +
  theme (legend.position="none",
         strip.background=element_rect(colour="black",
                                       fill="white")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

#####
#####
# Now get the same plots for the baseline, low, high, and perfect reporting

# Load in sim results
dat3 <- readRDS("Analysis/Simulations/sim_res_low_reporting_23dec2022.RDS")
dat4 <- readRDS("Analysis/Simulations/sim_res_high_reporting_23dec2022.RDS")
dat5 <- readRDS("Analysis/Simulations/sim_res_perfect_reporting_23dec2022.RDS")

check <- dat3[[1]]
check <- check[[1]]
check$sim_no <- 1
check$sim_name <- "Low\nreporting"
check <- select(check, -quantile)
check_long <- melt(check, id.vars = c("sim_no", "sim_name"))

my_list <- list()
for (i in 1:length(dat3)){
  sim <- dat3[[i]]
  sim <- sim[[1]]
  sim$sim_no <- i
  sim$sim_name <- "Low\nreporting"
  sim <- select(sim, -quantile)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

df5 <- do.call(rbind, my_list)
######
my_list <- list()
for (i in 1:length(dat4)){
  sim <- dat4[[i]]
  sim <- sim[[1]]
  sim$sim_no <- i
  sim$sim_name <- "High\nreporting"
  sim <- select(sim, -quantile)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

df6 <- do.call(rbind, my_list)
######
my_list <- list()
for (i in 1:length(dat5)){
  sim <- dat5[[i]]
  sim <- sim[[1]]
  sim$sim_no <- i
  sim$sim_name <- "Perfect\nreporting"
  sim <- select(sim, -quantile)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

df7 <- do.call(rbind, my_list)

# Combine all the res
df_98 <- filter(df, sim_name == "Cutoff\n98%") # optimal cutoff
df_98$sim_name <- "Baseline"

all3 <- rbind(df5, df6, df7, df_98)

all3$variable <- as.character(all3$variable)
all3$variable[all3$variable == "mean"] <- "Mean"

p3 <- ggplot(all3, aes(x = variable, y = value, fill = sim_name)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Ability of model to correctly identify outbreak clusters") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#A4036F","#16DB93","#EFEA5A","#F29E4C")) +
  facet_wrap(~sim_name, nrow = 1) +
  theme (legend.position="none",
         strip.background=element_rect(colour="black",
                                       fill="white")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
p3

###### Final plot
# Check one
check <- dat3[[1]]
check[[3]][1,] # central estimates for R
check[[4]][1,] # central estimates for import rate

# Loop
my_list <- list()
for (i in 1:length(dat3)){
  sim <- dat3[[i]]
  
  sim <- data.frame(sim_no = rep(i, length(sim[[1]]$quantile)), 
                    sim_name = "Low\nreporting",
                    R_err = (sim[[3]][1,] - R_used_to_gen_sim)/R_used_to_gen_sim,
                    import_err = (sim[[4]][1,] - import_rate_sim)/import_rate_sim)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

datt1 <- do.call(rbind, my_list)
####
my_list <- list()
for (i in 1:length(dat4)){
  sim <- dat4[[i]]
  
  sim <- data.frame(sim_no = rep(i, length(sim[[1]]$quantile)), 
                    sim_name = "High\nreporting",
                    R_err = (sim[[3]][1,] - R_used_to_gen_sim)/R_used_to_gen_sim,
                    import_err = (sim[[4]][1,] - import_rate_sim)/import_rate_sim)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

datt2 <- do.call(rbind, my_list)
####
####
my_list <- list()
for (i in 1:length(dat5)){
  sim <- dat5[[i]]
  
  sim <- data.frame(sim_no = rep(i, length(sim[[1]]$quantile)), 
                    sim_name = "Perfect\nreporting",
                    R_err = (sim[[3]][1,] - R_used_to_gen_sim)/R_used_to_gen_sim,
                    import_err = (sim[[4]][1,] - import_rate_sim)/import_rate_sim)
  my_list[[i]] <- melt(sim, id.vars = c("sim_no", "sim_name"))
}

datt3 <- do.call(rbind, my_list)

datt_all <- rbind(datt1, datt2, datt3)

datt_all$variable <- as.character(datt_all$variable)
datt_all$variable[datt_all$variable == "R_err"] <- "R"
datt_all$variable[datt_all$variable == "import_err"] <- "Spillover\nrate"

# Combine with baseline
dat_baseline <- filter(df3, sim_name == "Cutoff\n98%")
dat_baseline$sim_name <- "Baseline"

datt_to_plot <- rbind(datt_all, dat_baseline)

datt_to_plot$variable[datt_to_plot$variable == "R"] <- "Rt"

p4 <- ggplot(datt_to_plot, aes(x = variable, y = value, fill = sim_name)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Relative error in estimated parameters") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(-0.6,1.1), expand = c(0, 0)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#A4036F","#16DB93","#EFEA5A","#F29E4C")) +
  facet_wrap(~sim_name, nrow = 1) +
  theme (legend.position="none",
         strip.background=element_rect(colour="black",
                                       fill="white")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

p4

# Put all the plots together
(p1|p2)/(p3|p4) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = "bold", size = 14)) 

# 1000 x 1000 jpeg

