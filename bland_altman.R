
### Bland-Altman Analysis - Sample Size and Limits of Agreement ###
### Brian Hart ###
### March 29, 2016 ###

library(dplyr)
library(psych)

#setwd("C:/Users/bhart/Dropbox/MSSP/Consulting")

### ===== Look at Preliminary Data to Estimate Means and Standard Deviations ===== ###
# read in preliminary data
steps = read.csv("measurement.csv")
str(steps)
steps = na.omit(steps)

steps = steps %>% mutate(diff_fb_surge_r = video_count - fb_surge_r,
                         diff_fb_surge_l = video_count - fb_surge_l,
                         diff_fb_zip_r = video_count - fb_zip_r,
                         diff_fb_zip_l = video_count - fb_zip_l,
                         diff_jb_up2_r = video_count - jb_up2_r,
                         diff_jb_up2_l = video_count - jb_up2_l,
                         diff_jb_move_r = video_count - jb_move_r,
                         diff_jb_move_l = video_count - jb_move_r)

describeBy(steps, steps$task)

# Boxplots of differences
png("plots/boxplots_differences.png", height = 1000, width = 1800)
op = par(mfrow = c(2,2))
boxplot(steps[steps$task==1, c(12:19)], main="Task 1", col="red", ylim=c(-300, 250))
boxplot(steps[steps$task==2, c(12:19)], main="Task 2", col="blue", ylim=c(-300, 250))
boxplot(steps[steps$task==3, c(12:19)], main="Task 3", col="green", ylim=c(-300, 250))
boxplot(steps[steps$task==4, c(12:19)], main="Task 4", col="yellow", ylim=c(-300, 250))
par(op)
dev.off()

# Create dataframes of differences fore each task
for (i in 1:4) {
  assign(paste0("diff_task", i), steps %>% select(task, 
                                  diff_fb_surge_r, 
                                  diff_fb_surge_l, 
                                  diff_fb_zip_r, 
                                  diff_fb_zip_l, 
                                  diff_jb_up2_r, 
                                  diff_jb_up2_l, 
                                  diff_jb_move_r, 
                                  diff_jb_move_l) %>% arrange(task) %>% filter(task == i))
}



# Create dataframe of mean differences for each task
means = data.frame(rbind(apply(diff_task1, 2, mean), 
                         apply(diff_task2, 2, mean), 
                         apply(diff_task3, 2, mean), 
                         apply(diff_task4, 2, mean)))

# Create dataframe of standard deviations of differences for each task
deviations = data.frame(rbind(apply(diff_task1[,-1], 2, sd), 
                              apply(diff_task2[,-1], 2, sd), 
                              apply(diff_task3[,-1], 2, sd), 
                              apply(diff_task4[,-1], 2, sd))) %>% cbind(task = c(1,2,3,4))
deviations = deviations[ , c(9, 1:8)]

# View dataframes
means
deviations




### ===== Example Bland Altman Plot of Data Collected Thus Far ===== ###
library(BlandAltmanLeh)
bland.altman.plot(steps$video_count[steps$task == 1], steps$fb_zip_r[steps$task == 1], conf.int = 0.95)




### ===== Simulate Bland Altman Data ===== ###
mean_diff = 0 # this can be adjusted. From the prelim data, it looks like it should be between about 1 and 25 (with one at 62).
n = seq(from=10, to=60, by=1)
std_dev = seq(from=0, to=30, by=1)

sim_data = expand.grid(n = n, std_dev = std_dev)
sim_data = sim_data %>% mutate(ula = mean_diff + 1.96*std_dev,  # upper limit of agreement (ULA)
                              lla = mean_diff - 1.96*std_dev,  # lower limit of agreement (LLA)
                              LoA_width = ula - lla,  # width of limit of agreement
                              upper_95CI_bias = mean_diff + qt(.975, n-1),
                              lower_95CI_bias = mean_diff - qt(.975, n-1),
                              width_95CI_bias = upper_95CI_bias - lower_95CI_bias,
                              ula_conf_u = ula + qt(.975, n-1) * sqrt((3*std_dev^2)/n),  # ULA 95% CI Upper boundary
                              ula_conf_l = ula - qt(.975, n-1) * sqrt((3*std_dev^2)/n),  # ULA 95% CI lower boundary
                              ula_width = ula_conf_u - ula_conf_l,  # precision of ULA
                              lla_conf_u = lla + qt(.975, n-1) * sqrt((3*std_dev^2)/n),  # LLA 95% CI Upper boundary
                              lla_conf_l = lla - qt(.975, n-1) * sqrt((3*std_dev^2)/n),  # LLA 95% CI lower boundary
                              lla_width = lla_conf_u - lla_conf_l)  # precision of LLA

#write.csv(sim_data, file = "sim_data.csv")
View(sim_data)

sim_data %>% filter(LoA_width < ___)
sim_data %>% filter(ula_width < ___)
sim_data %>% filter(n < ___)
sim_data %>% filter(std_dev %in% c(5:10))






### ====== Functions for Easier Calculations and Plotting ===== ###

# Calculate 95% CI Width of LoA for given n and sd_diffs
CI_width = function(n, sd_diffs) {
  ula = 1.96*sd_diffs
  ula_conf_u = ula + qt(.975, n-1) * sqrt((3*sd_diffs^2)/n)  # ULA 95% CI Upper boundary
  ula_conf_l = ula - qt(.975, n-1) * sqrt((3*sd_diffs^2)/n)  # ULA 95% CI lower boundary
  ula_width = ula_conf_u - ula_conf_l  # precision of ULA
  ula_width
}
CI_width(30, 10)



# plot CI width vs sample size for given standard deviation of differences
plot_n_CI = function(sd_diffs){
  n = 2:50
  ula = 1.96*std_dev
  ula_conf_u = ula + qt(.975, n-1) * sqrt((3*sd_diffs^2)/n)  # ULA 95% CI Upper boundary
  ula_conf_l = ula - qt(.975, n-1) * sqrt((3*sd_diffs^2)/n)  # ULA 95% CI lower boundary
  ula_width = ula_conf_u - ula_conf_l  # precision of ULA  
  
  plot(ula_width, n, main = paste("Standard Deviation =", sd_diffs), xlab = "Error Margin", ylab = "Sample Size", type = "b", col="red", xlim = c(0, 60), ylim = c(0, 50))
}
png("plots/ci_width_vs_sample_size.png", height = 1000, width = 1500)
op = par(mfrow = c(2,2))
plot_n_CI(5)
plot_n_CI(10)
plot_n_CI(15)
plot_n_CI(20)
par(op)
dev.off()


# Calculate LoA width for a given sd_diffs
LoA_width = function(sd_diffs){
  n = 2:50
  ula = 1.96*sd_diffs  # upper limit of agreement (ULA)
  lla = - 1.96*sd_diffs  # lower limit of agreement (LLA)
  LoA_width = ula - lla  # width of limit of agreement
  LoA_width
}
LoA_width(10)


# put in values for standard deviation of differences and desired CI width and produce a plot
# in order to do this, we need to calculate the required sample size given the sd_diffs and the desired CI width
requiredN = function(sd_diffs, desired_width){
  # sample size approximation
  upper_bound = desired_width/2
  sample_size = 3/(upper_bound/(sd_diffs)^2)
  sample_size
}
requiredN(10, 20)

# desired margin of error was less than 15
# based on preliminary data, a sd_diffs of 10 seemed reasonable
# sample size of 30 was chosen based on this desired margin of error of the 95% confidence interval and the sd_diffs from 


# Simulate a Bland-Altman plot when given the following arguments:
# n = sample size
# sd_diffs = standard deviation of the differences between two measurements
plotBA = function(n, sd_diffs) {
  differences = rnorm(n, mean = 0, sd_diffs)
  std_dev = sd(differences)
  mean_diff = mean(differences)
  
  ula = mean_diff + 1.96*std_dev  # upper limit of agreement (ULA)
  lla = mean_diff - 1.96*std_dev  # lower limit of agreement (LLA)
  upper_95CI_bias = mean_diff + qt(.975, n-1)
  lower_95CI_bias = mean_diff - qt(.975, n-1)
  ula_conf_u = ula + qt(.975, n-1) * sqrt((3*std_dev^2)/n)  # ULA 95% CI Upper boundary
  ula_conf_l = ula - qt(.975, n-1) * sqrt((3*std_dev^2)/n)  # ULA 95% CI lower boundary
  ula_width = ula_conf_u - ula_conf_l  # precision of ULA
  lla_conf_u = lla + qt(.975, n-1) * sqrt((3*std_dev^2)/n)  # LLA 95% CI Upper boundary
  lla_conf_l = lla - qt(.975, n-1) * sqrt((3*std_dev^2)/n)  # LLA 95% CI lower boundary
  
  plot(differences, col = "blue", ylim = c(-50, 50), main = paste("B-A, n =", n, ", sd of diffs =", sd_diffs), ylab = "Differences", xlab = "", xaxt='n')
  abline(h = mean_diff, lty=1)
  abline(h = ula, lty=2)
  abline(h = lla, lty=2)
  abline(h = ula_conf_u, lty=3)
  abline(h = ula_conf_l, lty=3)
  abline(h = lla_conf_u, lty=3)
  abline(h = lla_conf_l, lty=3)
}

# Example Bland-Altman Plots
png("plots/ba_plots1.png", height = 1000, width = 1500)
op = par(mfrow=c(2,2))
plotBA(35, 5)
plotBA(35, 10)
plotBA(35, 15)
plotBA(35, 20)
par(op)
dev.off()

png("plots/ba_plots2.png", height = 1000, width = 1500)
op = par(mfrow=c(2,2))
plotBA(20, 10)
plotBA(30, 10)
plotBA(40, 10)
plotBA(50, 10)
par(op)
dev.off()


