
### Assessing the Differences Between Multiple Measures - How to Determine Sample Size Requirement ###
### Brian Hart ###
### March 29, 2016 ###


library(dplyr)
library(tidyr)
library(ggplot2)

### Simulate data to represent step counts from video and counts from two different monitors
n_subjects = 50
mean_videoSteps = 100
mean_monitor1 = 106
mean_monitor2 = 97

set.seed(987)
videoSteps = round(rnorm(n_subjects, mean = mean_videoSteps, sd = 10))
monitor1 = round(rnorm(n_subjects, mean = mean_monitor1, sd = 10))
monitor2 = round(rnorm(n_subjects, mean = mean_monitor2, sd = 10))


stepsDF = data.frame(videoSteps, monitor1, monitor2)
head(stepsDF, 15)
summary(stepsDF)

# Add two variables for differences between video counts and counts of by each of the monitors
stepsDF = stepsDF %>% mutate(monitor1_diff = videoSteps - monitor1,
                             monitor2_diff = videoSteps - monitor2)
head(stepsDF, 15)
summary(stepsDF)

# create long data frame of measurements, long data frame of measurement differences, 
# and "group_means" data frame to make density plots easier
stepsDF_long = stepsDF %>% select(videoSteps, monitor1, monitor2) %>% gather(count_method, steps_measured, videoSteps:monitor2)
stepsDiff_long = stepsDF %>% select(monitor1_diff, monitor2_diff) %>% gather(monitor_diff, diff, monitor1_diff:monitor2_diff)
group_means = stepsDF_long %>% group_by(count_method) %>% summarise(mean_steps = mean(steps_measured))
group_diff_means = stepsDiff_long %>% group_by(monitor_diff) %>% summarise(mean_diff = mean(diff))

# density plots of counts by video, monitor1, and monitor2; vertical lines represent means of each count method
measurements = ggplot(stepsDF_long, aes(x=steps_measured, fill=count_method)) +
  geom_density(alpha=0.5) +
  geom_vline(data = group_means, aes(xintercept=mean_steps,colour=count_method), size=1, linetype = "dashed")

differences = ggplot(stepsDiff_long, aes(x=diff, fill=monitor_diff)) +
  geom_density(alpha=0.5) +
  geom_vline(data = group_diff_means, aes(xintercept=mean_diff,colour=monitor_diff), size=1, linetype = "dashed")

measurements
differences




### Bland-Altman Plots
library(BlandAltmanLeh)

par(mfrow=c(2,1))
bland.altman.plot(stepsDF$videoSteps, stepsDF$monitor1, main="Bland-Altman Plot (Video Count vs Monitor 1)", xlab="Means", ylab="Differences")
bland.altman.plot(stepsDF$videoSteps, stepsDF$monitor2, main="Bland-Altman Plot (Video Count vs Monitor 2)", xlab="Means", ylab="Differences")
par(mfrow=c(1,1))

# sample size approximation
interval_width = 10
std_dev = 10
upper_bound = interval_width/2
sample_size = 3/(upper_bound/(1.96*std_dev)^2)
sample_size

# calculate interval width given sample size
n = 25
std_dev = 10
width = 1.96 * sqrt((3 * std_dev^2) / n)
width


### Intraclass Correlation Coefficient (ICC)
# used when quantitative measurements are made on units organized into groups
# describes how strongly units in the same group resemble each other
library(psych)

# I think we're interested in type "ICC3"
ICC(stepsDF[,c("videoSteps", "monitor1")], alpha = 0.05)
ICC(stepsDF[,c("videoSteps", "monitor2")], alpha = 0.05)

ICC(stepsDF[,c("videoSteps", "monitor1", "monitor2")], alpha = 0.05)
ICC(stepsDF[,c("videoSteps", "monitor1", "monitor2")], alpha = 0.05)$summary

# looks to me like these p-values are not significant, meaning that the three measures are not significantly different...





### One Sample T-Test
t.test(stepsDF$monitor1_diff) # reject null hypothesis
t.test(stepsDF$monitor2_diff) # fail to reject null hypothesis
