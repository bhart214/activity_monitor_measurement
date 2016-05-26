install.packages('lme4')
library(lme4)

install.packages('psych')
library(psych)

data <- read.csv("measurement.csv", header = T, sep = ",")

### Pulling out data for block of Environment 1 and Fitbit Surge Right 

true <- as.matrix(subset(data, task == 1, select = video.count))
obs <- as.matrix(subset(data, task == 1, select = Fitbit.surge.right))



### Bland-Altman Plot

diff <- abs(true - obs)
avg <- (true+obs)/2
plot(avg, diff)



### ICC using model fit

# Create a data frame with three columns, first one as indicator for true vs. recorded steps, second as indicator for subject, third with # of steps 
model_data <- as.data.frame(cbind(c(rep(1,nrow(true)),rep(2,nrow(obs))), rep(1:nrow(true),2), rbind(true,obs)))
colnames(model_data) <- c('Device', 'Subject', 'Steps')

# Fit linear random effects model
mod <- lmer(Steps ~ 1 + (1 | Subject) + (1 | Device), data = model_data)
s_mod <- summary(mod)
# In s_mod output, ICC is calculated by taking subject variance (197.066) dividied by all three variances added together 


### ICC directly using icc() function from psych package
icc_data <- cbind(true, obs) 
colnames(icc_data) <- c('True', 'Monitor')
rownames(icc_data) <- paste('Subject', 1:14, sep = " ")

ICC(icc_data)
