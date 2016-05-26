# Visual Representation of the Power and the Significance Level
library(ggplot2)

x <- seq(-4, 6, 0.1) # 100 values between -4 and 6, incremented by 0.1 (mean = 1.0)

# initialize two means
mean1 <- 0.00
mean2 <- 2.47

# data frame of x vector, randomly distributed values about mean1, randomly distributed values about mean2
dat <- data.frame(x = x, y1 = dnorm(x, mean1, 1), y2 = dnorm(x, mean2, 1))

# plot both distributions (y1 and y2);
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = y1, colour = 'H0 is true'), size = 1.2) +
  geom_line(aes(y = y2, colour = 'H1 is true'), size = 1.2) +
  geom_area(aes(y = y1, x = ifelse(x > 1.65, x, NA)), fill = 'black') +
  geom_area(aes(y = y2, x = ifelse(x > 1.65, x, NA)), fill = 'blue', alpha = 0.3) +
  xlab("") + ylab("") + theme(legend.title = element_blank()) +
  scale_colour_manual(breaks = c("H0 is true", "H1 is true"), values = c("blue", "red"))




# Sample Size Requirements at Different Levels of Detectable Difference (Test of Proportions)
# Power increases with sqrt(n)
library(scales)
p1 <- 0.1 # baseline rate
b <- 0.8 # power
a <- 0.05 # significance level
dd <- seq(from = 0.01, to = 0.03, by = 0.0001) # detectable differences
result <- data.frame(matrix(nrow = length(dd), ncol = 2))
names(result) <- c("DD", "ni")
for (i in 1:length(dd)) {
  result[i, "DD"] <- dd[i]
  result[i, "ni"] <- power.prop.test(sig.level = a, p1 = p1, p2 = p1 - dd[i], alternative = 'two.sided', power = b)$n
}
ggplot(data = result, aes(x = DD, y = ni)) +
  geom_line() + 
  ylab("n") + 
  xlab("Detectable difference") + 
  scale_x_continuous(labels = comma) +
  geom_point(data = result[ceiling(result$n / 10) * 10 == 5000, ],aes(x = DD, y = ni), colour = "red", size = 5)

             
             
             
# Power Test of Proportions with Detectable Difference of 0.016
power.prop.test(sig.level=0.05, p1=0.1, p2=0.10-0.016, alternative='two.sided', n=5000)$power




# Effect Size vs. Sample Size Requirement for T-Test (Difference Between Averages)

# replace 0.5 with desired effect size
desired_effect_size = 0.5

# create vector of effect sizes
effect_sizes <- seq(from = 0.2, to = 1.5, by = 0.01)

# initialize data frame for effect size column and sample size column
ES_and_N <- data.frame(matrix(nrow = length(effect_sizes), ncol = 2))

# ES = effect size; ni = sample size requirement for given effect
names(ES_and_N) <- c("ES", "ni")

# compute sample size requirements for vector of effect sizes
# significance level is set to .05 and power is set to .8
for (i in 1:length(effect_sizes)){
  ES_and_N[i, "ES"] <- effect_sizes[i]
  ES_and_N[i, "ni"] <- power.t.test(sig.level = 0.05, d = effect_sizes[i], sd = 1, alternative = 'two.sided', power = 0.8)$n  
}

# plot curve of effect size vs. sample size; mark desired effect with red dot
ggplot(data = ES_and_N, aes(x = ES, y = ni)) + 
  geom_line() + xlab("Effect Size") +   ylab("N") +  
  ggtitle("Effect Size vs. Sample Size") +
  ylim(0, 200) +
  scale_x_continuous() +
  geom_point(data = ES_and_N, aes(x = ES_and_N[ES_and_N$ES == 0.5,]$ES, y = ES_and_N[ES_and_N$ES == 0.5,]$ni), colour = "red", size = 5) +
  geom_text(data = ES_and_N, aes(x = ES_and_N[ES_and_N$ES == 0.5,]$ES, y = ES_and_N[ES_and_N$ES == 0.5,]$ni, label = paste(ES_and_N[ES_and_N$ES == 0.5,]$ES, ",", round(ES_and_N[ES_and_N$ES == 0.5,]$ni, 2)), hjust=-.5, vjust=-.5))

