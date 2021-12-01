# Statistical Sleuth Case 4.1 
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Does launch temperature has an influence to o-ring failure?
# Is it more likely to face higher risk of o-ring failure at lower launch temperatures?

# Data Import and Data Check
data.41 <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store

# Check the data
View(data.41)
summary(data.41)
str(data.41)

# Summarize data
install.packages("FSA")
library(FSA)
Summarize(INCIDENTS ~ LAUNCH, data=data.41, digits=1)

# Data Visualitaion
stem(data.41[,1])  #stem-and-leaf plot

boxplot(INCIDENTS ~ LAUNCH,
        data=data.41,
        ylab="Number of Incidents")

# Package Activation
library(coin)

# t-Test
(my.t.test <- t.test(INCIDENTS ~ LAUNCH, var.equal=TRUE, data=data.41)) #there-is no huge dif. btw. variances

my.t.test$statistic # t-value

my.t.test$p.value # p-value

## Permutation test of independence
# This test treats the two groups (COOL and WARM) as independent samples, 
# and tests if there is a difference in values between the two groups.
independence_test(INCIDENTS ~ LAUNCH,
                  data = data.41, alternative="greater", distribution="exact",teststat="maximum")

##################################################

# Statistical Sleuth Case 4.2
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Does cognitive load have an influence on student's learning performance? 
# Is it more likely to make students more successful by modified teaching materials?
# Data Import and Data Check
data.42 <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store

# Check the data
View(data.42)
summary(data.42)
str(data.42)

# Summarize data
#install.packages("FSA")
#library(FSA)
Summarize(TIME ~ TREATMENT, data=data.42, digits=1)

# Data Visualitaion
stem(data.42[,1])  #stem-and-leaf plot

boxplot(TIME~ TREATMENT,
        data=data.42,
        ylab="Completion Time (Seconds)")

# Package Activation
library(coin)

# t-Test
(my.t.test2 <- t.test(TIME ~ TREATMENT, var.equal=TRUE, data=data.42)) #there-is no huge dif. btw. variances

my.t.test2$statistic # t-value

my.t.test2$p.value # p-value

## Wilcoxon Rank-Sum Test
(wilcox.test <- wilcox.test(TIME ~ TREATMENT,
                  data = data.42, alternative="two.sided", distribution="exact",teststat="maximum"))

wilcox.test$p.value/2 #one sided p-value 

## Wilcoxon Rank-Sum Test with Contuinuity Correction
wilcox.test(TIME ~ TREATMENT, conf.int=TRUE, exact=TRUE, data=data.42)