# NORMALITY - TESTS
install.packages("dplyr")
# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
# OR install.packages("ggpubr")
library("dplyr")
library("ggpubr")
# If .txt tab file, use this
my_data <- read.delim(file.choose())
# Or, if .csv file, use this
my_data <- read.csv(file.choose())
# Store the data in the variable my_data
my_data <- ToothGrowth
set.seed(1234)
dplyr::sample_n(my_data, 10)
# Visual Methods
library("ggpubr")
ggdensity(my_data$len, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")
library(ggpubr)
ggqqplot(my_data$len)
library("car")
qqPlot(my_data$len)
# Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S.
# It is based on the correlation between the data and the corresponding normal scores.
shapiro.test(my_data$len)