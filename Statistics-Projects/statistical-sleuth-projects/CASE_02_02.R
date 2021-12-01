# Statistical Sleuth Case 2.2 
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Early studies, based largel on postmortem analysis,
# suggest that the sizes of certain areas of the brain may be different
# in persons afflicted with schizophrenia than in others.
# In a 1990 article, researchers reported the results of a study
# that controlled for genetic and socioeconomic differences by examining
# 15 pairs of monozygotic twins
# where one of the twins was schizophrenic and the other was not.

# What is the magnitude of the difference in volumes of the left hippocampus
# between the unaffected and the affected individuals?
# Can the observed difference be attributed to chance?

# Data Import and Data Check
mydata=data <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store
View(mydata)

mydata[,1] -> group1
mydata[,2] -> group2
difdata <- group1-group2
View(difdata)
paste(group1,group2) -> mysample
View(mysample)
View(group1)
View(group2)

# Basic Statistics
summary(mydata)
summary(group1) #Unaffected group
summary(group2) #Affected group
summary(difdata) #Differences

# Estimated differences: mean and median
estimated.difference.mean <- mean(group1)-mean(group2)
estimated.difference.mean
estimated.difference.median <- median(group1)-median(group2)
estimated.difference.median

# Standard deviations
sd(group1) #standard deviation for unaffected group
sd(group2) #standard deviation for affected group
sd(difdata) #standard deviation for differences

# Standard error of the mean
sd(group1)/sqrt(length(group1)) #standard error for unaffected group
sd(group2)/sqrt(length(group2)) #standard error for affected group
sd(difdata)/sqrt(length(difdata)) #standard error for affected group

# Descriptive Statistics
# Scatter Plots
rangedata <- range(0,group1,group2)
plot(group1, type="p", col="blue", main="Differences in volumes (cm3) of left hippocampus\nScatter Plot", xlab="Pair Number", ylab="Differences in volumes (cm3)", col.lab="black", xlim=c(0,15), ylim=c(1,2.5))
lines(group2, type="p", col="red", pch=4, lty=2)  
legend("topleft", c("Unaffected","Affected"), col=c("blue","red"), pch=c(1,4))  

# Box Plots
boxplot(group1,group2,data=mydata, main="Differences in volumes (cm3) of left hippocampus", xlab="Unaffected Group & Affected Group", ylab="Differences in volumes (cm3) of left hippocampus", col=c("blue","red"), ylim=c(1,2.5))

# Histogram
hist(group1,
     main="Histogram for Unaffected Group",
     xlab="Differences in volumes (cm3) of left hippocampus",
     border="blue",
     col="green",
     las=1,
     breaks=15,
     xlim=c(1,2.5))
hist(group2,
     main="Histogram for Affected Group",
     xlab="Differences in volumes (cm3) of left hippocampus",
     border="blue",
     col="red",
     las=1,
     breaks=15,
     xlim=c(1,2.5))

# Inferential Statistics (PAIRED t-Test)
t.test <- t.test(group1, group2, paired=T)
t.test
sample.sd <- mean(mydata)
# t-ratio (μ is assumed as zero)
t.ratio <- (t.test$estimate-0)/(sd(difdata)/sqrt(length(difdata)))
t.ratio
