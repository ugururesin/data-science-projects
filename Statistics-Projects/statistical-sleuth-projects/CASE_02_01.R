# Statistical Sleuth Case 2.1 
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Do humerus lengths tend to be different for survivors than for those that perished?
# If so, how large is the difference?

# Data Import and Data Check
mydata=data <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store
View(mydata)

humerus.length <- mydata[,1]
labels <- as.character(mydata[,2])   #Labels are already given in the data
View(mydata)

mydata[1:24,1] -> group1
mydata[25:59,1] -> group2
View(group1)
View(group2)

# Basic Statistics
summary(mydata)
summary(group1) #perished group
summary(group2) #survived group

# Estimated differences: mean and median
estimated.difference.mean <- mean(group1)-mean(group2)
estimated.difference.mean
estimated.difference.median <- median(group1)-median(group2)
estimated.difference.median

# Standard deviations
sd(group1) #standard deviation for perished group
sd(group2) #standard deviation for survived group

# Standard error of the mean
sd(group1)/sqrt(length(group1)) #standard error for perished group
sd(group2)/sqrt(length(group2)) #standard error for survived group

# Descriptive Statistics
# Scatter Plots
rangedata <- range(0,group1,group2)
plot(group1, type="p", col="blue", main="Humerus Length / Survival Status\nScatter Plot", xlab="Sample Number", ylab="Humerus Length", col.lab="black", xlim=c(0,40), ylim=c(600,800))
lines(group2, type="p", col="red", pch=4, lty=2)  
legend(0,rangedata[2], c("Perished","Survived"), col=c("blue","red"), pch=c(1,4))  

# Box Plots
boxplot(group1,group2,data=mydata, main="Humerus Length Distribution", xlab="Perished Group & Survived Group", ylab="Humerus Length", col=c("blue","red"), ylim=c(650,800))

# Histogram
hist(group1,
     main="Histogram for Perished Group",
     xlab="Humerus Length",
     border="blue",
     col="green",
     las=1,
     breaks=24,
     xlim=c(650,800))
hist(group2,
     main="Histogram for Survived Group",
     xlab="Humerus Length",
     border="blue",
     col="red",
     las=1,
     breaks=35,
     xlim=c(650,800))

# Inferential Statistics
#Two-sided Sample t-test
t.test(humerus.length~labels, var.equal=F, alternative="two.sided",data=mydata)
#One-sided Sample t-test
t.test(humerus.length~labels, var.equal=TRUE, alternative="less",data=mydata)