# Statistical Sleuth Case 1.2
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Did a bank discriminatorily pay higher starting salaries to men than to women?

# Data Import and Data Check
salary.data=read.csv("/Users/UGUR/Desktop/R/Research_Methods_I/data/excel/CASE0102.csv",sep=";", header=T)
View(salary.data)
summary(salary.data[,1])
summary(salary.data[,2])

salaries <- salary.data[,1]
labels <- salary.data[,2]   #Labels are already given in the data
salary.data[,2] <- as.character(salary.data[,2])

sal.female <- salary.data[1:61,1]
sal.male <- salary.data[62:93,1]
typeof(sal.female)
View(sal.female)
View(sal.male)

# Basic Statistics
summary(sal.female)
summary(sal.male)
summary(salary.data)
estimated.difference <- mean(sal.male)-mean(sal.female)
estimated.difference

# Descriptive Statistics
# Scatter Plots
rangedata <- range(0,sal.male,sal.female)
plot(sal.male, type="p", col="blue", main="Salary/Gender Scatter Plot", xlab="Sample Number", ylab="Salary", col.lab="black", xlim=c(0,70))         
lines(sal.female, type="p", col="red", pch=4, lty=2)  
legend(1,rangedata[2], c("Male","Female"), col=c("blue","red"), pch=c(1,4))  

# Box Plots
boxplot(sal.male,sal.female,data=salary.data, main="Salary Distribution", xlab="Male Salaries & Female Salaries", ylab="Salary", col=c("blue","red"))

# Histogram
hist(sal.male,
     main="Histogram for Male Salaries",
     xlab="Creativity Scores",
     border="blue",
     col="green",
     las=1,
     breaks=24,
     xlim=c(4000,9000))
hist(sal.female,
     main="Histogram for Female Salaries",
     xlab="Creativity Scores",
     border="blue",
     col="red",
     las=1,
     breaks=23,
     xlim=c(3000,7000))

# Inferential Statistics
t.test(salaries~labels,alternative="two.sided",data=salary.data)