# Statistical Sleuth Case 1.1
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Subjects are asked to write a poem.
# All poems were submitted to 12 poets and
# They evaluated poems on a 40-points scale
# The average score is the creativity score!

# Intrinsic Motivation: Satisfaction of doing sth.
# Extrinsic Motivation: Be rewarded for doing sth.

# Data Creation (Creativity Scores) and Data Check
creativity <- c(12,12,12.9,13.6,16.6,17.2,
               17.5,18.2,19.1,19.3,19.8,20.3,
              20.5,20.6,21.3,21.6,22.1,22.2,
              22.6,23.1,24.0,24.3,26.7,29.7,
              5,5.4,6.1,10.9,11.8,12,
               12.3,14.8,15,16.8,17.2,17.2,
               17.4,17.5,18.5,18.7,18.7,19.2,
               19.5,20.7,21.2,22.1,24)
View(creativity)
typeof(creativity)
intrinsic <- creativity[1:24]
extrinsic <- creativity[25:47]
df.int<-as.data.frame(intrinsic)
df.ext<-as.data.frame(extrinsic)

# Data Label Assigning
labels1 <- replicate(24,"Int")
labels2 <- replicate(23,"Ext")
labels <- c(labels1,labels2)
typeof(labels)
data101 <- data.frame(creativity,labels)

typeof(data101)
View(data101)

# Basic Statistics
summary(intrinsic)
summary(extrinsic)
summary(data101)
estimated.difference <- mean(intrinsic)-mean(extrinsic)
estimated.difference

# Descriptive Statistics
# Scatter Plots
rangedata <- range(0,intrinsic,extrinsic)
rangedata <- round(rangedata)
plot(intrinsic, type="p", col="blue", axes=F, ann=F)  
lines(extrinsic, type="p", col="red", pch=4, lty=2)   #Adds the data into previous plot
axis(1, at=1:24, lab=1:24, las=0)                     #Changes X-Axis Labels
axis(2, las=1, at=0:rangedata[2])                     #Change Y-Axis Labels with the 'Data Range' (las=3 to rotate the label)
title(xlab="Sample Number", col.lab="black")          #Defines X-Axis Title
title(ylab="Creativity Score", col.lab="black")       #Defines Y-Axis Title
title(main="Creativity Scores - Scatter Plot", col.main="black", font.main=1)    
box()                                                 #Creates a box around the plot
legend(1,rangedata[2], c("Intrinsic Group","Extrinsic Group"), col=c("blue","red"), pch=c(1,4))  

# Box Plots
boxplot(intrinsic,extrinsic,data=data101, main="Creativity Score Distribution", xlab="Intrinsic Group & Extrinsic Group", ylab="Creativity Score", col=c("blue","red"))

# Histograms
intrange <- range(intrinsic)
extrange <- range(extrinsic)

hist(intrinsic,
     main="Histogram for Intrinsic Group",
     xlab="Creativity Scores",
     border="blue",
     col="green",
     las=1,
     breaks=24,
     xlim=c(10,30))
hist(extrinsic,
     main="Histogram for Extrinsic Group",
     xlab="Creativity Scores",
     border="blue",
     col="red",
     las=1,
     breaks=23,
     xlim=c(0,30))

# Inferential Statistics
t.test(creativity~labels,alternative="two.sided",data=data101)