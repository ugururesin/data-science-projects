#########################
# Multivariate Analysis #
#########################

# DEFINITION
# If there is more than one outcomes (y1, y2...), it's 'Multivariate'
# Example: Y1 + Y2 + Y3 + Y4 = B0 + B1*X1 + B2*X2 + (B1,B2,B3) 

# ASSUMPTIONS: Normality, Positive Determinant, Equal Variance-Covariance Btw Two Groups
# ISSUES: Can make a difference in the analysis results

# TESTS
# t-test        -> H0: u1=u2
# ANOVA         -> H0: u1=u2=u3=u4... (Scalar)
# Hotelling T2  -> H0: (u1=u2=u3)=(x1=x2=x3)
# MANOVA        -> H0: (u11,u12,u13)=(u21,u22,u23)=(x31,x32,x33)=... (Vectoral)
## One-Way MANOVA: One group variable
## Factorial MANOVA: More than one group variables


## Hotelling T2 ##
##################

## DATA
attach(attitude)
View(attitude)
mydata <- data.frame(attitude$complaints, attitude$learning, attitude$raises)
#
summary(mydata)
describe(mydata)

## ASSUMPTION TESTING
#1. Testing the Normality Assumption
install.packages("mvnormtest")
library(mvnormtest)
transpose_attitude <- t(mydata)
mshapiro.test(transpose_attitude)

#2. Testing Positive Determinant 
CovAttitude <- cov(mydata)
det(CovAttitude)

#3. Testing Equality of Variance-Covariance Matrices of Groups (Genders)
group <- rep(c("male","female"),c(15,15))
factor(group)
group

#Box's M-test for Homogeneity of Covariance Matrices
boxM(mydata,group)

#Graph the means of the 3 variables for males and females
#install.packages("gplots")
library(gplots)

plotmeans(attitude.complaints ~ group, data=mydata, ylim=c(0,100), xlab="Groups",
          legends=c("Males","Females"), main="Attitude", connect=FALSE, mean.labels = TRUE,
          col=NULL, p=1.0)

plotmeans(attitude.learning ~ group, data=mydata, ylim=c(0,100), xlab="Groups",
          legends=c("Males","Females"), main="Attitude", connect=FALSE, mean.labels = TRUE,
          col=NULL, p=1.0)

plotmeans(attitude.raises ~ group, data=mydata, ylim=c(0,100), xlab="Groups",
          legends=c("Males","Females"), main="Attitude", connect=FALSE, mean.labels = TRUE,
          col=NULL, p=1.0)

#Perform Hotelling T2 Test
install.packages("ICSNP")
library(ICSNP)
HotellingsT2(mydata[1:15,], mydata[16:30,])
##################


## MANOVA ##
############
# Hotelling T2  -> multivariate extension of t-test for means of 2 groups
# MANOVA        -> multivariate extension of ANOVA

# 4 test statistics for MANOVA (All calculates an approximate F value)
# Wilk's Lambda
# Hotelling-Lawley Trace
# Pillai Trace
# Roy's Maximum Root

#One-Way MANOVA
install.packages("car")
library(car)
data(Baumann) #Baumann education data
attach(Baumann)
?Baumann

# HYPOTHESIS
# H0: The joint mean of 3 tests among the 3 teaching techniques ARE NOT different!

#Testing the Independence Assumption
install.packages("psych")
library("psych")
ICC(Baumann[,4:6])

group = factor(group) #encode group as a categoriacal variable
Y = cbind(post.test.1, post.test.2, post.test.3)
Baumann.manova = manova(Y~group)

summary(Baumann.manova, test="Pillai")
summary(Baumann.manova, test="Wilks")
summary(Baumann.manova, test="Hotelling-Lawley")
summary(Baumann.manova, test="Roy")


# Factorial MANOVA
# DATA: 3 outcome variables: pH, Density, Conductivity 
# Independents: Contours (top,slope,depression), depths(0-10,10-30,30-60,60-90 cm)
data(Soils)
attach(Soils)
View(Soils)
options(scipen=999)

# HYPOTHESIS
# H0_1: The joint mean of 3 variables (pH, Density and Conductivity) ARE NOT
# significantly different among the categories of the variable Contours/Depths
# H0_2: The joint mean of 3 variables (pH, Density and Conductivity) ARE NOT
# significantly different among the categories of the variable Contours*Depths interaction


install.packages("MASS")
library(MASS)

Countour <- factor(Contour)
Contour
Depth <- factor(Depth)
Depth

soils.mod <- lm(cbind(pH,Dens,Conduc)~Contour+Depth+Contour*Depth-1,data=Soils)
summary(soils.mod)
#In summary it can be seen that all interaction values are bigger than alpha(0.05)
#Thus, interaction values do not influence to the outcome values
#In this case, we can use TYPE(II) SS in order to test H0

Manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Wilks"))
Manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Pillai"))
Manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Hotelling-Lawley"))
Manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Roy"))
#In results it can be seen that the interaction values are bigger than alpha(0.05)
#In this case, we can NOT reject H0_2 !!!

Manova(soils.mod, multivariate=TRUE, type=c("III"), test="Wilks")
Manova(soils.mod, multivariate=TRUE, type=c("III"), test=("Pillai"))
Manova(soils.mod, multivariate=TRUE, type=c("III"), test=("Hotelling-Lawley"))
Manova(soils.mod, multivariate=TRUE, type=c("III"), test=("Roy"))