# Statistical Sleuth Case 7.1 
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Edwin Hubble used the Mount Wilson Observatory telescopes
# to measure features of nebulae outside the Milky Way.
# He found a positive correlation between a nebula's distance from earth
# and the velocity with which it was going away from the earth.
#

# RESEARCH QUESTIONS
# Is the relationship between distance and velocity indeed a straight line?
# Is the y-intercept in the straight line equation zero, as the Big Bang theory predicts?
# How old is the universe?


# Data Import and Data Check
setwd("/Users/ugur/Desktop/R/myprojects/ISL601E/data/excel")
data.71 <- read.csv("CASE0701.CSV", sep=";", header = T) #please import the data from where you store
rm(list=setdiff(ls(), c("data.71","data.72"))) #to clean global environment by keeping initial data
                        
# Check the data
#View(data.71)
summary(data.71)
str(data.71)

# Dimensions and Vectorizing
dim(data.71)[1] -> nr
dim(data.71)[2] -> nc
data.71$VELOCITY -> x1
data.71$DISTANCE -> y1

# Summarize data
#install.packages("FSA") #please active it if the package is not installed before
#library(FSA)
Summarize(x1, data=data.71, digits=3)
Summarize(y1, data=data.71, digits=3)

# Data Visualitaion: Scatter plot
# ATTENTION: If you don't have the ggplot2 package, please install it as follows:
#install.packages("ggplot2")
data.71 %>%
  ggplot(aes(x=x1, y=y1))+
  geom_point()+
  labs(x='Recession Velocity [km/sec]',
       y="Distance [megaparsecs]",
       title="Measured distance vs. Velocity")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm')

#Histograms and the Normality
hist(y1,breaks=10, prob=T, col='blue',
     xlab="Distance [megaparsecs]",
     ylab="Density",
     main="Histogram of Distances")
lines(density(y1, n=512),col="Red",lwd=2)

hist(x1,breaks=10, prob=T, col='blue',
     xlab="Velocity [km/sec]",
     ylab="Density",
     main="Histogram of Velocities")
lines(density(x1, n=512),col="Red",lwd=2)

#Correlation
cor(x1,y1)        #Correlation coefficient
cor.test(x1,y1)   #Pearson's product-moment correlation

## Simple Linear Regression
#Linear Regresion Model
mymodel.1 <- lm(DISTANCE~VELOCITY, data = data.71) 
#Model Coefficients
mymodel.1 #??(distance) = .39917 + .001372*VELOCITY
summary(mymodel.1)
#Confidence intervals of the slope and intercept of linear regression of mymodel
confint(mymodel.1)
#Predicted values (??)
(yhat <- fitted(mymodel.1))

# The-Big Bang Theory (Straight Line Relation btw. Y and X)
mymodel.2 <- lm(DISTANCE~VELOCITY-1, data = data.71) #without intercept!
#Model Coefficients
mymodel.2 #??(distance) = .001921*VELOCITY
summary(mymodel.2)
#Confidence intervals of the slope and intercept of linear regression of mymodel
confint(mymodel.2)

#Since it's given that .001922 megaparsecs-seconds per kilometer equals 1.88 billion years
#1.88/.001922=978.1478
confint(mymodel.2)*978.1478 #converted to billion years

#Fitted Values and the Residuals Table
table7.8=list(BigBangData=(cbind(i=c(1:24),
              Velocity=data.71$VELOCITY,
              Distance=data.71$DISTANCE,
              fit_i=yhat,
              res_i=yhat-data.71$DISTANCE)))
table7.8[] <- lapply(table7.8,round,2) #2 digits for the list
table7.8

#Conclusion
#In the first model the p-value for the intercept is found as 0.0028
#It's an evidence against the null hypothesis that the intercept equals zero.
#Then a linear regression model without an intercept is fitted
#It's found that the value of the line at velocity zero is apparently not zero,
#as predicted by the theory (two-sided p-value = .0028 for a test that the intercept equals zero).

###################################################
###################################################

# Statistical Sleuth Case 7.2
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# A certain kind of meat processing may begin once the pH
# in postmortem muscle of a steer carcass decreases to 6.0,
# from a pH at time of slaughter of around 7.0 to 7.2.
# An estimate is needed of the time after slaughter at which the pH reaches 6.
# To estimate this time, 10 steer carcasses were assigned to be measured
# for pH at one of five times after slaughter.


# Data Import and Data Check
data.72 <- read.csv("CASE0702.CSV", sep=";", header = T) #please import the data from where you store

# Check the data
#View(data.72)
summary(data.72$TIME)
summary(data.72$PH)
str(data.72)

# Vectorizing
data.72$TIME -> x2
data.72$PH -> y2

# Summarize data
#install.packages("FSA") #please active it if the package is not installed before
#library(FSA)
Summarize(y2 ~ x2, data=data.72, digits=3)

# Data Visualitaion: Scatter plot
# ATTENTION: If you don't have the ggplot2 package, please install it as follows:
#install.packages("ggplot2")
data.72 %>%
  ggplot(aes(x=x2, y=y2))+
  geom_point()+
  labs(x='Hours',
       y="pH",
       title="pH vs. Time after Slaughter")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm')+
  geom_hline(yintercept=6, linetype="dashed",color="red",size=0.5)

data.72 %>%
  ggplot(aes(x=log(x2), y=y2))+
  geom_point()+
  labs(x='Log Hours',
       y="pH",
       title="pH vs. Time after Slaughter")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm')+
  geom_hline(yintercept=6, linetype="dashed",color="red",size=0.5)
  
#Histograms and the Normality
hist(y2,breaks=5, prob=T, col='blue',
     xlab="pH Level",
     ylab="Density",
     main="Histogram of pH")
lines(density(y2, n=512),col="Red",lwd=2)

#Correlation
cor(x2,y2)        #Correlation coefficient
cor.test(x2,y2)   #Pearson's product-moment correlation

## Simple Linear Regression
#Linear Regresion Model (HOUR TIME)
mymodel.3 <- lm(PH~TIME, data = data.72) 
#Model Coefficients
mymodel.3 #??(pH) =6.9965-.2087*TIME
summary(mymodel.3)
#Confidence intervals of the slope and intercept of linear regression of mymodel
confint(mymodel.3)
#Predicted values (??)
(yhat.3 <- fitted(mymodel.3))
#
#Beta Coefficients and the Sigma
(b0_3 = coef(mymodel.3)["(Intercept)"])
(b1_3 = coef(mymodel.3)["TIME"])
(sigma1_3=summary(mymodel.3)$sigma)
#
#SE Parameters
n <- length(data.72$TIME)
mean.time_3 <- mean(data.72$TIME)
sx3 <- sd(data.72$TIME)^2
#
#Prediction for pH of 4 hours after slaughter
b0_3 <- as.numeric(b0_3)
b1_3 <- as.numeric(b1_3)
Pred_Y_x4 = b0_3 + b1_3*(4)
Pred_Y_x4 #predicted Y value when x=4 Hours!
#
SE.fun <- function(x){
  sigma1_3*sqrt(0+
                1/n+
                (x-mean.time_3)^2/((n-1)*sx3))
}
#
SE_Pred_Y_x4 <- SE.fun(4)
SE_Pred_Y_x4
#
#Confidence Interval for pH of 4 hours after slaughter
t.cr <- qt(.975,n-2) #critical t-value=2.306
(conf.int.ph.x4 = cbind("LowerLimit"=Pred_Y_x4-t.cr*SE_Pred_Y_x4,
                        "UpperLimit"=Pred_Y_x4+t.cr*SE_Pred_Y_x4))
#
#All confidence intervals (hour-time only)
lowerlimits = (b0_3+b1_3*data.72$TIME)-t.cr*SE.fun(data.72$TIME)
upperlimits = (b0_3+b1_3*data.72$TIME)+t.cr*SE.fun(data.72$TIME)

table7.12=list(MeatData=(cbind(i=c(1:10),
                               TIME=data.72$TIME,
                               pH=data.72$PH,
                               fit_i=yhat.3,
                               res_i=(yhat.3-data.72$PH),
                               lowlim=lowerlimits,
                               uplim=upperlimits)))
table7.12[] <- lapply(table7.12,round,2) #2 digits for the list
table7.12$MeatData[unique(table7.12$MeatData[,2]),] #unique observations only


## Simple Linear Regression
#Linear Regresion Model (LOG TIME)
data.72_xlog <- as.data.frame(cbind(log(data.72$TIME),data.72$PH))
colnames(data.72_xlog)=c("logTIME","PH")
mymodel.4 <- lm(PH~logTIME, data = data.72_xlog) 
#Model Coefficients
mymodel.4 #??(pH) = .001921*TIME
summary(mymodel.4)
#Confidence intervals of the slope and intercept of linear regression of mymodel
confint(mymodel.4)
#Predicted values (??)
(yhat.4 <- fitted(mymodel.4))
#
#Beta Coefficients and the Sigma
(b0_4 = coef(mymodel.4)["(Intercept)"])
(b1_4 = coef(mymodel.4)["logTIME"])
(sigma1_4=summary(mymodel.4)$sigma)
#
#SE Parameters
n <- length(data.72_xlog$logTIME)
mean.logtime_4 <- mean(data.72_xlog$logTIME)
sx4 <- sd(data.72_xlog$logTIME)^2
#
#Prediction for pH of 4 hours after slaughter
b0_4 <- as.numeric(b0_4)
b1_4 <- as.numeric(b1_4)
Pred_Y_xlog4 = b0_4 + b1_4*log(4)
Pred_Y_xlog4
#
SE.fun <- function(x){
  sigma1_4*sqrt(1+
              (1/n)+
              (log(x)-mean.logtime_4)^2/((n-1)*sx4))
}
#
SE_Pred_Y_xlog4 <- SE.fun(4)
SE_Pred_Y_xlog4
#
#Confidence Interval for pH of 4 hours after slaughter
t.cr <- qt(.975,n-2) #critical t-value=2.306
(conf.int.ph.x4 = cbind("LowerLimit"=Pred_Y_xlog4-t.cr*SE_Pred_Y_xlog4,
                        "UpperLimit"=Pred_Y_xlog4+t.cr*SE_Pred_Y_xlog4))


##CALIBRATION PROBLEM (INVERSE PREDICTION)
#Estimating the X that results Y=Y0
#
Y0=6 #desired Y value
(xhat_3=(Y0-b0_3)/b1_3) #for hour time
(xhat_4=(Y0-b0_4)/b1_4) #for log time
xhour.mean = mean(data.72$TIME)
xloghour.mean = mean(data.72_xlog$logTIME)
#
#Inverse prediction for HOUR-TIME
SE.fun2 <- function(x){
  sigma1_3*sqrt(1+
                (1/n)+
                (xhat_3-xhour.mean)^2/((n-1)*sx3))
}
#
(SE_inv_pred_xhour <- SE.fun2(0)/abs(b1_3))
#
#Confidence interval for hour time
(conf.int.pred.Y6_hour = cbind("LowerLimit"=xhat_3-t.cr*SE_inv_pred_xhour,
                               "UpperLimit"=xhat_3+t.cr*SE_inv_pred_xhour))
#
#
#Inverse prediction for LOG-TIME
#
SE.fun3 <- function(x){
  sigma1_4*sqrt(1+
                (1/n)+
                (xhat_4-xloghour.mean)^2/((n-1)*sx4))
}
#
(SE_inv_pred_xloghour <- SE.fun3(0)/abs(b1_4))
#
exp(1)^xhat_4
#Confidence interval for logtime
(conf.int.pred.Y6_loghour = cbind("LowerLimit"=xhat_4-t.cr*SE_inv_pred_xloghour,
                                  "UpperLimit"=xhat_4+t.cr*SE_inv_pred_xloghour))
#Time-prediction in Hours
exp(1)^conf.int.pred.Y6_loghour[,1]
exp(1)^conf.int.pred.Y6_loghour[,2]
