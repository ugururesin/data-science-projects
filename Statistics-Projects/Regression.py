### REGRESSION ###

#LINEAR REGRESSION ASSUMPTIONS
#1 Linearity
#2 Homoscedasticity
#3 Multivariate normality
#4 Independence of error (The Durbin-Watson Statistic)
#5 Lack of multicollinearity
#Check also: https://murraylax.org/m230/spring2009/assumptions_print.pdf

#SIMPLE LINEAR REGRESSION IN PYTHON
###########################

#IMPORTING THE LIBRARIES
import numpy as np
import matplotlib.plylot as plt
import pandas as pd
#NOTE: Cmd+i gives the info for the seleceted function


#IMPORTING THE DATASET
dataset=pd.read_csv('.csv')
x = dataset.iloc[:, :-1].values		#to create x-column vector
y = dataset.iloc[:, :1].values		#to create y-column vector 

#SPLITTING THE DATASET INTO THE TRAINING SET AND TEST SET
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=1/3, random_state=0)

# Feature Scaling


# FITTING SIMPLE LINEAR REGRESSION TO THE TRAINING SET
from sklearn.linear_model import LinearRegression #LinearRegression class from the sklearn.linear_model package
regressor = LinearRegression()
#regressor. #to see the sub methods in the regressor
regressor.fit(X_train, y_train)


# PREDICTING THE TEST SET RESULTS
y_pred = regressor.predict(X_test)


#VISUALING THE TRAINING SET RESULTS
plt.scatter(X_train, y_train, color='red')
plt.plot(X_train, regressor.predict(X_train))
plt.title('Salary vs Experince (Training set)')
plt.xlabel('Years of Experince')
plt.ylabel('Salary')
plt.show

#VISUALING THE TEST SET RESULTS
plt.scatter(X_test, y_test, color='red')
plt.plot(X_train, regressor.predict(X_train))	#initial training model!
plt.title('Salary vs Experince (Test set)')
plt.xlabel('Years of Experince')
plt.ylabel('Salary')
plt.show

#
#
#

#MULTIPLE LINEAR REGRESSION IN PYTHON
#############################

