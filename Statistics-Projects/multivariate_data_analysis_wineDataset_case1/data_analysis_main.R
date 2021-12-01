#########################################################
#ITU END620 - UYGULAMALI COK DEGISKENLI ISTATISTIK ANALIZ
#UYGULAMA ODEVI (2018-2019 BAHAR DONEMI)
#HAZIRLAYAN UGUR URESIN (507172109)
#########################################################

#ATTENTION! PLEASE MODIFY BELOW DIRECTORY ACCORDING TO YOUR W.DIRECTORY!
########################################################################
setwd("/Users/ugur/Desktop/R/myprojects/END620/data")

## Dataset importing
dataset = read.csv('Wine.csv')
dataset -> dataset0 #as a spare
#View(dataset)

## Initial data checks
summary(dataset[,1:14])
str(dataset)

## Visual data checks
# Combined Scatter Plot
independents <- data.frame(dataset[,-14])
dim(independents)
plot(independents,dataset$Customer_Segment)

# Box-PLOTS
boxplot(scale(dataset)) #Variables needs to be scaled before being plotted together!

boxplot(Alcohol ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Alcohol")

boxplot(Malic_Acid ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Malic_Acid")

boxplot(Ash ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Ash")

boxplot(Ash_Alcanity ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Ash_Alcanity")

boxplot(Magnesium ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Magnesium")

boxplot(Total_Phenols ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Total_Phenols")

boxplot(Flavanoids ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Flavanoids")

boxplot(Nonflavanoid_Phenols ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Nonflavanoid_Phenols")

boxplot(Proanthocyanins ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Proanthocyanins")

boxplot(Color_Intensity ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Color_Intensity")

boxplot(Hue ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Hue")

boxplot(OD280 ~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="OD280")

boxplot(Proline~ Customer_Segment,
        data=dataset,
        xlab="Customer Segment",
        ylab="Proline")

# QQ-Plots
library(car)
qqPlot(dataset$Alcohol)
qqPlot(dataset$Malic_Acid)
qqPlot(dataset$Ash)
qqPlot(dataset$Ash_Alcanity)
qqPlot(dataset$Magnesium)
qqPlot(dataset$Total_Phenols)
qqPlot(dataset$Flavanoids)
qqPlot(dataset$Nonflavanoid_Phenols)
qqPlot(dataset$Proanthocyanins)
qqPlot(dataset$Color_Intensity)
qqPlot(dataset$Hue)
qqPlot(dataset$OD280)
qqPlot(dataset$Proline)

# Shapiro-Wilk test
SW_list0 <- lapply(dataset[,-14],shapiro.test)        #Apply SW test to the dataset
SW_list0_p <- lapply(SW_list0, '[', "p.value")        #Find p-values from SW test
lapply(SW_list0_p,function(x)which(x>=0.05))          #Check which value(s) is bigger than 0.05
unlist(lapply(SW_list0_p,function(x)which(x>=0.05)))  #Show which value(s) is bigger than 0.05

# Shapiro-Wilk test
norm01 <- shapiro.test(dataset$Alcohol)
norm02 <- shapiro.test(dataset$Malic_Acid)
norm03 <- shapiro.test(dataset$Ash)
norm04 <- shapiro.test(dataset$Ash_Alcanity)
norm05 <- shapiro.test(dataset$Magnesium)
norm06 <- shapiro.test(dataset$Total_Phenols)
norm07 <- shapiro.test(dataset$Flavanoids)
norm08 <- shapiro.test(dataset$Nonflavanoid_Phenols)
norm09 <- shapiro.test(dataset$Proanthocyanins)
norm10 <- shapiro.test(dataset$Color_Intensity)
norm11 <- shapiro.test(dataset$Hue)
norm12 <- shapiro.test(dataset$OD280)
norm13 <- shapiro.test(dataset$Proline)
#
SW_scores <- list("V1"=norm01$p.value,
                  "V2"=norm02$p.value,
                  "V3"=norm03$p.value,
                  "V4"=norm04$p.value,
                  "V5"=norm05$p.value,
                  "V6"=norm06$p.value,
                  "V7"=norm07$p.value,
                  "V8"=norm08$p.value,
                  "V9"=norm09$p.value,
                  "V10"=norm10$p.value,
                  "V11"=norm11$p.value,
                  "V12"=norm12$p.value,
                  "V13"=norm13$p.value)
SW_scores
#

# Data Transforms
dataset0 -> dataset

#Variable4
sqrt(dataset$Ash_Alcanity) -> dataset$Ash_Alcanity
shapiro.test(dataset$Ash_Alcanity)
qqPlot(dataset$Ash_Alcanity)
#
#Variable9
sqrt(dataset$Proanthocyanins) -> dataset$Proanthocyanins
shapiro.test(dataset$Proanthocyanins)
qqPlot(dataset$Proanthocyanins)
#
#Variable10
log(dataset$Color_Intensity) -> dataset$Color_Intensity
shapiro.test(dataset$Color_Intensity)
qqPlot(dataset$Color_Intensity)
#

# Outlier Detection
outliers01 <- boxplot(dataset0[,1], plot=FALSE)$out
outliers02 <- boxplot(dataset0[,2], plot=FALSE)$out
outliers03 <- boxplot(dataset0[,3], plot=FALSE)$out
outliers04 <- boxplot(dataset0[,4], plot=FALSE)$out
outliers05 <- boxplot(dataset0[,5], plot=FALSE)$out
outliers06 <- boxplot(dataset0[,6], plot=FALSE)$out
outliers07 <- boxplot(dataset0[,7], plot=FALSE)$out
outliers08 <- boxplot(dataset0[,8], plot=FALSE)$out
outliers09 <- boxplot(dataset0[,9], plot=FALSE)$out
outliers10 <- boxplot(dataset0[,10], plot=FALSE)$out
outliers11 <- boxplot(dataset0[,11], plot=FALSE)$out
outliers12 <- boxplot(dataset0[,12], plot=FALSE)$out
outliers13 <- boxplot(dataset0[,13], plot=FALSE)$out
print(outliers01)
print(outliers02)
print(outliers03)
print(outliers04)
print(outliers05)
print(outliers06)
print(outliers07)
print(outliers08)
print(outliers09)
print(outliers10)
print(outliers11)
print(outliers12)
print(outliers13)

# Correlation Check
library(psych)
pairs.panels <- pairs.panels(dataset[-14])
pwc <- corr.test(dataset[-14], use="pairwise", adjust="holm")   
cormat0 <- pwc$r  
cormat0

# Bartletts TEST
datalist <- list(dataset0$Alcohol,
                 dataset0$Malic_Acid,
                 dataset0$Ash,
                 dataset0$Ash_Alcanity,
                 dataset0$Magnesium,
                 dataset0$Total_Phenols,
                 dataset0$Flavanoids,
                 dataset0$Nonflavanoid_Phenols,
                 dataset0$Proanthocyanins,
                 dataset0$Color_Intensity,
                 dataset0$Hue,
                 dataset0$OD280,
                 dataset0$Proline,
                 dataset0$Customer_Segment)
bartlett.test(datalist)

# Kaiser, Meyer, Olkin Measure of Sampling Adequacy (KMO)
KMO(as.matrix(dataset0))

# PCA (PRINCIPAL COMPONENT ANALYSIS)
####################################

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8) #80% percent for training
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Pre-Checks
data_pca <- princomp(training_set[-14], scores=T, cor=T, covmat=NULL)
plot(data_pca)                                            #PC barchart
screeplot(data_pca, type="line", main="Scree Plot")       #Scree plot
sd_comps <- data_pca$sdev                                 #Standart deviations
barplot(data_pca$sdev/sum(data_pca$sdev))                 #PC barchart in st. dev.
cum_facs <- cumsum(data_pca$sdev/sum(data_pca$sdev))      #Cumulative sum of pca ratios
fac_cutoff = 0.33
as.numeric(which(cum_facs>=fac_cutoff)[1]) -> xfactors    #Finds num of factors > fac_cutoff
xfactors

# Applying PCA
# install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x = training_set[-14], method = 'pca', pcaComp = xfactors)
training_set = predict(pca, training_set)
training_set = training_set[c(2, 3, 1)]
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])
y_pred

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'PCA & SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'PCA & SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))



# LDA (LINEAR DISCRIMINANT ANALYSIS)
####################################
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
#library(caTools)
#set.seed(123)
#split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

# Feature Scaling
#training_set[-14] = scale(training_set[-14])
#test_set[-14] = scale(test_set[-14])

# Applying LDA
library(MASS)
lda = lda(formula = Customer_Segment ~ ., data = training_set)
training_set2 = as.data.frame(predict(lda, training_set))
training_set2 = training_set2[c(5, 6, 1)]
test_set2 = as.data.frame(predict(lda, test_set))
test_set2 = test_set2[c(5, 6, 1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier2 = svm(formula = class ~ .,
                 data = training_set2,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred2 = predict(classifier2, newdata = test_set2[-3])

# Making the Confusion Matrix
cm2 = table(test_set2[, 3], y_pred2)
cm2

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier2, newdata = grid_set)
plot(set[, -3],
     main = 'LDA & SVM (Training set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier2, newdata = grid_set)
plot(set[, -3], main = 'LDA & SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
#
# END OF THE CODE#
# UGUR URESIN , uresinugur35@gmail.com#
#
