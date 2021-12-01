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

## Initial data checks
summary(dataset[,1:14])
str(dataset)

## Visual data checks
# Combined Scatter Plot
independents <- data.frame(dataset$Alcohol,
                           dataset$Malic_Acid,
                           dataset$Ash,
                           dataset$Ash_Alcanity,
                           dataset$Magnesium,
                           dataset$Total_Phenols,
                           dataset$Flavanoids,
                           dataset$Nonflavanoid_Phenols,
                           dataset$Color_Intensity,
                           dataset$Hue,
                           dataset$OD280,
                           dataset$Proline)

plot(independents,dataset$Customer_Segment)

# Box-PLOTS
boxplot(dataset[,-c(5,13)])

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
norm01$p.value
norm02$p.value
norm03$p.value
norm04$p.value
norm05$p.value
norm06$p.value
norm07$p.value
norm08$p.value
norm09$p.value
norm10$p.value
norm11$p.value
norm12$p.value
norm13$p.value

# Normality Check for log(Variable_#4))
shapiro.test(log(dataset$Ash_Alcanity))
qqPlot(log(dataset$Ash_Alcanity))
#
# LOG-Transformation
dataset$Ash_Alcanity <- log(dataset$Ash_Alcanity)
shapiro.test(dataset$Ash_Alcanity)


# Correlation Check
pairs.panels <- pairs.panels(dataset[-14])
pwc <- corr.test(dataset[-14], use="pairwise", adjust="holm")   
cormat0 <- pwc$r  
cormat0

# Bartletts TEST
datalist <- list(dataset$Alcohol,
                 dataset$Malic_Acid,
                 dataset$Ash,
                 dataset$Ash_Alcanity,
                 dataset$Magnesium,
                 dataset$Total_Phenols,
                 dataset$Flavanoids,
                 dataset$Nonflavanoid_Phenols,
                 dataset$Proanthocyanins,
                 dataset$Color_Intensity,
                 dataset$Hue,
                 dataset$OD280,
                 dataset$Proline,
                 dataset$Customer_Segment)
bartlett.test(datalist)

# Kaiser, Meyer, Olkin Measure of Sampling Adequacy (KMO)
KMO(as.matrix(dataset))

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
barplot(data_pca$sdev/sum(data_sce1_pca$sdev))            #PC barchart in st. dev.
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
     main = 'PCA (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'PCA (Test set)',
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
     main = 'LDA (Training set)',
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
plot(set[, -3], main = 'LDA (Test set)',
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