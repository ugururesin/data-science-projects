# Statistical Sleuth Case 5.1 
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# A series of studies involving several species of animals found that
# restricting caloric intake can dramatically increase life expectancy.
#
# The questions of interest involve specific comparisons of treatments as diagrammed in
# (a) Does lifetime on the 50 kcal/wk diet exceed the lifetime on the 85 kcal/wk diet?
# If so, by how much? (This calls for a comparison of the N/R50 group to the N/N85 group.)
#
# (b) Is lifetime affected by providing a reduced calorie diet before weaning,
# given that a 50 kcal/wk diet is provided after weaning?
# (This calls for a comparison of the R/R50 group to the N/R50 group.)
#
# (c) Does lifetime on the 40kcal/wk diet exceed the lifetime on the 50 kcal/wk diet? 
# (This calls for a comparison of the N/R40 group to the N/R50 group.)
#
# (d) Given a reduced calorie diet of 50 kcal/week,
# is there any additional effect on lifetime due to decreasing the protein intake? 
# (This calls for a comparison of the N/R50 lopro diet to the N/R50 diet.)
#
# (e) Is there an effect on lifetime due to restriction at 85 kcal/week?
# This would indicate the extent to which the 85 kcal/wk diet served as a proper control
# and possibly whether there was any effect of restricting the diet, even with a standard caloric intake.
# (This calls for a comparison of the N/N85 group to the NP group.)

# Data Import and Data Check
data.51 <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store

# Check the data
View(data.51)
summary(data.51)
str(data.51)

# Dimensions
dim(data.51)[1] -> nr
dim(data.51)[2] -> nc
df <- nr-length(unique(data.51$DIET)) #unique gives the number of different factors/labels

# Label-Partition
which(data.51$DIET=="NP") -> NP.label.indices
which(data.51$DIET=="N/N85") -> NN85.label.indices
which(data.51$DIET=="lopro") -> lopro.label.indices
which(data.51$DIET=="N/R50") -> NR50.label.indices
which(data.51$DIET=="N/R40") -> NR40.label.indices
which(data.51$DIET=="R/R50") -> RR50.label.indices
#
# Data-Partition
data.51[NP.label.indices,] -> NP.data
data.51[NN85.label.indices,] -> NN85.data
data.51[lopro.label.indices,] -> lopro.data
data.51[NR50.label.indices,] -> NR50.data
data.51[NR40.label.indices,] -> NR40.data
data.51[RR50.label.indices,] -> RR50.data

# Summarize data
#install.packages("FSA") #please active it if the package is not installed before
#library(FSA)
Summarize(LIFETIME ~ DIET, data=data.51, digits=2)

# Pooled-Standart Deviation
sp2 <- 44.647 #calculated by hand
sp <- sqrt(sp2)

# Data Visualitaion
stem(data.51[,1])  #stem-and-leaf plot

boxplot(LIFETIME ~ DIET,
        data=data.51,
        xlab="Diet Groups",
        ylab="Months Survived")

# ANOVA TEST (Analysis of Variance)
anova5.1 <- anova(lm(LIFETIME ~ DIET, data=data.51)) #There is a strong statistically significant difference between the diet
anova5.1
summary(lm(LIFETIME ~ DIET, data=data.51))

# PAIRWISE COMPARISON
#
# Case-a
pairs.a <- rbind(NR50.data, NN85.data)
length(pairs.a$LIFETIME)[1] -> nr.a
summary(pairs.a)
(difY.a <- abs(mean(NR50.data$LIFETIME)-mean(NN85.data$LIFETIME)))
(SE.difY.a <- sp*sqrt(1/length(NR50.data$LIFETIME)+1/length(NN85.data$LIFETIME)))
(conf.int.a <- c(difY.a-1.96*SE.difY.a,difY.a+1.96*SE.difY.a))
(t.a <- difY.a/SE.difY.a)
tt.a <- t.test(LIFETIME ~ DIET, var.equal=TRUE, data=pairs.a, alternative="less")
tt.a$p.value

# Case-b
pairs.b <- rbind(NR50.data, RR50.data)
length(pairs.b$LIFETIME)[1] -> nr.b
summary(pairs.b)
(difY.b <- abs(mean(NR50.data$LIFETIME)-mean(RR50.data$LIFETIME)))
(SE.difY.b <- sp*sqrt(1/length(NR50.data$LIFETIME)+1/length(RR50.data$LIFETIME)))
(conf.int.b <- c(difY.b-1.96*SE.difY.b,difY.b+1.96*SE.difY.b))
(t.b <- difY.b/SE.difY.b)
tt.b <- t.test(LIFETIME ~ DIET, var.equal=TRUE, data=pairs.b, alternative="less")
tt.b$p.value

# Case-c
pairs.c <- rbind(NR40.data, NR50.data)
length(pairs.c$LIFETIME)[1] -> nr.c
summary(pairs.c)
(difY.c <- abs(mean(NR40.data$LIFETIME)-mean(NR50.data$LIFETIME)))
(SE.difY.c <- sp*sqrt(1/length(NR40.data$LIFETIME)+1/length(NR50.data$LIFETIME)))
(conf.int.c <- c(difY.c-1.96*SE.difY.c,difY.c+1.96*SE.difY.c))
(t.c <- difY.c/SE.difY.c)
2*pt(-abs(t.c),df=nr.c-1)

# Case-d
pairs.d <- rbind(lopro.data, NR50.data)
length(pairs.d$LIFETIME)[1] -> nr.d
summary(pairs.d)
(difY.d <- abs(mean(lopro.data$LIFETIME)-mean(NR50.data$LIFETIME)))
(SE.difY.d <- sp*sqrt(1/length(lopro.data$LIFETIME)+1/length(NR50.data$LIFETIME)))
(conf.int.d <- c(difY.d-1.96*SE.difY.d,difY.d+1.96*SE.difY.d))
(t.d <- difY.d/SE.difY.d)
2*pt(-abs(t.d),df=nr.d-1)

# Case-e
pairs.e <- rbind(NN85.data, NP.data)
length(pairs.e$LIFETIME)[1] -> nr.e
summary(pairs.e)
(difY.e <- abs(mean(NN85.data$LIFETIME)-mean(NP.data$LIFETIME)))
(SE.difY.e <- sp*sqrt(1/length(NN85.data$LIFETIME)+1/length(NP.data$LIFETIME)))
(conf.int.e <- c(difY.e-1.96*SE.difY.e,difY.e+1.96*SE.difY.e))
(t.e <- difY.e/SE.difY.e)
2*pt(-abs(t.e),df=nr.e-1)

###################################################

###################################################

# Statistical Sleuth Case 5.2
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Dr. Benjamin Spock was tried in US in 1968
# The defense in that case challenged the method by which jurors were selected,
# claiming that women-many of whom had raised children according to popular methods developed
# by Dr. Spock-were underrepresented.
# In fact, the Spock jury had no women.
#
# (1) Is there evidence that women are underrepresented on the Spock judge's
# venires compared to the venires of the other judges?
# 
# (2) Is there any evidence that there are differences
# in women's representation in the venires of the other six judges?

# Data Import and Data Check
data.52 <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store

# Check the data
View(data.52)
summary(data.52)
str(data.52)

# Dimensions
dim(data.52)[1] -> nr2
dim(data.52)[2] -> nc2
df1 <- nr2- length(unique(data.52$JUDGE)) #unique gives the number of different factors/labels

# Label-Partition
which(data.52$JUDGE=="A") -> A.label.indices
which(data.52$JUDGE=="B") -> B.label.indices
which(data.52$JUDGE=="C") -> C.label.indices
which(data.52$JUDGE=="D") -> D.label.indices
which(data.52$JUDGE=="E") -> E.label.indices
which(data.52$JUDGE=="SPOCK'S") -> SPOCKS.label.indices
#
# Data-Partition
data.52[A.label.indices,] -> A.data
data.52[B.label.indices,] -> B.data
data.52[C.label.indices,] -> C.data
data.52[D.label.indices,] -> D.data
data.52[E.label.indices,] -> E.data
data.52[SPOCKS.label.indices,] -> SPOCKS.data

# Summarize data
#install.packages("FSA") #please active it if the package is not installed before
#library(FSA)
Summarize(PERCENT~ JUDGE, data=data.52, digits=2)

# Data Visualitaion
stem(data.52[,1])  #stem-and-leaf plot

boxplot(PERCENT ~ JUDGE,
        data=data.52,
        xlab="JUDGE",
        ylab="Percentage of Women")

# ANOVA TEST (Analysis of Variance)
anova5.2 <- anova(lm(PERCENT ~ JUDGE, data=data.52))
anova5.2
summary(lm(PERCENT ~ JUDGE, data=data.52))

# ANOVA for the SPOCKS/NON-SPOCKS Data
which(data.52$JUDGE=="SPOCK'S") -> spocks.ind
which(data.52$JUDGE!="SPOCK'S") -> non.spocks.ind
data.52[non.spocks.ind,] -> others
data.52[spocks.ind,] -> spocks
others[,2]="Other"
pair.52 <- rbind(spocks, others)
summary(pair.52)
length(pair.52$PERCENT)[1] -> nr2.com
anova5.2e <- anova(lm(PERCENT ~ JUDGE, data=pair.52))
anova5.2e
summary(lm(PERCENT ~ JUDGE, data=pair.52))
(ss1e <- anova5.2$`Sum Sq`[2])
(df2 <- anova5.2e$Df[2])
(ss2e <- anova5.2e$`Sum Sq`[2])
(FStat <- ((ss2e-ss1e)/(df2-df1))/(ss1e/df1))
1-pf(FStat,5,df1)

# t-test
(difY.com <- abs(mean(others$PERCENT)-mean(spocks$PERCENT)))
(SE.difY.com <- sp*sqrt(1/length(others$PERCENT)+1/length(spock.judge$PERCENT)))
(conf.int.com <- c(difY.com-1.96*SE.difY.com,difY.com+1.96*SE.difY.com))
(t.com <- difY.com/SE.difY.com)
tt.com <- t.test(PERCENT ~ JUDGE, var.equal=TRUE, data=pair.52, alternative="two.sided")
tt.com$p.value

#Kruskal-Wallis Nonparametric Variance Test
kruskal.test(PERCENT ~ JUDGE, data=data.52)

