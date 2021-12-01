# Statistical Sleuth Case 6.1 
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Handicapped-A Randomized Experiment
# The researchers prepared five videotaped job interviews, using the same two male actors for each.
# A set script was designed to reflect an interview with an applicant of average qualifications.
# The tapes differed only in that the applicant appeared with a different handicap.
# In one, he appeared in a wheelchair; (1)
# in a second, he appeared on crutches; (2)
# in another, his hearing was impaired; (3)
# in a fourth, he appeared to have one leg amputated; (4)
# and in the final tape, he appeared to have no handicap (5).
#
# 70 undergraduate students from a U.S. university were randomly assigned to view the tapes,
# 14 to each tape.
# After viewing the tape, each subject rated the qualifications of the applicant
# on a 0- to 10-point applicant qualification scale.
#
# The question is, do subjects systematically evaluate qualifications differently according to the candidate's handicap?
# If so, which handicaps produce the different evaluations?

# Data Import and Data Check
data.61 <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store
rm(list=setdiff(ls(), c("data.61","data.62"))) #to clean global environment by keeping initial data
                        
# Check the data
View(data.61)
summary(data.61)
str(data.61)

# Dimensions
dim(data.61)[1] -> nr
dim(data.61)[2] -> nc
df <- nr-length(unique(data.61$HANDICAP)) #unique gives the number of different factors/labels

# Label-Partition
which(data.61$HANDICAP=="NONE") -> label.indices.11
which(data.61$HANDICAP=="AMPUTEE") -> label.indices.12
which(data.61$HANDICAP=="CRUTCHES") -> label.indices.13
which(data.61$HANDICAP=="HEARING") -> label.indices.14
which(data.61$HANDICAP=="WHEELCHAIR") -> label.indices.15
#
# Data-Partition
data.61[label.indices.11,] -> data.11
data.61[label.indices.12,] -> data.12
data.61[label.indices.13,] -> data.13
data.61[label.indices.14,] -> data.14
data.61[label.indices.15,] -> data.15

# Summarize data
#install.packages("FSA") #please active it if the package is not installed before
#library(FSA)
Summarize(SCORE ~ HANDICAP, data=data.61, digits=2)

# Data Visualitaion
stem(data.61[,1])  #stem-and-leaf plot

boxplot(SCORE ~ HANDICAP,
        data=data.61,
        xlab="Applicant Status",
        ylab="Score")

# Pooled-Standart Deviation
sd(data.11[,1]) -> sd.11
sd(data.12[,1]) -> sd.12
sd(data.13[,1]) -> sd.13
sd(data.14[,1]) -> sd.14
sd(data.15[,1]) -> sd.15
length(data.11[,1]) -> n.11
length(data.12[,1]) -> n.12
length(data.13[,1]) -> n.13
length(data.14[,1]) -> n.14
length(data.15[,1]) -> n.15

spooled = sqrt(( (n.11-1)*sd.11^2+
                   (n.12-1)*sd.12^2+
                   (n.13-1)*sd.13^2+
                   (n.14-1)*sd.14^2+
                   (n.15-1)*sd.15^2 )
               /(n.11+n.12+n.13+n.14+n.15-5))
spooled -> sp2
(sp <- sqrt(sp2))

# Mean Differences
list(NONE=mean(data.11$SCORE)-mean(data.61$SCORE),
     AMPUTEE=mean(data.12$SCORE)-mean(data.61$SCORE),
     CRUTCHES=mean(data.13$SCORE)-mean(data.61$SCORE),
     HEARING=mean(data.14$SCORE)-mean(data.61$SCORE),
     WHEELCHAIR=mean(data.15$SCORE)-mean(data.61$SCORE))


# ANOVA TEST (Analysis of Variance)
anova6.1 <- anova(lm(SCORE ~ HANDICAP, data=data.61)) 
anova6.1
summary(lm(SCORE ~ HANDICAP, data=data.61))
#The evidence is strong but not-convincing! (p=0.03)
#Note that the group "AMPUTEE" is the reference group


# Group Mean Differences - Tukey Kramer Test
TukeyHSD(aov(lm(SCORE ~ HANDICAP, data=data.61)))
# The difference between the average qualification scores given to the crutches candidate and
# to the hearing-impaired candidate is not likely to be by chance.

# Handicap Study
Feigned.Handicap = list(cbind(rbind(n=14,Average=mean(data.11$SCORE),C=0),
                        rbind(n=14,Average=mean(data.12$SCORE),C=-0.5),
                        rbind(n=14,Average=mean(data.13$SCORE),C=0.5),
                        rbind(n=14,Average=mean(data.14$SCORE),C=-0.5),
                        rbind(n=14,Average=mean(data.15$SCORE),C=0.5)))
colnames(Feigned.Handicap[[1]]) <- c("None","Amputee","Crutches","Hearing","WheelChair")
(Feigned.Handicap -> FH)

(g = ((FH[[1]][2,3] + FH[[1]][2,5]) - (FH[[1]][2,2] + FH[[1]][2,4]))/2) #estimate the linear com.
(SE.g = sp2*sqrt((FH[[1]][3,1]^2)/length(data.11$SCORE)+
                  (FH[[1]][3,2]^2)/length(data.12$SCORE)+
                  (FH[[1]][3,3]^2)/length(data.13$SCORE)+
                  (FH[[1]][3,4]^2)/length(data.14$SCORE)+
                  (FH[[1]][3,5]^2)/length(data.15$SCORE)))
#find the st. error of the estimate
conf.int.61 <-  c(g-qt(0.975,65)*SE.g,g+qt(0.975,65)*SE.g)  #95% confidence interval
conf.int.61


# Multiple Comparison Procedures
# ???Interval half-width??? = (Multiplier)*(Standard error)
I=5
df
(F61 <- qf(.95,I-1,df)) #F-value
alpha=0.05

#Tukey-Kramer Procedure
qtukey.61 <-  qtukey(p=0.95,nmeans=I,df)
(tkp <- qtukey.61/sqrt(2))


# Scheffe Test
(sch <- sqrt((I-1)*F61*(1-alpha)))

#The LSD
(lsd <- qt(1-alpha/2,df))

#In the handicap study, the p-value from the ANOVA test was .03,
#so the F-protected comparison plan would be the way of using
#t-test or confidence intervals with the t-multiplier

#Bonferroni
k=I*(I-1)/2
(bon <- qt(1-alpha/(2*k),df))

#Standard Error
SE <- sp2*sqrt(2/n.11)

#Multiple Comparison Measures According to the Procedures
list("LSD"=lsd*SE,
     "Tukey-Kramer"=tkp*SE,
     "Bonferroni"=bon*SE,
     "Scheffe"=sch*SE)

###################################################
###################################################

# Statistical Sleuth Case 6.2
# Analyzed by Ugur Uresin - Github: ugururesin

# CASE
# Six pairs of males were surgically given artificial, plastic swordtails.
# One male of each pair received a bright yellow sword, while the other received a transparent sword.
# The males in a pair were placed in closed compartments at opposite ends of a fish tank.
# One at a time, females were placed in a central compartment, where they could choose to engage
# in courtship activity with either of the males by entering a side compartment adjacent to it.
# Of the total time spent by each female engaged in courtship during a 20-minute observation period,
# the percentages of time spent with the yellow-sword male were recorded.
#
# Investigate did these females show a preference for the males that were given yellow swordtails?

# Data Import and Data Check
data.62 <- read.csv(file.choose(), sep=";", header = T) #please import the data from where you store

# ATTENTION: The data for 2nd case of chapter 6 is given with proportion (percentage) values be rounded-up!!!
# HENCE: The proportion was multiplied by 100 so as to align the number with the chapter!
data.62$PROPORTION <- data.62$PROPORTION*100 

# Check the data
View(data.62)
summary(data.62)
str(data.62)

# Dimensions
dim(data.62)[1] -> nr2
dim(data.62)[2] -> nc2
df1 <- nr2- length(unique(data.62$PAIR)) #unique gives the number of different factors/labels

# Label-Partition
which(data.62$PAIR=="PAIR 1") -> label.indices.21
which(data.62$PAIR=="PAIR 2") -> label.indices.22
which(data.62$PAIR=="PAIR 3") -> label.indices.23
which(data.62$PAIR=="PAIR 4") -> label.indices.24
which(data.62$PAIR=="PAIR 5") -> label.indices.25
which(data.62$PAIR=="PAIR 6") -> label.indices.26
#
# Data-Partition
data.62[label.indices.21,] -> data.21
data.62[label.indices.22,] -> data.22
data.62[label.indices.23,] -> data.23
data.62[label.indices.24,] -> data.24
data.62[label.indices.25,] -> data.25
data.62[label.indices.26,] -> data.26

# Summarize data
#install.packages("FSA") #please active it if the package is not installed before
#library(FSA)
Summarize(PROPORTION ~ PAIR, data=data.62, digits=2)

# Data Visualitaion
stem(data.62[,1])  #stem-and-leaf plot for the proportions 
stem(data.62[,3])  #stem-and-leaf plot for the lengths

boxplot(PROPORTION ~ PAIR,
        data=data.62,
        xlab="Pairs",
        ylab="Proportions")

# Pooled-Standart Deviation
sd(data.21[,1]) -> sd.21
sd(data.22[,1]) -> sd.22
sd(data.23[,1]) -> sd.23
sd(data.24[,1]) -> sd.24
sd(data.25[,1]) -> sd.25
sd(data.26[,1]) -> sd.26
length(data.21[,1]) -> n.21
length(data.22[,1]) -> n.22
length(data.23[,1]) -> n.23
length(data.24[,1]) -> n.24
length(data.25[,1]) -> n.25
length(data.26[,1]) -> n.26

spooled2 = sqrt(( (n.21-1)*sd.21^2+
                    (n.22-1)*sd.22^2+
                    (n.23-1)*sd.23^2+
                    (n.24-1)*sd.24^2+
                    (n.25-1)*sd.25^2+
                    (n.26-1)*sd.26^2)
                /(n.21+n.22+n.23+n.24+n.25+n.26-6))
spooled2 -> sp22
sp2 <- sqrt(sp22)


# Mean Differences
list(PAIR1=mean(data.21$PROPORTION)-mean(data.62$PROPORTION),
     PAIR2=mean(data.22$PROPORTION)-mean(data.62$PROPORTION),
     PAIR3=mean(data.23$PROPORTION)-mean(data.62$PROPORTION),
     PAIR4=mean(data.24$PROPORTION)-mean(data.62$PROPORTION),
     PAIR5=mean(data.25$PROPORTION)-mean(data.62$PROPORTION),
     PAIR6=mean(data.26$PROPORTION)-mean(data.62$PROPORTION))

# ANOVA TEST (Analysis of Variance)
anova6.2_pair <- anova(lm(PROPORTION ~ PAIR, data=data.62))
anova6.2_pair
summary(lm(PROPORTION ~ PAIR, data=data.62))
# No evidence for tail-type (p=0.56)

anova6.2_length <- anova(lm(PROPORTION ~ LENGTH, data=data.62))
anova6.2_length
summary(lm(PROPORTION ~ LENGTH, data=data.62))
# No evidence for tail-type (p=0.59)

# Specific Linear Combinations
Linear.Effect = list(cbind(
  rbind("Pair 1","Pair 2","Pair 3","Pair 4","Pair 5","Pair 6"),
  rbind(length(data.21$PAIR),length(data.22$PAIR),length(data.23$PAIR),length(data.24$PAIR),length(data.25$PAIR),length(data.26$PAIR)),
  rbind(mean(data.21$PROPORTION),mean(data.22$PROPORTION),mean(data.23$PROPORTION),mean(data.24$PROPORTION),mean(data.25$PROPORTION),mean(data.26$PROPORTION)),
  rbind(sd(data.21$PROPORTION),sd(data.22$PROPORTION),sd(data.23$PROPORTION),sd(data.24$PROPORTION),sd(data.25$PROPORTION),sd(data.26$PROPORTION)),
  rbind(data.21[1,3],data.22[1,3],data.23[1,3],data.24[1,3],data.25[1,3],data.26[1,3]),
  rbind(5,-3,1,3,-9,3)))

colnames(Linear.Effect[[1]]) <- c("Group","n","Average(%)","St.Dev.","Male Body Size (mm)","Coefficient")
(Linear.Effect -> LE)

pooled.n <- sum(c(length(data.21$PAIR),length(data.22$PAIR),length(data.23$PAIR),length(data.24$PAIR),length(data.25$PAIR),length(data.26$PAIR)))
pooled.mean <- mean(c(data.21$PROPORTION,data.22$PROPORTION,data.23$PROPORTION,data.24$PROPORTION,data.25$PROPORTION,data.26$PROPORTION))
pooled.sd <- sd(c(data.21$PROPORTION,data.22$PROPORTION,data.23$PROPORTION,data.24$PROPORTION,data.25$PROPORTION,data.26$PROPORTION))
pooled.size <- mean(c(data.21$LENGTH,data.22$LENGTH,data.23$LENGTH,data.24$LENGTH,data.25$LENGTH,data.26$LENGTH))


(g2 = as.numeric(LE[[1]][1,3])*as.numeric(LE[[1]][1,6])+
  as.numeric(LE[[1]][2,3])*as.numeric(LE[[1]][2,6])+
  as.numeric(LE[[1]][3,3])*as.numeric(LE[[1]][3,6])+
  as.numeric(LE[[1]][4,3])*as.numeric(LE[[1]][4,6])+
  as.numeric(LE[[1]][5,3])*as.numeric(LE[[1]][5,6])+
  as.numeric(LE[[1]][6,3])*as.numeric(LE[[1]][6,6]))

(SE.g2 = sp22*sqrt(((as.numeric(LE[[1]][1,6])^2))/as.numeric(length(data.21$PROPORTION))+
                     ((as.numeric(LE[[1]][2,6])^2))/as.numeric(length(data.22$PROPORTION))+
                     ((as.numeric(LE[[1]][3,6])^2))/as.numeric(length(data.23$PROPORTION))+
                     ((as.numeric(LE[[1]][4,6])^2))/as.numeric(length(data.24$PROPORTION))+
                     ((as.numeric(LE[[1]][5,6])^2))/as.numeric(length(data.25$PROPORTION))+
                     ((as.numeric(LE[[1]][6,6])^2))/as.numeric(length(data.24$PROPORTION)))) 
(df2 <- pooled.n-6)
(t_stat2 <- g2/SE.g2)
(p.value2 = dt(t_stat2, df2))

#Confidence Interval
mean_6.2 <- mean(data.62$PROPORTION) 
confidence_interval_6.2 <-  c(mean_6.2-(t_stat2*(SE.g2/sqrt(pooled.size))),mean_6.2+(t_stat2*(SE.g2/sqrt(pooled.size))))
confidence_interval_6.2
