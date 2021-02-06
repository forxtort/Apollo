#Hypothesis Testing Exercise

#Fantaloons Sales managers commented that % of males versus females walking in to 
#the store differ based on day of the week. Analyze the data and determine whether 
#there is evidence at 5 % significance level to support this hypothesis.

#Minitab File: Fantaloons.mtw

################################## Lets explore the  Data sets ###################
library(readr)
Faltoons <- read.csv(file.choose())
View(Faltoons)
str(Faltoons)
summary(Faltoons)
Faltoons$Weekdays<-as.numeric(as.factor(Faltoons$Weekdays))
Faltoons$Weekend<-as.numeric(as.factor(Faltoons$Weekend))
View(Faltoons)
############### Lets start the EDA process #################
#### Variance #######
var(Faltoons)
## standard Deviance ###
sd(Faltoons$Weekdays)
sd(Faltoons$Weekend)
#install.packages("moments")
library(moments)
## skewness ###
skewness(Faltoons)
## kurtosis ###
kurtosis(Faltoons)

#### Histogram ###
hist(Faltoons$Weekdays)
hist(Faltoons$Weekend)

### pairs ###
pairs(Faltoons)

### boxplot ###
boxplot(Faltoons)

#### barplot ###
barplot(Faltoons$Weekdays)
barplot(Faltoons$Weekend)
sum(is.na(Faltoons))
attach(Faltoons)

#############Normality test###############

shapiro.test(Faltoons$Weekdays) 
shapiro.test(Faltoons$Weekend)
# p-value = 2.2e-16 < 0.05 so p low null fly => It does not follow normal distribution

#############Variance test###############

var.test(Faltoons$Weekdays,Faltoons$Weekend)#variance test
# p-value = 0.06922 > 0.05 so p high null fly => Equal variances

#########X and Y are discrete|2 Prop Test#################


table(Weekdays)
table(Weekend)
table1 <- table(Weekdays,Weekend)
table1
prop.test(x=c(167,47),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions 
# p-value = 2.2e-16 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

prop.test(x=c(167,47),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "greater")
# p-value = 2.2e-16 < 0.05 accept alternate hypothesis 

################################# Thank you ############################################
