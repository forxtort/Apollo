## A hospital wants to determine whether there is any difference in the average
## Turn Around Time (TAT) of reports of the laboratories on their preferred list.
##They collected a random sample and recorded TAT for reports of 4 laboratories. 
##TAT is defined as sample collected to report dispatch.

##Analyze the data and determine whether there is any difference in average TAT among 
##the different laboratories at 5% significance level.

## Lets start ##
LabTAT <- read.csv(file.choose())
View(LabTAT)

library(readr)
str(LabTAT)
summary(LabTAT)
var(LabTAT)
sd(LabTAT$Laboratory.1)
sd(LabTAT$Laboratory.2)
sd(LabTAT$Laboratory.3)
sd(LabTAT$Laboratory.4)
range(LabTAT)

#install.packages("moments")
library(moments)

skewness(LabTAT)
kurtosis(LabTAT)
hist(LabTAT$Laboratory.1)
hist(LabTAT$Laboratory.2)
hist(LabTAT$Laboratory.3)
hist(LabTAT$Laboratory.4)
pairs(LabTAT)
barplot(LabTAT$Laboratory.1)
barplot(LabTAT$Laboratory.2)
barplot(LabTAT$Laboratory.3)
barplot(LabTAT$Laboratory.4)
boxplot(LabTAT$Laboratory.1,LabTAT$Laboratory.2,LabTAT$Laboratory.3,LabTAT$Laboratory.4)
sum(is.na(LabTAT))
attach(LabTAT) 


## Normality test ### 

shapiro.test(Laboratory.1) 
## p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution
## We choose Null Hypothesis
shapiro.test(Laboratory.2)

# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution
## We choose Null Hypothesis

shapiro.test(Laboratory.3) 
## p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution
## we choose Null Hypothesis

shapiro.test(Laboratory.4)
## p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution
## ## we choose Null Hypothesis.

#############Variance test###############

var.test(Laboratory.1,Laboratory.2) #variance test
# p-value = 0.1675 > 0.05 so p high null fly => Equal variances

var.test(Laboratory.1,Laboratory.3)#variance test
# p-value = 0.01366 < 0.05 so p is low null fly => UnEqual variances

var.test(Laboratory.1,Laboratory.4)#variance test
# p-value = 0.1408 > 0.05 p high null fly => Equal variances

var.test(Laboratory.2,Laboratory.3)#variance test
# p-value = 0.2742 > 0.05 p high null fly => Equal variances

var.test(Laboratory.2,Laboratory.4)#variance test
# p-value = 0.9261 > 0.05 p high null fly => Equal variances

var.test(Laboratory.3,Laboratory.4)#variance test
# p-value = 0.3168 > 0.05 p high null fly => Equal variances

##Anova test ##

#install.packages("car")
library(car)
library(carData)

Stacklab <- stack(LabTAT)
View(Stacklab)
attach(Stacklab)
library(car)

##Levene test ## it is just like variance test

leveneTest(values,ind, data=Stacklab)

##Anova result ##
labtat_result <- aov(values~ind, data = Stacklab)
summary(labtat_result)
## P value is 2e-16, which is less than 0.05
## therefore all proportions are not equal
## we accept the Alternative Hypothesis (Ha). 

################################## Thank you ################################