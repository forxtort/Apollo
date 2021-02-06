## Cutlet data ##
## F & B , these two manager wants to kow that a siginificant change in
## in two units in A and B. 

## Lets Begin it ## 
cutlet <- read.csv(file.choose())
View(cutlet)
summary(cutlet)
str(cutlet)
sd(cutlet$Unit.A)
sd(cutlet$Unit.B)
range(cutlet)
#install.packages("moments")
library(moments)
skewness(cutlet)
kurtosis(cutlet)
hist(cutlet$Unit.A)
hist(cutlet$Unit.B)
plot(cutlet$Unit.A,cutlet$Unit.B)
barplot(cutlet$Unit.A)
barplot(cutlet$Unit.B)
boxplot(cutlet$Unit.A,cutlet$Unit.B)
sum(is.na(cutlet))
attach(cutlet)
colnames(cutlet)
str(cutlet)
### Normality test ###

shapiro.test(Unit.A)

## p value is 0.32 which is greater than 0.05, null hypothesis will accpet
## in simple language whenever we have p>0.05 we will not take any action (ho) or
## we will called it as null hypothesis and 
## whenever we have P<0.05, we will take action means(ha),         
## ho= No action required= Null hypothesis
## ha= Action is required= alternative hypothesis

shapiro.test(Unit.B)

## p-value is 0.5225 which is again greater than 0.05, same above things will follow here
## that mention in shapiro. test (Unit.B)

## Conclusion of "shapiro. test"  ##
## whenever we got Null hypthothesis and p-value is greater than 0.05
## it follow "Normal distribution "

## 2nd step is Variance test ##

var.test(Unit.A,Unit.B)

## p-value is 0.3136 which is greater than 0.05
## both have equal variance
## ration of variance 0.70
## again we don't take any action and ho
## i.e is null hypothesis will accept

## 2 Sample test t test ##
t.test(Unit.A, Unit.B, alternative = "two.sided", conf.level = 0.95, correct=TRUE)
## p-value is 0.47 which is greater than 0.05
## alternative = "two.sided" means we are checking for equal and unequal mean
## Equal means 
## Null Hypothesis > equal means or ho. 
## Alternative Hpothesis < unequal means or ha.
## Accept Null Hypothesis 

t.test(Unit.A,Unit.B, alternative = 'greater', var.equal = T)

## alternative = greater true mean difference is greater than o
## p-value is 0.2351 therfore
## Accept Null Hypothesis 
## Unit B is better than Unit A
