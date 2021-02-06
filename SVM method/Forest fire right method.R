#Classify the Size_Categorie using SVM

#month	month of the year: 'jan' to 'dec'
#day	day of the week: 'mon' to 'sun'
#FFMC	FFMC index from the FWI system: 18.7 to 96.20
#DMC	DMC index from the FWI system: 1.1 to 291.3
#DC	DC index from the FWI system: 7.9 to 860.6
#ISI	ISI index from the FWI system: 0.0 to 56.10
#temp	temperature in Celsius degrees: 2.2 to 33.30
#RH	relative humidity in %: 15.0 to 100
#wind	wind speed in km/h: 0.40 to 9.40
#rain	outside rain in mm/m2 : 0.0 to 6.4
#Size_Categorie 	the burned area of the forest ( Small , Large)

getwd()
setwd("D:/Data science/Data Science vedio/Supervised assignement forlder/SVM")
library(readr)
Forest <- read.csv("forestfires.csv")
View(Forest)
str(Forest)
################### Building a Model of Simple Vector Machine ####################

## data should be in numeric and  data should be normailized.
## that's why we are using "as.numeric function" her for those column in 
## which data are not in numeric.

##########################Let explore the data ##################################

######### Data conversion from factor to Numeric ################################

Forest$month <- as.numeric(as.factor(Forest$month))
Forest$day <- as.numeric(as.factor(Forest$day))
str(Forest)

View(Forest)


### standard deviation of forest fire data #######################################
sd(Forest$month)
sd(Forest$day)
sd(Forest$FFMC)
sd(Forest$DMC)
sd(Forest$DC)
sd(Forest$ISI)
sd(Forest$temp)
sd(Forest$RH)
sd(Forest$wind)
sd(Forest$rain)
sd(Forest$area)

##install.packages("moments")
library(moments)

## 3rd and 4th Business moment ####

skewness(Forest$month)
skewness(Forest$day)
skewness(Forest$FFMC)
skewness(Forest$DMC)
skewness(Forest$DC)
skewness(Forest$ISI)
skewness(Forest$temp)
skewness(Forest$RH)
skewness(Forest$wind)
skewness(Forest$rain)
skewness(Forest$area)

kurtosis(Forest$month)
kurtosis(Forest$day)
kurtosis(Forest$FFMC)
kurtosis(Forest$DMC)
kurtosis(Forest$DC)
kurtosis(Forest$ISI)
kurtosis(Forest$temp)
kurtosis(Forest$RH)
kurtosis(Forest$wind)
kurtosis(Forest$rain)
kurtosis(Forest$area)

######## Histogram #####
hist(Forest$month)
hist(Forest$day)
hist(Forest$FFMC)
hist(Forest$DMC)
hist(Forest$DC)
hist(Forest$ISI)
hist(Forest$temp)
hist(Forest$RH)
hist(Forest$wind)
hist(Forest$rain)
hist(Forest$area)

###########Barplot $$$$$$$$$$$$
barplot(Forest$month)
barplot(Forest$day)
barplot(Forest$FFMC)
barplot(Forest$DMC)
barplot(Forest$DC)
barplot(Forest$ISI)
barplot(Forest$temp)
barplot(Forest$RH)
barplot(Forest$wind)
barplot(Forest$rain)
barplot(Forest$area)
boxplot(Forest)
sum(is.na(Forest))


summary(Forest)
library(kernlab)
library(caret)
names(Forest)[29] <- 'unknownname'
names(Forest)[31] <- 'Size'
View(Forest)

### Data partion of the data ## 

Traininglocal <- createDataPartition(Forest$Size, p=.70, list = F)
## Tain data ##

Train <- Forest[Traininglocal,]
prop.table(table(Train$Size))
table(Train$Size)

## Test data ##

Test <- Forest[-Traininglocal,]
table(Test$Size)
prop.table(table(Test$Size))
View(Train)
View(Test)

## Build a model Size_Categorie 	the burned area of the forest ( Small , Large)
## one thing keep in mind area completely shows the size column. 
## so we will delete the area column

Forest <- Forest[-11]
View(Forest)

##############################Buiding a model####################################

############################# "Using a kernal method" ###############################

Model_1 <- ksvm(Size~., data=Train, kernel="rbfdot")
pred <- predict(Model_1, newdata=Test)                 
mean(pred==Test$Size)
library(gmodels)
CrossTable(pred,Test$Size)

## when using kernel method we get accuracy for model  is 75.32%%

## lets try to increace the accuracy of the model by using different
## type of method #############

############################# "Using a "polydot method" ###############################

Model_1 <- ksvm(Size~., data=Train, kernel="polydot")
pred <- predict(Model_1, newdata=Test)                 
mean(pred==Test$Size)
library(gmodels)
CrossTable(pred,Test$Size)
## accuracy we accieve from polydot method is 87.6% 

## Let try another method ######

############################# "Using a "splinedot method" ###############################

Model_1 <- ksvm(Size~., data=Train, kernel="splinedot")
pred <- predict(Model_1, newdata=Test)                 
mean(pred==Test$Size)
library(gmodels)
CrossTable(pred,Test$Size)
## Accuracy we achieve from this methond is 65.5% 

## so from above three model we can say that the polydot method is best
## for this model, because of the best accuracy we achieved from this. 

## las thing is that we have also another method we can used for "SVM"
## rbfodt, polydot, tanhdot, vanilladot, anovadot, besseldot, laplacedot, etc.
## just type kenel-class help in R where you get all type of method. 

################################ Thank you ###########################################