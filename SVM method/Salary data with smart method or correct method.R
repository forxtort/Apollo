getwd()
setwd("D://Data science//Data Science vedio//Supervised assignement forlder//SVM")
library(readr)

#Prepare a classification model using SVM for salary data 

#Data Description:

#age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

########################## Lets Explore the data set #######################
## For SVM data should be in numeric and normailized 
## we are going to numeric those column in which factor data is present####

Train <- read.csv("SalaryData_Train.csv")
View(Train)
str(Train)
Train$workclass <- as.numeric(as.factor(Train$workclass))
Train$education <- as.numeric(as.factor(Train$education))
Train$maritalstatus <- as.numeric(as.factor(Train$maritalstatus))
Train$occupation<- as.numeric(as.factor(Train$occupation))
Train$relationship <- as.numeric(as.factor(Train$relationship))
Train$race <- as.numeric(as.factor(Train$race))
Train$native <- as.numeric(as.factor(Train$native))
Train$sex <- as.numeric(as.factor(Train$sex))

########## Variance of Train data ######

sd(Train$age)
sd(Train$workclass)
sd(Train$education)
sd(Train$educationno)
sd(Train$maritalstatus)
sd(Train$occupation)
sd(Train$relationship)
sd(Train$race)
sd(Train$sex)
sd(Train$capitalgain)
sd(Train$capitalloss)
sd(Train$hoursperweek)
sd(Train$native)

## variance of Train data ##
var(Train)

### 3rd and 4th Business data ##
library(moments)
skewness(Train[,-14])
kurtosis(Train[,-14])

### Histogram #####

hist(Train$age)
hist(Train$workclass)
hist(Train$education)
hist(Train$educationno)
hist(Train$maritalstatus)
hist(Train$occupation)
hist(Train$relationship)
hist(Train$race)
hist(Train$sex)
hist(Train$capitalgain)
hist(Train$capitalloss)
hist(Train$hoursperweek)
hist(Train$native)

 
## box plot ###

barplot(Train$age)
barplot(Train$workclass)
barplot(Train$education)
barplot(Train$educationno)
barplot(Train$maritalstatus)
barplot(Train$occupation)
barplot(Train$relationship)
barplot(Train$race)
barplot(Train$sex)
barplot(Train$capitalgain)
barplot(Train$capitalloss)
barplot(Train$hoursperweek)
barplot(Train$native)
boxplot(Train[,-14])
sum(is.na(Train))
str(Train)

#################### Model building process start ####################
#################### Normalizing the data by custom method ###########
Norm <- function(x)
{return((x-min(x))/(max(x)-min(x)))}
library(kernlab)

Train_n <- as.data.frame(lapply(Train[c(1:13)], Norm))
View(Train_n)
summary(Train_n)
Train <- data.frame(Train_n[1:13],Train[14])
View(Train)
summary(Train)
######################### Model building #############################
######################### Using Method "Vanilladot" ##################
library(kernlab)
Model_1 <- ksvm(Salary~.,data=Train, kernel='vanilladot')

######################## uploading test data#########################

getwd()
setwd("D:/Data science/Data Science vedio/Supervised assignement forlder/SVM")

library(readr)

Test <- read.csv("SalaryData_Test.csv")

View(Test)
str(Test)
Test$workclass <- as.numeric(as.factor(Test$workclass))
Test$education <- as.numeric(as.factor(Test$education))
Test$maritalstatus <- as.numeric(as.factor(Test$maritalstatus))
Test$occupation<- as.numeric(as.factor(Test$occupation))
Test$relationship <- as.numeric(as.factor(Test$relationship))
Test$race <- as.numeric(as.factor(Test$race))
Test$native <- as.numeric(as.factor(Test$native))
Test$sex <- as.numeric(as.factor(Test$sex))
View(Test)
##### Here we are not doing the skewness and kurtosis ###
#### also barplot, boxplot, other EDA parts ##
#### we already done above the same thing, the difference here, the main data is 
#### already split in test and train, we generally do "EDA to the Main data frame. ".

### Custom Norm function that we created above just we apply here ### 
### for Normalizing the data ####

Test_n <- as.data.frame(lapply(Test[c(1:13)], Norm))
View(Test_n)
## adding the data, in first data there are only 13 columns are normalized and we left
## the output variabe, so we just take from main test data here ##

Test <- data.frame(Test_n[1:13],Test[14])
View(Test)
summary(Test)
Pred <- predict(Model_1,  newdata=Test[-14])
mean(Pred==Test$Salary)
library(gmodels)
CrossTable(Pred,Test$Salary)
## Accuary of model by using 
## different kernel method using "vanilladot" method is 80.8% ##


############################# "Using a "besseldot method" ###############################

Model_2 <- ksvm(Salary~.,data=Train, kernel='besseldot')
Pred1 <- predict(Model_2,newdata=Test[-14])
mean(Pred1==Test$Salary)
CrossTable(Pred,Test$Salary)


## Accuracy is 68.9 % , so we can say that Vanilladot method is best ##
## we are using different method here because of achieving the higher accuracy.  

## last thing is that we have also another method we can used for "SVM"
## rbfodt, polydot, tanhdot, vanilladot, anovadot, besseldot, laplacedot, etc.
## just type kenel-class help in R where you get all type of method. 

################################ Thank you ###########################################
