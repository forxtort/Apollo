#Use Random Forest to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"


#################Lets Exploring the "Fraud data"##############################
library(readr)

Fraud <- read.csv(file.choose())
View(Fraud)
#install.packages("ISLR")
library(ISLR)
#install.packages("randomForest")
library(randomForest)

###################### EDA Process ##########################


### standard deviation ##

sd(Fraud$Taxable.Income)
sd(Fraud$City.Population)
sd(Fraud$Work.Experience)

#####  Variance ######
var(Fraud)

###### 3rd and 4th "Business Moment" ######
library(moments)

skewness(Fraud$Taxable.Income)
skewness(Fraud$City.Population)
skewness(Fraud$Work.Experience)


kurtosis(Fraud$Taxable.Income)
kurtosis(Fraud$City.Population)
kurtosis(Fraud$Work.Experience)

######## histogram ######


hist(Fraud$Taxable.Income)
hist(Fraud$City.Population)
hist(Fraud$Work.Experience)

###### Bar plot ##############

barplot(Fraud$Taxable.Income)
barplot(Fraud$City.Population)
barplot(Fraud$Work.Experience)

##########################################################################################

FaadMagic <- ifelse(Fraud$Taxable.Income<=30000,"Risky","Good")
head(FaadMagic)

Fraud <- data.frame(Fraud,FaadMagic)
View(Fraud)
Fraud <- Fraud[-3]
View(Fraud)


################################ Model Building start ########################

FraudrandomForest <- randomForest(factor(FaadMagic)~. , data = Fraud, split=c("deviance", "gini"))
is.na(FraudrandomForest)
sum(is.na(FraudrandomForest))
str(FraudrandomForest)
summary(FraudrandomForest)
## Data partion by using 'caret' package ####

library(caret)
Training <- createDataPartition(Fraud$FaadMagic, p=0.50, list=F)

Train <- Fraud[Training,]
table(Train$FaadMagic)

Test <- Fraud[-Training,]
table(Test$FaadMagic)

Model_train <- randomForest(factor(FaadMagic)~., data = Train)
dim(Model_train)
summary(Model_train)
prop.table(table(Fraud$FaadMagic))

pred <- predict(Model_train, newdata=Test[-7], type = "class")
summary(pred)


dim(pred)

mean(pred==Test$FaadMagic)
library(gmodels)
CrossTable(pred,Test$FaadMagic) ## accuracy is 76%

#to Increase the accuracy of the model we use begging method

#begging method to increase the accuracy

acc <- NULL
for (i in 1:50) { print(i)
  Training <- createDataPartition(Fraud$FaadMagic, p=0.75, list=F)
  Train <- Fraud[Training,]
  Test <- Fraud[-Training,]
  
  
  Model_train <- randomForest(factor(FaadMagic)~., data = Train)
  
  pred <- predict(Model_train, newdata=Test[-7], type = "class")
  
  
  acc <- c(acc, mean(pred==Test$FaadMagic))
}
mean(acc)
library(gmodels)
CrossTable(pred,Test$FaadMagic)#### we increase our accuracy here 78%
#which is far better than previous model



