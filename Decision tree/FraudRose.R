#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

#Data Description :

#Undergrad : person is under graduated or not
#Marital.Status : marital status of a person
#Taxable.Income : Taxable income is the amount of how much tax an individual owes to the government 
#Work Experience : Work experience of an individual person
#Urban : Whether that person belongs to urban area or not


Fraud <- read.csv(file.choose())
View(Fraud)
summary(Fraud)
str(Fraud)
## packages required for "Decision tree" ## 
#install.packages("ISLR")
library(ISLR)
#install.packages("tree")
library(tree)

####################### Lets begin data exploring ########################

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
#################### Lets Begin the "Model Building "############################

library(tree)
# There is package "ROSE"  i used for Model building, that's why I am taking only love object
Love <- ifelse(Fraud$Taxable.Income<=30000,"Risky","Good")
head(Love)
Fraud <- data.frame(Fraud,Love)
View(Fraud)
Fraud <- Fraud[-3]
View(Fraud)
#create data partion
library(caret)
Training <- createDataPartition(Fraud$Love, p=0.75, list=F)
Train <- Fraud[Training,]
table(Train$Love)
Test <- Fraud[-Training,]
View(Test)
################################## Model Building ###################################
library(tree)
Model_1 <- tree(Love~., data= Train)
pred_model <- predict(Model_1, newdata = Test, type = "class")
mean(pred_model==Test$Love)

library(gmodels)
CrossTable(pred_model, Test$Love)
#here we find the our data is overfit(unbalanced)
#to balance the data we use "Rose", package
library(ROSE)
data.rose <- ROSE(Love ~ ., data = Train, seed = 1)$data
table(data.rose$Love)
prop.table(table(data.rose$Love))

library(rpart)
Rose_model <- rpart(Love~., data=data.rose)

pred_models <- predict(Rose_model, newdata = Test, type = "class")

mean(pred_models==Test$Love)

library(gmodels)
CrossTable(pred_models,Test$Love) #### Accuracy is 57.33%

#begging method to increase the accuracy

acc <- NULL
for (i in 1:50) { print(i)
  Training <- createDataPartition(Fraud$Love, p=0.75, list=F)
  Train <- Fraud[Training,]
  Test <- Fraud[-Training,]
  
  data.rose <- ROSE(Love ~ ., data = Train, seed = 1)$data
  Rose_model <- rpart(Love~., data=data.rose)
  pred_models <- predict(Rose_model, newdata = Test, type = "class")
  acc <- c(acc, mean(pred_model==Test$Love))
}
mean(acc)
library(gmodels)
CrossTable(pred_models,Test$Love)#### we increase our accuracy here 79.33% 
#which is far better than previous model

roc.curve(Test$Love, pred_models)
### Area under curve is high best the Model 

