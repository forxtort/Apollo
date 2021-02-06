#Decision Tree

#Assignment


#About the data: 
#  Let's consider a Company dataset with around 10 variables and 400 records. 
#The attributes are as follows: 
#??? Sales -- Unit sales (in thousands) at each location
#??? Competitor Price -- Price charged by competitor at each location
#??? Income -- Community income level (in thousands of dollars)
#??? Advertising -- Local advertising budget for company at each location (in thousands of dollars)
#??? Population -- Population size in region (in thousands)
#??? Price -- Price company charges for car seats at each site
#??? Shelf Location at stores -- A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
#??? Age -- Average age of the local population
#??? Education -- Education level at each location
#??? Urban -- A factor with levels No and Yes to indicate whether the store is in an urban or rural location
#??? US -- A factor with levels No and Yes to indicate whether the store is in the US or not

#  Problem Statement:
#  A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A decision tree can be built with target variable Sale (we will first convert it in categorical variable) & all other variable will be independent in the analysis.  

library(readr)
Company <- read.csv(file.choose())
View(Company)
##########Packages required for building "Decision Tree"#################
#install.packages('C50')
#install.packages("tree")
#install.packages("ISLR")
library(ISLR)
library(tree)
#################Lets explore the data ########################

## standard deviation ###
sd(Company$CompPrice)
sd(Company$Income)
sd(Company$Advertising)
sd(Company$Population)
sd(Company$Price)
sd(Company$Age)

## variance ##

var(Company$CompPrice)
var(Company$Income)
var(Company$Advertising)
var(Company$Population)
var(Company$Price)
var(Company$Age)

######### 3rd and 4th Business Momemt##########
library(moments)
skewness(Company$CompPrice)
skewness(Company$Income)
skewness(Company$Advertising)
skewness(Company$Population)
skewness(Company$Price)
skewness(Company$Age)
###############################################

## Hitrogram ##

hist(Company$Income, main = "company Income", col = c("red","blue", "green"))
hist(Company$Sales, col=c("red","pink","red"))
hist(Company$Income, col=c("red","pink","yellow"))
hist(Company$Advertising, col=c("red","pink","orange"))
hist(Company$Population, col=c("red","yellow","violet"))
hist(Company$Price, col=c("black","pink","red"))
hist(Company$Age, col=c("red","yellow","black"))

## barplot##

barplot(Company$CompPrice)
barplot(Company$Income)
barplot(Company$Advertising)
barplot(Company$Population)
barplot(Company$Price)
barplot(Company$Age)

plot(Company)

plot(Company$Sales,Company$Advertising)

############### Lets start the Model building ###########
Highsales <- ifelse(Company$Sales<=8,"No", "Yes")
head(Highsales)
Company <- data.frame(Company,Highsales)
View(Company)
Company <- Company[-1]
View(Company)
Companytree <- tree(factor(Highsales) ~ ., data = Company, split=c("deviance", "gini"))

summary(Companytree) ##### In decision tree Nodes are very important
## This model build 27 Nodes. 
plot(Companytree)

text(Companytree, pretty = 1)

## Data Partion ####

library(caret)
Training <- createDataPartition(Company$Highsales, p=0.70, list=F)

## Train  ##
Train <- Company[Training,]
#View(Train)
dim(Train)
Test <- Company[-Training,]
#View(Test)
summary(Test)

########## Building a Model #######################
Model_train <- tree(factor(Highsales)~., data = Train)
dim(Model_train)
summary(Model_train)

plot(Model_train);text(Model_train)
pred <- predict(Model_train, newdata=Test[-12], type = "class")
summary(pred)
head(pred)
dim(pred)
head(pred)
mean(pred==Test$Highsales) ####### We get 78% accuracy which is not so bed #
library(gmodels)
CrossTable(pred,Test$Highsales) ## like  confusion matrix##


#begging method to increase the accuracy

acc <- NULL
for (i in 1:50) { print(i)
  
  Training <- createDataPartition(Company$Highsales, p=0.70, list=F)
  
  ## Train  ##
  Train <- Company[Training,]
  View(Train)
  dim(Train)
  Test <- Company[-Training,]
  
  Model_train <- tree(factor(Highsales)~., data = Train)
  pred <- predict(Model_train, newdata=Test[-12], type = "class")
  acc <- c(acc, mean(pred==Test$Highsales))
  
}
mean(acc)
library(gmodels)
CrossTable(pred,Test$Highsales)#### we increase our accuracy here 79.33% 
#which is far better than previous model

roc.curve(Test$Highsales, pred)
### Area under curve is high best the Model 


