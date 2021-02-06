#Build a Neural Network model for 50_startups data to predict profit 


getwd()
setwd("D:/Data science/Data Science vedio/Supervised assignement forlder/Neutal network")
library(readr)
startups <- read.csv("Startups.csv")
View(startups)
table(startups$State)
summary(startups)
str(startups)

############################### Lets start Data exploring ################################

## standard deviation ##
sd(startups$R.D.Spend)
sd(startups$Administration)
sd(startups$Marketing.Spend)
sd(startups$Profit)

## variance ##
var(startups$R.D.Spend)
var(startups$Administration)
var(startups$Marketing.Spend)
var(startups$Profit)
#install.packages("moments")
library(moments)

################## 3rd and 4th "Business Moment" ########################################

skewness(startups$R.D.Spend)
skewness(startups$Administration)
skewness(startups$Marketing.Spend)
skewness(startups$Profit)

###kurtosis ##
kurtosis(startups$R.D.Spend)
kurtosis(startups$Administration)
kurtosis(startups$Marketing.Spend)
kurtosis(startups$Profit)
#####################################################################################

## histogram ##
hist(startups$R.D.Spend)
hist(startups$Profit)
hist(startups$Marketing.Spend)
hist(startups$Administration)

########boxplot #########
boxplot(startups$R.D.Spend)
boxplot(startups$Profit)
boxplot(startups$Marketing.Spend)
boxplot(startups$Administration)

## There is a rule for Neural Network data frame, i.e all data should be in numeric
## we are doing this by using the function name "as.numeric"

startups$State <- as.numeric(as.factor(startups$State))
View(startups)

## In Neural Network data shoul be normalized means in one scale. 
## we do this by creating a "custom norm function"

#Normalization of data by create a custom norm function

Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#testing of Norm function
Norm(c(1,5,-9,-859647))

startups_N <- as.data.frame(lapply(startups, Norm))
View(startups_N)
summary(startups_N)

#create data partition
library(readr)
library(caret)
Traininglocal <- createDataPartition(startups_N$Profit, p=0.70, list = F)
## "Test data" ##
Train <- startups_N[Traininglocal,]

## "Train data " ##
Test <- startups_N[-Traininglocal,]
#View(Train)
#View(Test)

### Model building by using package "neuralnet", "nnet"
library(neuralnet)
library(nnet)

Model_1 <- neuralnet(Profit~., data = Train)
plot(Model_1) ## step is 270
Pred <- compute(Model_1, Test[1:4])
Pred_profit <- Pred$net.result
Pred$neurons
cor(Pred_profit, Test$Profit) ## co-relation is good here is 97%

################################# "By using hidden neuron"##################
Model_2 <- neuralnet(Profit~.,data = startups_N, hidden = c(3,2))
plot(Model_2) ## step is 133
Pred_1 <- compute(Model_2,Test[1:4])
Pred1_profit <- Pred_1$net.result
Pred_1$neurons
cor(Pred1_profit,Test$Profit)
## co-relation is 98% which is greater the previous model or we can say that
## by using hidder normal steps is also less than "Model_1"
## "Model_2" is good as comparison to "Model_1". 

