#Problem Statement : Prepare a model for strength of concrete data using Neural Networks

library(readr)
Concrete <- read.csv(file.choose())
View(Concrete)
sum(is.na.data.frame(Concrete))
table(Concrete$age)
summary(Concrete)
str(Concrete)
################################ Let explore the Model ########################

## standard deviation ##
sd(Concrete$cement)
sd(Concrete$slag)
sd(Concrete$ash)
sd(Concrete$water)
sd(Concrete$superplastic)
sd(Concrete$coarseagg)
sd(Concrete$fineagg)
sd(Concrete$age)
sd(Concrete$strength)

## variance ##
var(Concrete$cement)
var(Concrete$slag)
var(Concrete$ash)
var(Concrete$water)
var(Concrete$superplastic)
var(Concrete$coarseagg)
var(Concrete$fineagg)
var(Concrete$age)
var(Concrete$strength)

########### 3rd and 4th Moment #################
#install.packages("moments")
library(moments)

##skewness ##
skewness(Concrete)

## Kurtosis ##
kurtosis(Concrete)

pairs(Concrete)

### Histogram ##

hist(Concrete$cement)
hist(Concrete$slag)
hist(Concrete$ash)
hist(Concrete$water)
hist(Concrete$superplastic)
hist(Concrete$coarseagg)
hist(Concrete$fineagg)
hist(Concrete$age)
hist(Concrete$strength)

## boxplot ###
boxplot(Concrete$cement)
boxplot(Concrete$slag)
boxplot(Concrete$ash)
boxplot(Concrete$water)
boxplot(Concrete$superplastic)
boxplot(Concrete$coarseagg)
boxplot(Concrete$fineagg)
boxplot(Concrete$age)
boxplot(Concrete$strength)

######### Normalizing the data in one sclae ###########

#Normalization of data by create a custom norm function
Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#Testing of Norm function
Norm(c(1,5,-9,-859647))

Concrete_N <- as.data.frame(lapply(Concrete, Norm))
View(Concrete_N)
summary(Concrete_N)
table(Concrete_N$age)

#############################Create data partition ####################################
library(readr)
library(caret)
Traininglocal <- createDataPartition(Concrete_N$strength, p=0.70, list = F)

################## Train data ############

Train <- Concrete_N[Traininglocal,]

################## Test data ############

Test <- Concrete_N[-Traininglocal,]

##View(Train)
##View(Test)

################################# Model -1###############################
##############Model building by using package "neuralnet", "nnet" #######
library(neuralnet)
library(nnet)
Model_1 <- neuralnet(strength~., data = Train)
plot(Model_1) ## steps 5101, error 4.658
Pred <- compute(Model_1, Test[1:8])
Pred_strength <- Pred$net.result
Pred$neurons
cor(Pred_strength, Test$strength) ## Co-relation is 81%

#######################By using hidden neuron #############################
Model_2 <- neuralnet(strength~.,data = Concrete_N, hidden = c(3,2))
plot(Model_2)## error 2.54, steps 16125
Pred_1 <- compute(Model_2,Test[1:8])
Pred1_strength <- Pred_1$net.result
Pred_1$neurons
cor(Pred1_strength,Test$strength) ## Co-relation is 94%

## From above two models we can say that Model_2 is quite good because of
## less error value. 

###################################Thank you ##################################
