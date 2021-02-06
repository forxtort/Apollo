#Problem Statement : PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

getwd()
setwd("D:/Data science/Data Science vedio/Supervised assignement forlder/Neutal network")
library(readr)
forest <- read.csv("forestfires.csv")
View(forest)
str(forest)
summary(forest)

############################### Lets start Data exploring ################################

## standard deviation ##

sd(forest$FFMC)
sd(forest$DMC)
sd(forest$DC)
sd(forest$ISI)
sd(forest$temp)
sd(forest$RH)
sd(forest$wind)
sd(forest$rain)
sd(forest$area)

## variance ##

var(forest$FFMC)
var(forest$DMC)
var(forest$DC)
var(forest$ISI)
var(forest$temp)
var(forest$RH)
var(forest$wind)
var(forest$rain)
var(forest$area)

#install.packages("moments")
library(moments)

################## 3rd and 4th "Business Moment" ########################################

skewness(forest$FFMC)
skewness(forest$DMC)
skewness(forest$DC)
skewness(forest$ISI)
skewness(forest$temp)
skewness(forest$RH)
skewness(forest$wind)
skewness(forest$rain)
skewness(forest$area)


###kurtosis ##

kurtosis(forest$FFMC)
kurtosis(forest$DMC)
kurtosis(forest$DC)
kurtosis(forest$ISI)
kurtosis(forest$temp)
kurtosis(forest$RH)
kurtosis(forest$wind)
kurtosis(forest$rain)
kurtosis(forest$area)

#####################################################################################

## histogram ##
hist(forest$FFMC)
hist(forest$DMC)
hist(forest$DC)
hist(forest$ISI)
hist(forest$temp)
hist(forest$RH)
hist(forest$wind)
hist(forest$rain)
hist(forest$area)

########boxplot #########
boxplot(forest$FFMC)
boxplot(forest$DMC)
boxplot(forest$DC)
boxplot(forest$ISI)
boxplot(forest$temp)
boxplot(forest$RH)
boxplot(forest$wind)
boxplot(forest$rain)
boxplot(forest$area)


## There is a rule for Neural Network data frame, i.e all data should be in numeric
## we are doing this by using the function name "as.numeric"

forest$month <- as.numeric(as.factor(forest$month))
forest$day<- as.numeric(as.factor(forest$day))
forest$size_category <- as.numeric(as.factor(forest$size_category))

View(forest)
table(forest$size)
forest<- forest[,c(1:10 , 12:31,11)]
View(forest)
summary(forest)

#creat a norm function to normalize the data
norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#testing of norm function
norm(c(1,2,3,-85))

#apply Norm function to the "forest" data set.
forest_N <- as.data.frame(lapply(forest,norm))
View(forest_N)
str(forest_N)
summary(forest_N)

###################### Data partition of area column ##################################
library(caret)
Traininglocal <- createDataPartition(forest_N$area, p=.70, list=F)
Train <- forest_N[Traininglocal,]
Test <- forest_N[-Traininglocal,]
#View(Train)
#View(Test)

library(nnet)
library(neuralnet)
################################# Model-1###############################################

Model_1 <- neuralnet(area~., data = Test)
str(Model_1)
plot(Model_1)
Predict_1 <- compute(Model_1, Test[1:30])
Area_burned <- Predict_1$net.result
Predict_1$neurons
cor(Area_burned, Test$area)## Co-relation value is 96%

################# Model-2 using "hidden neurons" startegy #############################

Model_2 <- neuralnet(area~., data =forest_N, hidden = c(5,4))
plot(Model_2)
Predict_area <- compute(Model_2, Test[1:30])
areaburned <- Predict_area$net.result
Predict_area$neurons
cor(areaburned, Test$area) ###### Co-relation is 98% ##################################
## we can say that Hidden neuron startegy is good for Model building. 

################################### THANK YOU ##########################################
