## Prepare a prediction model for profit of 50_startups data.
Startup <- read.csv(file.choose())
View(Startup)
#We found the in state column there are factor type of data
#which is not useful for building  Model
#we create a dummy variable for State column


#### creating a dummy variable#### 

library(dummies)
state_dummy <- dummy(Startup$State)
View(state_dummy)
#bind this two data sets, by cibnd function

Sate_1 <- cbind(Startup,state_dummy)
View(Sate_1)

Sate_1 <- Sate_1[-4]
View(Sate_1)

colnames(Sate_1)[7] <- "Newyork"


library(readr)
summary(Sate_1)
str(Sate_1)
plot(Sate_1)
attach(Sate_1)

##Histogram  startup data set 'variables'

hist(Sate_1$R.D.Spend)
hist(Sate_1$Administration)
hist(Sate_1$Marketing.Spend)
hist(Sate_1$Profit)
hist(Sate_1$StateCalifornia)
hist(Sate_1$StateFlorida)
hist(Sate_1$Newyork)

####Variance of Startup data set "Variables"

var(Sate_1$R.D.Spend)
var(Sate_1$Administration)
var(Sate_1$Marketing.Spend)
var(Sate_1$Profit)
var(Sate_1$StateCalifornia)
var(Sate_1$StateFlorida)
var(Sate_1$Newyork)

################ Standard deviance of Startup set "variable"

sd(Sate_1$R.D.Spend)
sd(Sate_1$Administration)
sd(Sate_1$Marketing.Spend)
sd(Sate_1$Profit)
sd(Sate_1$StateCalifornia)
sd(Sate_1$StateFlorida)
sd(Sate_1$Newyork)

###Busines 3rd and 4th Moment. (skewness and kurtosis)

library(moments)
skewness(Sate_1$R.D.Spend)
skewness(Sate_1$Administration)
skewness(Sate_1$Marketing.Spend)
skewness(Sate_1$Profit)
skewness(Sate_1$StateCalifornia)
skewness(Sate_1$StateFlorida)
skewness(Sate_1$Newyork)


kurtosis(Sate_1$R.D.Spend)
kurtosis(Sate_1$Administration)
kurtosis(Sate_1$Marketing.Spend)
kurtosis(Sate_1$Profit)
kurtosis(Sate_1$StateCalifornia)
kurtosis(Sate_1$StateFlorida)
kurtosis(Sate_1$Newyork)

### Bar Plot########

barplot(Sate_1$R.D.Spend)
barplot(Sate_1$Administration)
barplot(Sate_1$Marketing.Spend)
barplot(Sate_1$Profit)
barplot(Sate_1$StateCalifornia)
barplot(Sate_1$StateFlorida)
barplot(Sate_1$Newyork)

########Box plot############

boxplot(Sate_1$R.D.Spend)
boxplot(Sate_1$Administration)
boxplot(Sate_1$Marketing.Spend)
boxplot(Sate_1$Profit)
boxplot(Sate_1$StateCalifornia)
boxplot(Sate_1$StateFlorida)
boxplot(Sate_1$Newyork)

########Mean, Median, Mode###########

mean(Sate_1$R.D.Spend)
mean(Sate_1$Administration)
mean(Sate_1$Marketing.Spend)
mean(Sate_1$Profit)
mean(Sate_1$StateCalifornia)
mean(Sate_1$StateFlorida)
mean(Sate_1$Newyork)

median(Sate_1$R.D.Spend)
median(Sate_1$Administration)
median(Sate_1$Marketing.Spend)
median(Sate_1$Profit)
median(Sate_1$StateCalifornia)
median(Sate_1$StateFlorida)
median(Sate_1$Newyork)


mode(Sate_1$R.D.Spend)


########## Lets begin the model#######

Model_Startup <- lm(Profit~., data = Sate_1)
summary(Model_Startup)


## R^2 is 0.95, 
## SQRT is 2.56e-13
## Here we can conclude that the P value apart from R.D.Spend has greater than 0.05.
## These 4 varialbe are insignificant

#Lets explore more, build a model in these four variable alone

Model_Admin <- lm(Profit~Administration)
summary(Model_Admin)

#R^2 is 0.040
##When we build alone with this variable we can say that Administration is insignificant


Model_MT <- lm(Profit~Marketing.Spend)
summary(Model_MT)
# R^2 is 0.559
#Here again we find the build a model alone with Maketing.spend,this is significant


Model_CA <- lm(Profit~StateCalifornia)
summary(Model_CA)
##R^2 is, 0.02127, StateCalifornia is also insignificant to our Model

Model_FL <- lm(Profit~StateFlorida)
summary(Model_FL)
#R^2 is 0.0315, and again StateFlorida is also insginificant to our Model


#Let build a model with these four variable  together
Model_both <- lm(Profit~Administration+Marketing.Spend+StateCalifornia+StateFlorida)
summary(Model_both)

#When we take these four variable together we find that, Administration and Marketing.spend
#are significant and Statecalifornia and StateFlorida is insignificant 
#R^2, is 0.6131

library(psych)
library(car)
pairs.panels(Sate_1) #Through this plot we can check so many things
# like r value, graph linear or not linear, co-linearity relationship


influence.measures(Model_Startup)
#From consolve window we can say that row no. values in row no. 47,50
#we delete all than again lost lots off data

#Ploting the influence measures
influenceIndexPlot(Model_Startup)
influencePlot(Model_Startup) ## More clear picture from that plot
# we find that value in 1st, 46,47, 49, 50 rows has insignificant to that data


##### Building a model again to delete this values 
Model_2 <- lm(Profit~., data = Sate_1[-c(46,47,49,50),])
summary(Model_2)

## we find that R^2 value increased but however
## Administration, Marketing.Spend(quiet close to P value), 
## StateCalifornia, StateFlorida all these 4 again insignificant




##Going to apply" Backword elimination technique"#########


Model_A <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+StateCalifornia+StateFlorida+Newyork, data = Sate_1)
summary(Model_A)


## Removing state

Model_B <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = Sate_1)
summary(Model_B)

##Removing Marketing.Spend

Model_Marketing <- lm(Profit~R.D.Spend+Administration, data = Sate_1)
summary(Model_Marketing)

## Removing Administration

Model_Adminis <- lm(Profit~R.D.Spend, data = Sate_1)
summary(Model_Adminis)


## Removing the varialbe which are insignificant to the Model

Final_Model_A <- lm(Profit~R.D.Spend)

summary(Final_Model_A)
SQRT_Final_Model_A <- sqrt(sum(Final_Model_A$residuals)^2)/nrow(Sate_1)
SQRT_Final_Model_A
plot(Final_Model)
qqPlot(Model_1)

# has less than 0.05 , which is good.
# R^2 value is 0.94
#SQRT is 5.68e-14

### exponential model ###

Final_Model_B <- lm(log(Profit)~R.D.Spend)

summary(Final_Model_B)
SQRT_Final_Model_B <- sqrt(sum(Final_Model_B$residuals)^2)/nrow(Sate_1)
SQRT_Final_Model_B
plot(Final_Model_B)
qqPlot(Model_B)

# has less than 0.05 , which is good.
# R^2 value is 0.7586
## SQRT is 3.122506e-18

### Creating a table for RMSE value here because the  variable is significant to the model
### Before Variables are insignificant to the Models, that's why we did't created

Table_RMSE <- data.frame(c("SQRT_Final_Model_A","SQRT_Final_Model_B" ),c(SQRT_Final_Model_A,SQRT_Final_Model_B))
colnames(Table_RMSE) <- c("Model_name", "Model_rmse_value")
View(Table_RMSE)


## we can say that from above two model 
## Final_model_B is better than " Final_Model_A" 
## because Final_model_A is better R^2 value. 
##Apart from R^2 Value RMSE value of Final_Model_B is least than other. 

###############################Thank You################################################

