#Prepare a classification model using Naive Bayes for salary data 

#Data Description:

#  age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#  race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

####################### Lets explore the Model #######################################
##install.packages("readr")
library(readr)
setwd("F:/Assignment pending folder/Naive Bayes")
getwd()

#Training and Test Data
Train<-read.csv("SalaryData_Train.csv")
#View(Train)
str(Train)  ## Define the structure of the data frame ###
Train$educationno <-as.factor(Train$educationno)
str(Train)
Test<-read.csv("SalaryData_Test.csv")
Test$educationno <-as.factor(Test$educationno)
#View(Test)
str(Test)
summary(Test)

#################### "Data set Visualization " #########################

library(ggplot2)
# Plot and ggplot  for salary vs age
ggplot(data=Train,aes(x=Train$Salary, y = Train$age, fill = Train$Salary)) +
  geom_boxplot() +
  ggtitle("BoxPlot")

############### ggplot for capitalloss vs salary##################

ggplot(data=Train,aes(x=Train$Salary, y = Train$capitalloss, fill = Train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(Train$occupation,Train$Salary)
plot(Train$maritalstatus,Train$Salary)
plot(Train$race,Train$Salary)


################### Density Plot #######################

#Density Plot
### density plot for age vs salary

ggplot(data=Train,aes(x = Train$age, fill = Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Age - Density Plot")

## density plot for workclas vs salary
ggplot(data=Train,aes(x = Train$workclass, fill = Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Workclass Density Plot")

####################### Model Building fromation ##########################
# Naive Bayes Model 
Model <- naiveBayes(Train$Salary ~ ., data = Train)
Model

### Predict the values 
Model_pred <- predict(Model,Test[-14])
Model_pred

## Accuracy of the model
mean(Model_pred==Test$Salary)

## we get accuracy for this model is 81.8%

#### Cross table 
library(gmodels)
CrossTable(Model_pred,Test$Salary)

############################Thank you ###########################################
## I tried this begging method but get same accuracy that's I am not mention above
## but you can run this model ####
acc<-NULL

for (i in 1:10) {
 
  Model <- naiveBayes(Train$Salary ~ ., data = Train)
  
   Model_pred <- predict(Model,Test[-14])

  acc <- c(acc,mean(Model_pred==Test[,14]))
}

mean(acc)

