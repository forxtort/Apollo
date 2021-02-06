library(readr)
Calory <- read.csv(file.choose())
View(Calory)
#Changing the name of first coloum because it long name
#but remeber that weight gain is in "GRAM"
colnames(Calory)[1] <- "Weight_gain"
View(Calory)
plot(Calory)
plot(Calory$Calories.Consumed,Calory$Weight_gain)
#weight is depend on calories gained 
#weight is dependent variable in Y-axis and calories is independent varaible in X-axis
summary(Calory)
hist(Calory$Weight_gain)
hist(Calory$Calories.Consumed)

sd(Calory$Weight_gain)
sd(Calory$Calories.Consumed)

var(Calory$Weight_gain)
var(Calory$Calories.Consumed)

library(moments)
skewness(Calory$Weight_gain)
kurtosis(Calory$Calories.Consumed)
str(Calory)

barplot(Calory$Weight_gain)
barplot(Calory$Calories.Consumed)
boxplot(Calory)
attach(Calory)

##checking the co-relation between the varialbes
cor(Weight_gain, Calories.Consumed)
#There is a strong corelation between these two variables, 
#strong co-relation is, when r value is greater than 0.94

#Building a model

Model_1 <- lm(Weight_gain~Calories.Consumed)
confint(Model_1, level = 0.95)
summary(Model_1)
# R-squared value is.89
#Higher the R-squared value best is the model

Model_1$fitted.values # to check the predicted values
Model_1$residuals #to check the error values

# predicted the values for model

Model_Pred <- predict(Model_1, interval = "predict")
Model_Pred
RMSE_1 <- sqrt(sum(Model_1$residuals^2)/nrow(Calory)) #RMSE
RMSE_1

## RMSE values 103.3025


## lets try to increase the R-squared value, building a  logrithim model or exp. model

Model_2 <- lm(Weight_gain~log(Calories.Consumed))
summary(Model_2)

#There is a slightly change in R value not as much. R-Squared value is 0.8077
Model_2_pred <- predict(Model_2, interval = "predict")
Model_2_pred
confint(Model_2, level = 0.95)
RMSE_2 <- sqrt(sum(Model_2$residuals^2)/nrow(Calory))  #RMSE
RMSE_2
# RMSE value, 141.0054 


##Building a exp. model

Model_3 <- lm(log(Weight_gain)~Calories.Consumed)
Model_3_pred <- predict(Model_3, interval = "predict")
Model_3_pred
confint(Model_3, level = 0.95)
summary(Model_3)
RMSE_3 <- sqrt(sum(Model_3$residuals^2)/nrow(Calory))  #RMSE
RMSE_3
#RMSE value is 0.306
## Here R-squared value is 0.8776 ,which is better than above two models
## Apart from the R-squared values, RMSE value can also tell us which
## Model is best, least the 'RMSE' value, best is the "model".
# we can say that "exponential model/ Model_3" is best in all above "Three" models

#############Table for RMSE values#############
Table_rmse <- data.frame(c("RMSE_1", "RMSE_2", "RMSE_3"),c(RMSE_1,RMSE_2,RMSE_3))
colnames(Table_rmse) <- c("Models_RMSE_Name"," Model_RMSE_values")
View(Table_rmse)

#conclusion is MOdel_3 has least RMSE value, Least the RMSE value
# Best the Model
########################Thank YOU###########################