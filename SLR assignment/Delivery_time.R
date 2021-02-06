library(readr)
#Delivery_time -> Predict delivery time using sorting time
###Sorting time is input variable.
### Delivery Time is out variable.
Delivery <- read.csv(file.choose())
View(Delivery)
plot(Delivery)
plot(Delivery$Sorting.Time, Delivery$Delivery.Time)
#Delivery is depend on Sorting. Time variable 
#Delivery.Time is dependent variable in Y-axis and Sorting.Time is independent varaible in X-axis
summary(Delivery)
hist(Delivery$Delivery.Time)
hist(Delivery$Sorting.Time)

sd(Delivery$Delivery.Time)
sd(Delivery$Sorting.Time)

var(Delivery$Delivery.Time)
var(Delivery$Sorting.Time)

library(moments)
skewness(Delivery$Delivery.Time)
kurtosis(Delivery$Sorting.Time)
str(Delivery)

barplot(Delivery$Delivery.Time)
barplot(Delivery$Sorting.Time)

boxplot(Delivery)
attach(Delivery)

##checking the co-relation between the varialbes
cor(Sorting.Time,Delivery.Time)
#There is a Moderate corelation between these two variables, 
#strong co-relation is, when r value is greater than 0.85

#Building a model

Model_1 <- lm(Delivery.Time~Sorting.Time)
confint(Model_1, level = 0.95)
summary(Model_1)
# R-squared value is.68
#Higher the R-squared value best is the model

Model_1$fitted.values # to check the predicted values
Model_1$residuals #to check the error values

# predicted the values for model

Model_Pred <- predict(Model_1, interval = "predict")
Model_Pred
RMSE_1 <- sqrt(sum(Model_1$residuals^2)/nrow(Delivery)) #RMSE
RMSE_1

## RMSE values 2.79


## lets try to increase the R-squared value, building a  logrithim model or exp. model

Model_2 <- lm(Delivery.Time~log(Sorting.Time))
summary(Model_2)

#There is a slightly change in R value not as much. R-Squared value is 0.6954
Model_2_pred <- predict(Model_2, interval = "predict")
Model_2_pred
confint(Model_2, level = 0.95)
RMSE_2 <- sqrt(sum(Model_2$residuals^2)/nrow(Delivery))  #RMSE
RMSE_2
# RMSE value, 2.733 


##Building a exp. model

Model_3 <- lm(log(Delivery.Time)~Sorting.Time)
Model_3_pred <- predict(Model_3, interval = "predict")
Model_3_pred
confint(Model_3, level = 0.95)
summary(Model_3)
RMSE_3 <- sqrt(sum(Model_3$residuals^2)/nrow(Delivery))  #RMSE
RMSE_3
#RMSE value is 0.166

## Here R-squared value is 0.7109 ,which is better than above two models
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