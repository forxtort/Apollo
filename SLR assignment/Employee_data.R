library(readr)
#Build a prediction model for Churn_out_rate 

###churn is independent variable.
### salary_hike is dependent variable.

Employee <- read.csv(file.choose())
View(Employee)
plot(Employee)
plot(Employee$Churn_out_rate, Employee$Salary_hike)
#"Salary_hike" depend on a "Churn out rate"
#"Salary_hike" is dependent variable in Y-axis and Churn_out is independent varaible in X-axis
summary(Employee)

hist(Employee$Salary_hike)
hist(Employee$Churn_out_rate)

sd(Employee$Salary_hike)
sd(Employee$Churn_out_rate)

var(Employee$Salary_hike)
var(Employee$Churn_out_rate)

library(moments)
skewness(Employee$Salary_hike)
kurtosis(Employee$Churn_out_rate)
str(Employee)

barplot(Employee$Salary_hike)
barplot(Employee$Churn_out_rate)

boxplot(Employee)
attach(Employee)

##checking the co-relation between the varialbes
cor(Employee$Churn_out_rate,Employee$Salary_hike)
#There is a "Negative strong corelation" between these two variables, 
#strong co-relation is, when r value is greater than 0.85

#Building a model

Model_1 <- lm(Employee$Salary_hike~Churn_out_rate)
confint(Model_1, level = 0.95)
summary(Model_1)
# R-squared value is.83
#Higher the R-squared value best is the model

Model_1$fitted.values # to check the predicted values
Model_1$residuals #to check the error values

# predicted the values for model

Model_Pred <- predict(Model_1, interval = "predict")
Model_Pred
RMSE_1 <- sqrt(sum(Model_1$residuals^2)/nrow(Employee)) #RMSE
RMSE_1

## RMSE values 35.89


## lets try to increase the R-squared value, building a  logrithim model or exp. model

Model_2 <- lm(Employee$Salary_hike~log(Churn_out_rate))
summary(Model_2)

#There is a slightly change in R value not as much. R-Squared value is 0.87
Model_2_pred <- predict(Model_2, interval = "predict")
Model_2_pred
confint(Model_2, level = 0.95)
RMSE_2 <- sqrt(sum(Model_2$residuals^2)/nrow(Employee))  #RMSE
RMSE_2
# RMSE value, 31.06 


##Building a exp. model

Model_3 <- lm(log(Employee$Salary_hike)~Churn_out_rate)
Model_3_pred <- predict(Model_3, interval = "predict")
Model_3_pred
confint(Model_3, level = 0.95)
summary(Model_3)
RMSE_3 <- sqrt(sum(Model_3$residuals^2)/nrow(Employee))  #RMSE
RMSE_3
#RMSE value is 0.198

## Here R-squared value is 0.8486 ,which is better than above two models
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