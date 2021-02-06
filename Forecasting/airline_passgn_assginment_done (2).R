library(readr)
library(readxl)
Amtrak <- read_xlsx(file.choose())
View(Amtrak)
plot(Amtrak)
plot(Amtrak$Passengers)
summary(Amtrak)
str(Amtrak)
hist(Amtrak$Passengers)
var(Amtrak$Passengers)
sd(Amtrak$Passengers)
mean(Amtrak$Passengers)
boxplot(Amtrak$Passengers)
boxplot(Amtrak)
mode(Amtrak$Passengers)
median(Amtrak$Passengers)
library(moments)
skewness(Amtrak$Passengers)
kurtosis(Amtrak$Passengers)
barplot(Amtrak$Passengers)

######creating a dummy variable, to use in different forecasting method##3
##seasonality is 12 months, so we create 12 dummy variables
dummy_sales <- data.frame(outer(rep(month.abb,length=96), month.abb, "==")+0)
colnames(dummy_sales) <- month.abb
colnames(dummy_sales)
## Combining the two data frame with cbind funtcion
Airline_Passn <- cbind(Amtrak, dummy_sales)
View(Airline_Passn)
##creating a time varialbe t
Airline_Passn["t"] <- c(1:96)
Airline_Passn["t_square"] <- Airline_Passn$t*Airline_Passn$t
#output varialbe is sales and we know that in some forecasting model
## we need log of output variable so creating a log of sales
Airline_Passn["log_passn"] <- log(Airline_Passn$Passengers)

View(Airline_Passn)
###divide the data in train and test
train <- Airline_Passn[1:80,]
test <- Airline_Passn[80:96,]

############Creating a different type of Forecastig Model##
######## 1st model "linear Model" only one input variable t

Model_1 <- lm(Passengers~t, data = train)
Model_1_pred <- data.frame(predict(Model_1, newdata = test, interval = "predict"))
Model_RMSE <- sqrt(mean((test$Passengers- Model_1_pred$fit)^2))
Model_RMSE
######### 2nd model "exponential" method##
Model_2 <- lm(log_passn~t, data = train)
Model_2_pred <- data.frame(predict(Model_2, interval = "predict", newdata = test))
Model_2_RMSE <- sqrt(mean((test$Passengers- exp(Model_2_pred$fit))^2))
Model_2_RMSE
########## 3rd Model "quadratic Model""
Model_3 <- lm(Passengers~ t+t_square, data = train)
Model_3_pred <- data.frame(predict(Model_3, interval="predict", newdata=test))
Model_3_RMSE <- sqrt(mean((test$Passengers-Model_3_pred$fit)^2))
Model_3_RMSE
######### 4th Model "Additive Seasonality "####
Model_4 <- lm(Passengers~Jan+Feb+Mar+Apr+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
Model_4_pred <- data.frame(predict(Model_4, newdata = test, interval = "predict"))
Model_4_RMSE <- sqrt(mean((test$Passengers-Model_4_pred$fit)^2))
Model_4_RMSE

############ 5th Model "' Additive seasonality with quadratic trend"##
Model_5 <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
Model_5_pred <- data.frame(predict(Model_5, newdata = test, interval = "predict"))
Model_5_RMSE <- sqrt(mean((test$Passengers-Model_5_pred$fit)^2))
Model_5_RMSE

###########6th Modle""Multiplicative seasonality#######
Model_6 <- lm(log_passn~Jan+Feb+Mar+Apr+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
Model_6_pred <- data.frame(predict(Model_6, newdata = test, interval = "predict"))
Model_6_RMSE <- sqrt(mean((test$Passengers-Model_6_pred$fit)^2))
Model_6_RMSE

#######Creating a table for RMSE values

table_RMSE <- data.frame(c("Model_RMSE", "Model_2_RMSE", "Model_3_RMSE", "Model_4_RMSE","Model_5_RMSE", "Model_6_RMSE" ),c(Model_RMSE,Model_2_RMSE, Model_3_RMSE,Model_4_RMSE, Model_5_RMSE, Model_6_RMSE))
colnames(table_RMSE) <- c("Model_RMSE","Model_rmse_values")
View(table_RMSE)

#########additive seasonality with quadratic has least RMSE value
##Least RMSE value best the Model

setwd("D:/Data science/Data Science vedio/Uns-supervised model/Forecasting/Forecasting assignmet data sets")
write.csv(Airline_Passn, file = "Airline_Passn.csv", col.names = F, row.names = F)

###############Final Model#########
Model_nord <- read.csv(file.choose())
View(Model_nord)
Final_model_pred <- data.frame(predict(Model_5, newdata = Model_nord,interval = "predict"))
Final_model_pred
