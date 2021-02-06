library(readr)
library(smooth)
library(greybox)
library(readxl)
library(fpp)
library(forecast)
library(tseries)
############Above following packages are requied to run the smoothing models###
Coca <- read_xlsx(file.choose())
View(Coca)
summary(Coca)
str(Coca)
str(Coca$Sales)
var(Coca$Sales)
plot(Coca$Sales)
boxplot(Coca$Sales)
library(moments)
skewness(Coca$Sales)
kurtosis(Coca$Sales)
mean(Coca$Sales)
median(Coca$Sales)
mode(Coca$Sales)
hist(Coca$Sales)
barplot(Coca$Sales)
## covert the data into time series data format###########

Coca_sales <- ts(Coca$Sales,frequency = 4, start = c(86))

#partition the data into "train" and "test" ###

train <- Coca_sales[1:38]
test <- Coca_sales[39:42]
#coverting train and test data inot time series format####

train <- ts(train, frequency = 4)
test <- ts(test, frequency = 4)
View(train)
View(test)
##########Building a model from smoothing technique######
#with optimal values, and alpha=0.2
# simple exponential smoothing method.

Hw_a <- HoltWinters(train, alpha = 0.2, beta = F, gamma = F)
hw_pred <- data.frame(predict(Hw_a, n.ahead = 4))
Hw_a_mape <- MAPE(hw_pred$fit,test)*100
Hw_a_mape

############# Building second model###
#optimal values, alpha=0.2, beta=0.1,
#also called double exponential method
hw_b <- HoltWinters(train,alpha = 0.2,beta = 0.1, gamma = F)
hw_b_pred <- data.frame(predict(hw_b,n.ahead = 4))
hw_b_pred
Hw_b_mape <- MAPE(hw_b_pred$fit,test)*100
Hw_b_mape
###############Building third model####
#optimal values, alpha=0.2, beta=0.1, gamma=0.1
#also called Winter's Method
hw_c <- HoltWinters(train, alpha = 0.2, beta=0.1, gamma=0.1)
hw_c_pred <- data.frame(predict(hw_c, n.ahead = 4))
hw_c_mape <- MAPE(hw_c_pred$f,test)*100
hw_c_mape

#########Building model####
#without choosing a optimal values###
#model will chosse automatic these values
Hw_model <- HoltWinters(train)
hw_model_pred <- data.frame(predict(Hw_model, n.ahead = 4))
hw_model_mape <- MAPE(hw_model_pred$fit,test)*100
hw_model_mape

#let check which optimal values automatically choose by" hw_model"##
Hw_model$alpha ### alpha=0.378
Hw_model$beta ## beta=0.256
Hw_model$gamma ##gamma=0.889

#####creating a dataframe for MAPE values#####
table_MAPE <- data.frame(c("Hw_a_mape","Hw_b_mape", "hw_c_mape","hw_model_mape"),c(Hw_a_mape,Hw_b_mape,hw_c_mape,hw_model_mape))
colnames(table_MAPE) <- c("Models_mape", "model_mape_values")
View(table_MAPE)
#form data frame "table_MAPE" has least MAPE value
##least the MAPE value best the Model

####applying the moving avg. method to the above data sets "Coca_sales"##
Moving_model <- sma(train)
Moving_pred <- data.frame(predict(Moving_model, h=4))
Moving_mape <- MAPE(Moving_pred$Point.Forecast,test)*100
Moving_mape


######### building a model from "ARIMA Method"#####
acf(train)
pacf(train)
plot(train)
ARIMA_model <- arima(train,order = c(1,1,8), method = "ML")

#####Here if are confuse about lags, how many lags we have to chosse, 
##use method of parsimony
Auto_Arima <- auto.arima(train)
Auto_Arima_pred <- data.frame(forecast(Auto_Arima))
acf(Auto_Arima$residuals)
pacf(Auto_Arima$residuals)
plot(forecast(Auto_Arima,h=12))

#######Finished Forecasting method from Smoothing Technique####