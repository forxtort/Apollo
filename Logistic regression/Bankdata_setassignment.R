##Output variable -> y
##y -> Whether the client has subscribed a term deposit or not 
##Binomial ("yes" or "no")

getwd()
### We use sep=";" here because the original data is not seperate ##3 
### for seperating the data we use sep=";" ## 

Bank <- read.csv(file.choose(), sep = ";")
View(Bank)
sum(is.na(Bank))
attach(Bank)

table(Bank$y)
table(Bank$poutcome)

## Building a Model ##
str(Bank)

Model_bank <- glm(factor(y)~., data = Bank, family = "binomial")
summary(Model_bank)

###AIC:21650
## MOdel prediction##


Model_pred_bank <- predict(Model_bank, data=Bank, type = 'response')
Model_pred_bank

## To find the odds ration ##
exp(coef(Model_bank))

##  confusion matrix ##

confusion <- table(Model_pred_bank>0.5, Bank$y)
confusion

#### Model accuracy ####
Bank_accuracy <- sum(diag(confusion))/sum(confusion)
Bank_accuracy

## Error##

bank_Model_error <- 1-Bank_accuracy
bank_Model_error

## Creating a Null vector to 
pred_value <- NULL
pred_value <- ifelse(Model_pred_bank>0.5, 1,0)
pred_value
Yes_no <- ifelse(Model_pred_bank>0.5,'Yes',"NO")
Yes_no

## store this value in "Bank data" ##
Bank["pred_value"] <- pred_value
Bank["Yes_no"] <- Yes_no

View(Bank[,c(17:19)])
table(Bank$y,Bank$Yes_no)

## ROC curve ## with threshold value 0.5
library(ROCR)
Bank_ROCR_pred <- prediction(Model_pred_bank, Bank$y)
Bank_ROCR_perf <- performance(Bank_ROCR_pred,"tpr","fpr")
plot(Bank_ROCR_perf, colorize=T)
## Area the area under the curve, best is the model.

## Check out the Cut_off values, true +ve rate, false-ve rate.##
Book_table_cutoff <- data.frame(cut_off=Bank_ROCR_perf@alpha.values[[1]], fpr=Bank_ROCR_perf@x.values, tpr=Bank_ROCR_perf@y.values)
colnames(Book_table_cutoff) <- c("cut_off",'fpr',"tpr")
View(Book_table_cutoff)

################################# Thank you ############################################
