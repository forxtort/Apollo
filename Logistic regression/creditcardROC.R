#Classify whether application accepted or not using Logistic regression
#Problem Statement for Credit Card Data

#install.packages("readr")
library(readr)
Credit<-read.csv(file.choose())
View(Credit)

sum(is.na(Credit)) # To get the count of NA Values
attach(Credit)
str(Credit)
summary(Credit)
## Histogram ###
hist(Credit$reports)
hist(Credit$age)
hist(Credit$income)
hist(Credit$share)
hist(Credit$expenditure)
hist(Credit$majorcards)

## Boxplot ###

boxplot(Credit$reports)
boxplot(Credit$income)

### Business moment 3rd and 4th ### 

skewness(Credit$reports)
skewness(Credit$age)
skewness(Credit$income)

kurtosis(Credit$reports)
kurtosis(Credit$age)
kurtosis(Credit$income)

## removing the first column ##

Credit <- Credit[-1] 
#View(Credit)
## Model building ##

logit<-glm(factor(card)~.,family=binomial,data = Credit)
summary(logit)

## odd's ratio ##
exp(coef(logit))

###prediction he future values ###
logit_pred <- predict(logit, data=Credit, type = 'response')
logit_pred

## confusion matrix

Confusion_matrix <- table(logit_pred>0.5, Credit$card)

Confusion_matrix
## Accuracy of Model ##

Model_accuracy <-  sum(diag(Confusion_matrix))/sum(Confusion_matrix)
Model_accuracy

##Creating Empty vector to store the predicted values##
Pred_values <- NULL
Pred_values <- ifelse(logit_pred>0.5, 1, 0)

Yes_NO <- NULL
Yes_NO <- ifelse(logit_pred>0.5, "Yes", "NO")

### Creating a column for above Null Vector

Credit["Pred_values"] <- Pred_values
Credit["Yes_NO"] <- Yes_NO
Credit['logit_pred '] <- logit_pred

View(Credit[,c(1,13:15)])

table(Credit$card,Credit$Yes_NO)

## Roc curve##
library(ROCR)

Roc_credit_pred <- prediction(logit_pred, Credit$card)
roc_credit_pref <- performance(Roc_credit_pred,"tpr","fpr")
plot(roc_credit_pref, colorize=T)
## higher the cureve under ROC curve best is the model

##3 Cut off or theshold values along with tpr and fpr

Table_cut <- data.frame(cut_off=roc_credit_pref@alpha.values[[1]], tpr=roc_credit_pref@y.values, fpr=roc_credit_pref@x.values)
colnames(Table_cut) <- c("cut_off","tpr","fpr")
View(Table_cut)
