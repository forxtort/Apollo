#Recommendation Systems
#Problem statement: Recommend a best book based on the author,publisher & ratings.

############## Packages required for Recommendation system ######################

#install.packages("readr")
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)
library(readr)
library(Matrix)
#### before proceding futher, we have to know that where you this system in our daily
#### life, Eg: Youtube, or social media platform, why I am saying this because
### lets take an example when we watch something on you tube, than after some time we get 
### some type of stuff that never vedio never we clicked but it is resemble to our previous vedio
### same thing happen in "Amazon, facebook", we you click any "item" there they in your listing
### you will see the same type of objets. 

########################### Let explore the Book data set #############################

#Importing Data Set
book_text <- read.csv(file.choose())
View(book_text)
class(book_text)
summary(book_text)

################# Variance and standard deviation ######################################

var(book_text)
sd(book_text$Book.Rating)
########################################################################################

range(book_text$Book.Rating)

########################## 3rd, 4th Business Moment ####################################
#install.packages("moments")
library(moments)
skewness(book_text$Book.Rating)
kurtosis(book_text$Book.Rating)
#######################################################################################

sum(is.na(book_text))
attach(book_text)

## str fuction define the structure of the data set ##################################
str(book_text)
table(book_text$Book.Title)

#Rating distribution
#################### Histogram ######################
hist(book_text$Book.Rating)

#################### barplot #######################
barplot(Book.Rating)

#################### boxplot ######################
boxplot(Book.Rating)

## removing the first column, it is just like serial no. ##
book<-book_text[-1]

View(book)

#The datatype should be realRatingMatrix in order to build recommendation engine
book_matrix <- as(book, 'realRatingMatrix')
View(book_matrix)


################################## Model 1########################################

###  Recommednation system based on "Popularity based" ###

book_recomm_model1 <- Recommender(book_matrix, method="POPULAR")

###  Predictions for two users 

recommended_items1 <- predict(book_recomm_model1, book_matrix[313:314], n=3)
as(recommended_items1, "list")

## Popularity model recommends the same books for all users , we need to improve our model
#Using Collaborative Filtering
#Item Based Collaborative Filtering, User Based Colloborative Filtering. 

##################################### Model 2#####################################

####  Recommednation system based on "Item based collorative filtering" ###

book_recomm_model2 <- Recommender(book_matrix, method="IBCF")

#Predictions for Users 

recommended_items2 <- predict(book_recomm_model2, book_matrix[313:314], n=3)
as(recommended_items2, "list")

#################################### Model 3 #################################

####  Recommednation system based on "USER based collorative filtering" ###

book_recomm_model3 <- Recommender(book_matrix, method="UBCF")

#Predictions for two users 

recommended_items3 <- predict(book_recomm_model3, book_matrix[313:314], n=3)
as(recommended_items3, "list")


############################# Thank you ##############################################
