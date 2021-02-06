#Association Rule Problem Statement for 'Book data set'.

#Prepare rules for all the data sets 
#1) Try different values of support and confidence. Observe the change in number
#of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots


#### Packages require for Association rule as mention below#####

#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)

#################################################################


#install.packages("readr")
library(readr)

############################ Data Set : Book  ##################
############################ Lets explore the Model ############

book<-read.csv(file.choose())
View(book)
class(book)
str(book)
summary(book)

###### Variance and standard deviation for Book data ###########

var(book)
sd(book$ChildBks)
sd(book$YouthBks)
sd(book$CookBks)
sd(book$DoItYBks)
sd(book$RefBks)
sd(book$ArtBks)
sd(book$GeogBks)
sd(book$ItalCook)
sd(book$ItalAtlas)
sd(book$ItalArt)
sd(book$Florence)
range(book)

###### 3rd and 4th Business Moment ######
##install.packages("moments")
library(moments)
skewness(book)
kurtosis(book)
sum(is.na(book))
attach(book)

############################## Barplot ############################################
barplot(book$ChildBks)
barplot(book$YouthBks)
barplot(book$CookBks)
barplot(book$DoItYBks)
barplot(book$RefBks)
barplot(book$ArtBks)
barplot(book$GeogBks)
barplot(book$ItalCook)
barplot(book$ItalAtlas)
barplot(book$ItalArt)
barplot(book$Florence)

### we are not getting clear picture from plots because the range of
### book data is from 0-1, that's why we are getting such complete black and white image. 

############################### boxplot ###########################################

boxplot(book$ChildBks)
boxplot(book$YouthBks)
boxplot(book$CookBks)
boxplot(book$DoItYBks)
boxplot(book$RefBks)
boxplot(book$ArtBks)
boxplot(book$GeogBks)
boxplot(book$ItalCook)
boxplot(book$ItalAtlas)
boxplot(book$ItalArt)
boxplot(book$Florence)

## Here the same thing will apply as i metioned in barplot, range is 0-1
## we are not getting so much clear picture about boxplot.

#################################### Histogram #####################################

hist(book$ChildBks)
hist(book$YouthBks)
hist(book$CookBks)
hist(book$DoItYBks)
hist(book$RefBks)
hist(book$ArtBks)
hist(book$GeogBks)
hist(book$ItalCook)
hist(book$ItalAtlas)
hist(book$ItalArt)
hist(book$Florence)

### you get the same type of histrogram because of the same type of data 
### range is also 0-1 .

#################################### Model Exoploring ##################################

### Model building, but there are some certain rule in Association
### we consider all rows or obervation as "transaction" 

book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])

# Whenever we have binary kind of data .....load them as csv and convert them into 
# matrix format using as.matrix(data) and use this for forming 
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transaction
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 

rules_A<-apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.7,minlen=4))
rules_A  ### tell us total no. of rules
inspect(rules_A[1:100])# higher the lift ration best is the model 

plot(rules_A,method = "scatterplot", jitter=0)
plot(rules_A,method = "grouped")
plot(rules_A,method = "graph")  
plot(rules_A,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules_A)) 

head(sort(rules_A, by="lift"))
inspect(head(sort(rules_A, by="lift"))) ## sorted out best six rule 


################################ Second Model ###################################

## Changing the support  and confidence value, right now we are taking these
## these values as rondom, but in practically these values are depend or suggested
## from the "Subject Matter Expert" ##

rules_B<-apriori(as.matrix(book),parameter = list(support=0.003,confidence=0.6,minlen=2))
inspect(rules_B[1:100])


plot(rules_B,method = "scatterplot", jitter=0)
plot(rules_B,method = "grouped")
plot(rules_B,method = "graph")
plot(rules_B,method = "matrix3D")## for this method is best to interpret which  
## item we have to put right side and left side

head(quality(rules_B)) 

inspect(head(sort(rules_B, by="lift"))) ## sorted out best six rule



############################# Model-3 ###########################################
rules_C<-apriori(as.matrix(book),parameter = list(support=0.004,confidence=0.8,minlen=2))
inspect(rules_C[1:100])

plot(rules_C,method = "scatterplot", jitter=0)
plot(rules_C,method = "grouped")
plot(rules_C,method = "graph")
plot(rules_C,method = "matrix3D") ## for this method is best to interpret which  
## item we have to put right side and left side

head(quality(rules_C)) ### sorted the rules by everthing like, support, confidence, lift etc.

inspect(head(sort(rules_B, by="lift"))) ## sorted out best six rule

################################# Thank you #############################################
