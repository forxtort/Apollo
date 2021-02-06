#Association Rule Problem Statement for "My_movies" Data sets

#Prepare rules for all the data sets 
#1) Try different values of support and confidence. Observe the change in number
#of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots

####################### packages required for Association rules #######################
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
library(readr)

#Data Set : my_movies 

my_movies<-read.csv(file.choose())
View(my_movies)

class(my_movies)
str(my_movies)
summary(my_movies)
#############Variance and standard deviation ###############
var(my_movies)
sd(my_movies$Sixth.Sense)
sd(my_movies$Gladiator)
sd(my_movies$LOTR1)
sd(my_movies$Harry.Potter1)
sd(my_movies$Patriot)
sd(my_movies$LOTR2)
sd(my_movies$Harry.Potter2)
sd(my_movies$LOTR)
sd(my_movies$Braveheart)
sd(my_movies$Green.Mile)

range(my_movies[,6:15])
################# 3rd and 4th Business moment #################

#install.packages("moments")
library(moments)
skewness(my_movies[,6:15])
kurtosis(my_movies[,6:15])
sum(is.na(my_movies))
attach(my_movies)
################################Barplot ##################################
barplot(my_movies$Sixth.Sense)
barplot(my_movies$Gladiator)
barplot(my_movies$LOTR1)
barplot(my_movies$Harry.Potter1)
barplot(my_movies$LOTR)
barplot(my_movies$Braveheart)
barplot(my_movies$Green.Mile)

################################ Box plot ###############################

boxplot(my_movies$Sixth.Sense)
boxplot(my_movies$Gladiator)
boxplot(my_movies$LOTR1)
boxplot(my_movies$Harry.Potter1)
boxplot(my_movies$LOTR)
boxplot(my_movies$Braveheart)
boxplot(my_movies$Green.Mile)

###############################################################################

#Character data from column 1 to 5(V1, V2,V3,V4,V5) is been converted to binary
#format and already attached to the given data set. Hence, we will consider binary 
# data for extracting rules.
movies_trans<-as(as.matrix(my_movies[,6:15]),"transactions")
inspect(movies_trans[1:10])

# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
################################# Model-A  ########################################### 
rules_A<-apriori(movies_trans,parameter = list(support=0.002,confidence=0.07,minlen=3))
rules_A  ### tell us total no. of rules
inspect(rules_A[1:10]) ## higher the lift ration best is the model 

plot(rules_A,method = "scatterplot", jitter=0)
plot(rules_A,method = "grouped")
plot(rules_A,method = "graph")  
plot(rules_A,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules_A)) 
head(sort(rules_A, by="lift"))
inspect(head(sort(rules_A, by="lift"))) ## sorted out best six rule 

################################### Model-B #######################################
rules_B<-apriori(movies_trans,parameter = list(support=0.003,confidence=0.6,minlen=4))
rules_B
inspect(rules_B[1:10])

plot(rules_B,method = "scatterplot", jitter=0)
plot(rules_B,method = "grouped")
plot(rules_B,method = "graph")  
plot(rules_B,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules_B)) 
head(sort(rules_B, by="lift"))
inspect(head(sort(rules_B, by="lift"))) ## sorted out best six rule 

################################ Model-C ###########################################

rules_C<-apriori(movies_trans,parameter = list(support=0.004,confidence=0.8,minlen=2))
inspect(rules_C[1:10])

plot(rules_C,method = "scatterplot", jitter=0)
plot(rules_C,method = "grouped")
plot(rules_C,method = "graph")  
plot(rules_C,method = "matrix3D")  ## for this method is best to interpret which  
## item we have to put  right side and left side.

head(quality(rules_C)) 
head(sort(rules_C, by="lift"))
inspect(head(sort(rules_C, by="lift"))) ## sorted out best six rule 

################################# Thank you ############################################