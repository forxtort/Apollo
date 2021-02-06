#Association Rule Problem Statement for 'Grocery data'.

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

############################ Data Set : Grocery  ##################
############################ Lets explore the data set ############

groceries<-read.transactions(file.choose(),format="basket") 
## why I choosed this method, to explore the file of "grocery" see line 56:58
View(groceries)
class(groceries)
str(groceries)
summary(groceries)

###### Variance and standard deviation for" Grocery data" ###########
## we cannot perform the variance and standard deviation for grocery data 
## because the data in factor from not in numeric or binary format
## "sd, var" data should be in numeric


###### 3rd and 4th Business Moment ######
##install.packages("moments")
library(moments)

#skewness(groceries)
#kurtosis(groceries)

## we can't get the skewness and kurtosis because data is not numeric on in binary format.
sum(is.na(groceries))

#################################### Histogram #####################################

## To perform this for grocery data set " numeric"

#################################### Model Exoploring ##################################

### Model building, but there are some certain rule in Association
### we consider all rows or obervation as "transaction" 

# Whenever we have data containing item names, then load that data using 
# read.transactions(file="path",format="basket",sep=",")
# use this to form association rules 


#groceries<-read.transactions(file.choose(),format="basket") 
# already run this line in beginning. 
inspect(groceries[1:10])
class(groceries)


rules_A<-apriori(groceries,parameter = list(support=0.002,confidence=0.05,minlen=3))
rules_A  ### tell us total no. of rules
inspect(rules_A[1:10])  

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

rules_B<-apriori(groceries,parameter = list(support=0.003,confidence=0.06,minlen=2))
rules_B
inspect(rules_B[1:100])


plot(rules_B,method = "scatterplot", jitter=0)
plot(rules_B,method = "grouped")
plot(rules_B,method = "graph")
plot(rules_B,method = "matrix3D")## for this method is best to interpret which  
## item we have to put right side and left side.

head(quality(rules_B)) 

inspect(head(sort(rules_B, by="lift"))) ## sorted out best six rule



############################# Model-3 ###########################################
rules_C<-apriori(groceries,parameter = list(support=0.004,confidence=0.08,minlen=2))
rules_C
inspect(rules_C[1:100])

plot(rules_C,method = "scatterplot", jitter=0)
plot(rules_C,method = "grouped")
plot(rules_C,method = "graph") 
plot(rules_C,method = "matrix3D") ## for this method is best to interpret which  
## item we have to put right side and left side.


head(quality(rules_C))
### sorted the rules by everthing like, support, confidence, lift etc.

inspect(head(sort(rules_B, by="lift"))) ## sorted out best six rule

################################# Thank you #############################################
