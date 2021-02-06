#Hierarchical Clustering for Crime Data
#Perform Clustering for the crime data and identify the number of clusters formed 
#and draw inferences.

#Data Description:
#Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States

#install.packages("readr")
library(readr)

input <- read.csv(file.choose())
View(input)
# Normalaizing the data by using the scale function 
mydata<-scale(Uni[,2:5])

View(mydata)
summary(mydata)
str(mydata)

################Lets Explore the Model###################################

##########Standard Deviation ###########

sd(mydata$Murder)
sd(mydata$Assault)
sd(mydata$UrbanPop)
sd(mydata$Rape)
#########Variance ###########################
var(mydata)

############3rd and 4th Business Moment#######
#install.packages("moments")
library(moments)

############Skewness###############
skewness(mydata)
###########Kurtosis################
kurtosis(mydata)

##########Histogram##############

hist(mydata$Murder)
hist(mydata$Assault)
hist(mydata$UrbanPop)
hist(mydata$Rape)
pairs(mydata) ######## Plot in one graph(one page) #######

############## Barplot ###########################

barplot(mydata$Murder)
barplot(mydata$Assault)
barplot(mydata$UrbanPop)
barplot(mydata$Rape)
boxplot(mydata)

sum(is.na(mydata)) ###first count the na values than sum of that
attach(mydata) 

#################################### Model Building Formation ##########################

#

######################### Model Building by "Complete Method" ##########################
Distance matrix
d <- dist(mydata, method = "euclidean") 
d


fit <- hclust(d, method="complete")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=3) # cut tree into 5 clusters

rect.hclust(fit, k=3, border="red")

Crime_rate<-as.matrix(groups)
table(Crime_rate)

final <- data.frame(input, Crime_rate)
#final <- cbind(input, Crime_rate)
View(final)


write.csv(final, file="final.csv",row.names = F)

aggregate(input[,-1],by=list(final$Crime_rate),mean)
######## after aggregate function it aggregate the all group and shows us the 
## In first group has maximum values in Murder, assault, unbranpop, rape. 

################################## Different Method ##########################
## first thing we have to know that in Hirarcheal clustering  we can't determine the 
## value of k. we take , sqrt(n/2).
## above we apply "complete method" i.e determine the farthest distance between 2 points. 
## Here we are going to apply "single linkage method" i.e counts smallest distance b/w
## 2 points

################################ Lets Begin ######################################

# Distance matrix
## One more thing we have also different method to calculate the distance b/w 2 points
## like euclidean, maximum, manhattan, canberra, binary, minkowski.


######################## "Model Building by "single" linkage Method" #######################
# Distance matrix

d <- dist(mydata, method = "manhattan") ### distance count by "Manhattan" method. 
d

fit <- hclust(d, method="single")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

Crime_rate<-as.matrix(groups)
table(Crime_rate)

final <- data.frame(input, Crime_rate)
#final <- cbind(input, Crime_rate)
View(final)


write.csv(final, file="final.csv",row.names = F)

aggregate(input[,-1],by=list(final$Crime_rate),mean)


######################## "Model Building by average linkage Method" #######################
# Distance matrix

d <- dist(mydata, method = "maximum")  ## distance count by "maximum" method
d

fit <- hclust(d, method="average")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

Crime_rate<-as.matrix(groups)
table(Crime_rate)

final <- data.frame(input, Crime_rate)
#final <- cbind(input, Crime_rate)
View(final)


write.csv(final, file="final.csv",row.names = F)

aggregate(input[,-1],by=list(final$Crime_rate),mean)

