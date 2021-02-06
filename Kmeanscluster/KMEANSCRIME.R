#K-Means Clustering for Crime Data
#Perform Clustering for the crime data and identify the number of clusters formed 
#and draw inferences.

#Data Description:
#Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States


library(readr)
input <- read.csv(file.choose())
View(input)
mydata<- input[,c(2:5)]
View(mydata)
summary(mydata)
str(mydata)
############################Lets Explore the data###################

##########Variance#################
var(mydata)

#########Standard Deviation ######
sd(mydata$Murder)
sd(mydata$Assault)
sd(mydata$UrbanPop)
sd(mydata$Rape)

###############3rd and 4th Business Moment ########################
#install.packages("moments")
library(moments)
#########Skewness #################
skewness(mydata)
########kurtosis #################
kurtosis(mydata)

########Histogram ################
hist(mydata$Murder)
hist(mydata$Assault)
hist(mydata$UrbanPop)
hist(mydata$Rape)
########### plot
pairs(mydata) #plot in one page####

###########barplot ########
barplot(mydata$Murder)
barplot(mydata$Assault)
barplot(mydata$UrbanPop)
barplot(mydata$Rape)

########## Boxplot #########
boxplot(mydata)
sum(is.na(mydata))
attach(mydata)

######### Model Exploring ################
#install.packages("plyr")
library(plyr)
km <- kmeans(mydata,5) #kmeans clustering
str(km)

#install.packages("animation")
library(animation)
km <- kmeans.ani(mydata, 5)

#Normalizing data
normalized_data<-scale(mydata) 
View(normalized_data)

#Elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:5) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# Selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
k <- kselection(mydata, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k



# Model Building
fit <- kmeans(normalized_data, 5) # 5 cluster solution
str(fit)
table(fit$cluster)
Crime_rate<- data.frame(input, fit$cluster) # append cluster membership
View(Crime_rate)
library(data.table)
setcolorder(Crime_rate, neworder = c("fit.cluster"))
View(Crime_rate)
aggregate(mydata, by=list(fit$cluster), FUN=mean)
