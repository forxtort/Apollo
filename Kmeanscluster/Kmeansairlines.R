#Problem_Statement for Airlines - K-Means Clustering
#Perform clustering (Both hierarchical and K means clustering) for the airlines data
#to obtain optimum number of clusters.Draw the inferences from the clusters obtained.

#Data Description:

#The file EastWestAirlinescontains information on passengers who belong to an 
#airlineâ???Ts frequent flier program. For each passenger the data include 
#information on their mileage history and on different ways they accrued or spent
#miles in the last year. The goal is to try to identify clusters of passengers 
#that have similar characteristics for the purpose of targeting different 
#segments for different types of mileage offers

#ID --Unique ID

#Balance--Number of miles eligible for award travel

#Qual_mile--Number of miles counted as qualifying for Topflight status

#cc1_miles -- Number of miles earned with freq. flyer credit card in the past 12 months:
#cc2_miles -- Number of miles earned with Rewards credit card in the past 12 months:
#cc3_miles -- Number of miles earned with Small Business credit card in the past 12 months:

#1 = under 5,000
#2 = 5,000 - 10,000
#3 = 10,001 - 25,000
#4 = 25,001 - 50,000
#5 = over 50,000

#Bonus_miles--Number of miles earned from non-flight bonus transactions in the past 12 months

#Bonus_trans--Number of non-flight bonus transactions in the past 12 months

#Flight_miles_12mo--Number of flight miles in the past 12 months

#Flight_trans_12--Number of flight transactions in the past 12 months

#Days_since_enrolled--Number of days since enrolled in flier program

#Award--whether that person had award flight (free flight) or not

library(readxl)

input <- read_excel(file.choose())
View(input)
mydata<- input[,c(2:12)]
View(mydata)
summary(mydata)
str(mydata)
############################Let explore the data###############################

#################################Standard deviation############################

sd(mydata$Balance)
sd(mydata$Qual_miles)
sd(mydata$cc1_miles)
sd(mydata$cc2_miles)
sd(mydata$cc3_miles)
sd(mydata$Bonus_miles)
sd(mydata$Bonus_trans)
sd(mydata$Flight_miles_12mo)
sd(mydata$Flight_trans_12)
sd(mydata$Days_since_enroll)
sd(mydata$Award)

#####################################Variance #####################################
library(moments)

###############3rd, 4th Business Moment ################

############Skewness ##############
skewness(mydata)

############kurtosis ##############
kurtosis(mydata)

############ Histogram ############

hist(mydata$Balance)
hist(mydata$Qual_miles)
hist(mydata$cc1_miles)
hist(mydata$cc2_miles)
hist(mydata$cc3_miles)
hist(mydata$Bonus_miles)
hist(mydata$Bonus_trans)
hist(mydata$Flight_miles_12mo)
hist(mydata$Flight_trans_12)
hist(mydata$Days_since_enroll)
hist(mydata$Award)
################
pairs(mydata) ### plot the data set in one page

###############Barplot #######################
barplot(mydata$Balance)
barplot(mydata$Qual_miles)
barplot(mydata$cc1_miles)
barplot(mydata$cc2_miles)
barplot(mydata$cc3_miles)
barplot(mydata$Bonus_miles)
barplot(mydata$Bonus_trans)
barplot(mydata$Flight_miles_12mo)
barplot(mydata$Flight_trans_12)
barplot(mydata$Days_since_enroll)
barplot(mydata$Award)

###########Boxplot###############
boxplot(mydata)
sum(is.na(mydata))
attach(mydata)

#install.packages("plyr")
library(plyr)
km <- kmeans(mydata,6) #kmeans clustering
str(km)

#install.packages("animation")
library(animation)
km <- kmeans.ani(mydata, 6)

#Normalizing data
normalized_data<-scale(mydata) 
View(normalized_data)

# Distance matrix
d <- dist(normalized_data, method = "euclidean") 
d

#Elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# Model Building
fit <- kmeans(normalized_data, 6) # 6 cluster solution
str(fit)
table(fit$cluster)
Airlines<- data.frame(input, fit$cluster) # append cluster membership
View(Airlines)
library(data.table)
setcolorder(Airlines, neworder = c("fit.cluster"))
View(Airlines)
aggregate(mydata, by=list(fit$cluster), FUN=mean)
