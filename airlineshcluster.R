#Problem_Statement for Airlines - Hierarchical Clustering
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

#install.packages("readxl")
library(readxl)

###############Importing the file#########
input <- read_excel(file.choose())
View(input)
mydata<- input[,c(2:12)] ### first column is just like a  serial no. 

View(mydata)
summary(mydata)
str(mydata)  ##### Structure of the data frame
############################ Lets Explore the Data set ######################

###### standard Deviation ###
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

######## Variance ###########
var(mydata)

########### 3rd and 4th Business Moment #############################

#install.packages("moments")
library(moments)

##########Skewness#######

skewness(mydata)

#########Kurtosis ########

kurtosis(mydata)

#####Histogram ###########
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

### plot ######
pairs(mydata)

########## Bar plot #######

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

## Box plot ###
boxplot(mydata)
sum(is.na(mydata))
attach(mydata)

########################### Model Building formation ###################

#Normalizing data
normalized_data<-scale(mydata)  ### Normalizing the data by using scale function 
View(normalized_data)

# Distance matrix
## store the normailized data into distance matrix by using "Euclidean Method"##
d <- dist(normalized_data, method = "euclidean") 
#d

# Model Building
fit <- hclust(d, method="complete")

# Display dendrogram
plot(fit) ## display of dendogram
plot(fit, hang=-1)
groups <- cutree(fit, k=6) # cut tree into 6 clusters

rect.hclust(fit, k=6, border="red")

Airlines<-as.matrix(groups)
table(Airlines)

final <- data.frame(input, Airlines)
#final <- cbind(input, Airlines)
View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(input[,-1],by=list(final$Airlines),mean)
################################### "Model-2" ##################################
## using "single linkage" and distance method is "manhattan".

#Normalizing data
normalized_data<-scale(mydata)  ### Normalizing the data by using scale function 
View(normalized_data)

# Distance matrix
## store the normailized data into distance matrix by using "Manhattan Method"##
d <- dist(normalized_data, method = "manhattan") 
#d

# Model Building
fit <- hclust(d, method="single")

# Display dendrogram
plot(fit) ## display of dendogram
plot(fit, hang=-1)
groups <- cutree(fit, k=6) # cut tree into 6 clusters

rect.hclust(fit, k=6, border="red")

Airlines<-as.matrix(groups)
table(Airlines)

final <- data.frame(input, Airlines)
#final <- cbind(input, Airlines)
View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(input[,-1],by=list(final$Airlines),mean)

#################################### Model-3 ##########################################

## using "average linkage" and distance method is "minkowski".

#Normalizing data
normalized_data<-scale(mydata)  ### Normalizing the data by using scale function 
View(normalized_data)

# Distance matrix
## store the normailized data into distance matrix by using "Minkowski"##
d <- dist(normalized_data, method = "minkowski") 
#d

# Model Building
fit <- hclust(d, method="average")

# Display dendrogram
plot(fit) ## display of dendogram
plot(fit, hang=-1)
groups <- cutree(fit, k=6) # cut tree into 6 clusters

rect.hclust(fit, k=6, border="red")

Airlines<-as.matrix(groups)
table(Airlines)

final <- data.frame(input, Airlines)
#final <- cbind(input, Airlines)
View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(input[,-1],by=list(final$Airlines),mean)

