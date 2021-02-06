#PCA - Wine Data set

#Perform Principal component analysis and perform clustering using first 
#3 principal component scores (both heirarchial and k mean clustering(scree plot or
#elbow curve) and obtain optimum number of clusters and check whether we have 
#obtained same number of clusters with the original data 
#(class column we have ignored at the begining who shows it has 3 clusters)df

#install.packages("readr")
library(readr)

mydata <- read.csv(file.choose())
View(mydata)
summary(mydata)
str(mydata)
#############Lets Explore the "wine data set" ################################

##############Variance ###############
var(mydata)
#############Standard Deviance########
sd(mydata$Type)
sd(mydata$Alcohol)
sd(mydata$Malic)
sd(mydata$Ash)
sd(mydata$Alcalinity)
sd(mydata$Magnesium)
sd(mydata$Phenols)
sd(mydata$Flavanoids)
sd(mydata$Nonflavanoids)
sd(mydata$Proanthocyanins)
sd(mydata$Color)
sd(mydata$Hue)
sd(mydata$Dilution)
sd(mydata$Proline)
##############3rd, 4th Business Moment#############

#install.packages("moments")
library(moments)
########skewness ##########
skewness(mydata)

########kurtosis############
kurtosis(mydata)

###########Histogram######
hist(mydata$Type)
hist(mydata$Alcohol)
hist(mydata$Malic)
hist(mydata$Ash)
hist(mydata$Alcalinity)
hist(mydata$Magnesium)
hist(mydata$Phenols)
hist(mydata$Flavanoids)
hist(mydata$Nonflavanoids)
hist(mydata$Proanthocyanins)
hist(mydata$Color)
hist(mydata$Hue)
hist(mydata$Dilution)
hist(mydata$Proline)

#####plot######
pairs(mydata) ## Plot in one page

################bar plot ###########
barplot(mydata$Type)
barplot(mydata$Alcohol)
barplot(mydata$Malic)
barplot(mydata$Ash)
barplot(mydata$Alcalinity)
barplot(mydata$Magnesium)
barplot(mydata$Phenols)
barplot(mydata$Flavanoids)
barplot(mydata$Nonflavanoids)
barplot(mydata$Proanthocyanins)
barplot(mydata$Color)
barplot(mydata$Hue)
barplot(mydata$Dilution)
barplot(mydata$Proline)

######boxplot ##########
boxplot(mydata) ####used to check the outliers
sum(is.na(mydata))
attach(mydata)

cor(mydata)

############################# Model Building ##################################
PCA<-princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)
summary(PCA)
#str(PCA)

plot(PCA) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(PCA)
PCA$loadings
PCA$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# Considering top 3 principal component scores and binding them with mydata
PCA_final1<-cbind(mydata,PCA$scores[,1:3])
View(PCA_final1)

############################### Hierarchical Clustering ############################

# Preparing data for clustering (considering only PCA scores;
#as they represent the entire data)
clus_data<-PCA_final1[,15:17]

# Normalizing the data 
normalized_data<-scale(clus_data) # Scale function is used to normalize data
View(normalized_data)

# Distance matrix
d <- dist(normalized_data, method = "manhattan") 
#d

# Model Building
fit <- hclust(d, method="complete")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

Wine_hierarchical<-as.matrix(groups)
table(Wine_hierarchical)

final <- data.frame(clus_data, Wine_hierarchical)
#final <- cbind(clus_data, Wine_hierarchical)
View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(mydata[,-1],by=list(final$Wine_hierarchical),mean)

################################K-Means Clustering##########################
#install.packages("plyr")
library(plyr)
km <- kmeans(clus_data,5) #kmeans clustering
str(km)

#install.packages("animation")
library(animation)
km <- kmeans.ani(clus_data, 5)

#Elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:3) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# Selecting K for kmeans clustering using kselection
#install.packages("kselection")
library(kselection)
k <- kselection(clus_data, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k
 ## kslection suggest 3 cluster. 
## After k selection we wil go for 3 cluster in final model 

# Model Building
fit <- kmeans(normalized_data, 3) # 3 cluster solution
str(fit)
table(fit$cluster)
Wine_kmeans<- data.frame(clus_data, fit$cluster) # append cluster membership
View(Wine_kmeans)
library(data.table)
setcolorder(Wine_kmeans, neworder = c("fit.cluster"))
View(Wine_kmeans)
aggregate(clus_data, by=list(fit$cluster), FUN=mean)

## above ananlysis  based on only PCA components, we make a model for Hclust, K-means
## only considering the PCA compomnets. 

############################# Let Begin a model without PCA forming ################
############################ Hirarcheal clustering #################################
# Normalizing the data 
normalized_data<-scale(mydata) # Scale function is used to normalize data
View(mydata)
range(mydata)

#Distance matrix
d <- dist(mydata, method = "euclidean") 
#d


fit <- hclust(d, method="complete")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=3) # cut tree into 3 clusters

rect.hclust(fit, k=3, border="red")

WINE_clust<-as.matrix(groups)
table(WINE_clust)

final <- data.frame(mydata, WINE_clust)
#final <- cbind(input, Crime_rate)
View(final)


write.csv(final, file="final.csv",row.names = F)

aggregate(mydata[,-1],by=list(final$WINE_clust),mean)
## We can compare here between aggregate of PCA components(hclust) model vs 
##aggregate of hclust without pca analysis. 

##################### Lets do the same thing with K- Means ##########################

#install.packages("plyr")
library(plyr)
km <- kmeans(mydata,6) #kmeans clustering
str(km)

#install.packages("animation")
library(animation) ## you can see how cluster from in plots windows right side.
km <- kmeans.ani(mydata, 6)

#Normalizing data
normalized_data<-scale(mydata) 
View(normalized_data)

# Distance matrix
d <- dist(normalized_data, method = "euclidean") 
#d

#Elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")
## Screen plot some times we can confuse somebody will say k =6,7, 8. will values 
## between them . 
## so best is to do both methods otherwise go for kselection method. 

# Selecting K for kmeans clustering using kselection
#install.packages("kselection")
library(kselection)
k <- kselection(mydata, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k
### K selection gives the value of k is 2. 
## quite strange as comparison with scree plot.
## In PCA components we get k vlaue is 3 by kselection and without PCA we get 2.


######################### Model Building ######################################
fit <- kmeans(normalized_data, 6) # 6 cluster solution
str(fit)
table(fit$cluster)
Wine_Kmeans_clust<- data.frame(mydata, fit$cluster) # append cluster membership
View(Wine_Kmeans_clust)

library(data.table)
setcolorder(Wine_Kmeans_clust, neworder = c("fit.cluster"))
View(Wine_Kmeans_clust)
aggregate(mydata, by=list(fit$cluster), FUN=mean)

### Here we can compare between PCA-kmeans clust(where we used PCA components)
### and Kmeans whole data of wine. 

###################################### Thank you ########################################
