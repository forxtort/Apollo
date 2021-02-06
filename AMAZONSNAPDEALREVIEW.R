#For Text Mining assignment

#TWO:
#1) Extract reviews of any product from ecommerce website like snapdeal and amazon
#2) Perform sentimental analysis

### Packages required for ###
#install.packages("rvest")
#install.packages("XML")
library(rvest)
library(XML)
library(magrittr)


######################## Amazon Reviews #############################
aurl <- "https://www.amazon.in/Apple-iPhone-11-64GB-Product/product-reviews/B07XVKG5XV/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"iphone.txt",row.names = F)

getwd()
####################################################################
x = readLines(file.choose())
library(tm)
x <- stemDocument(x)
View(x)

################# Corpus ###################
## Corpus is collection of documents##
x <- Corpus(VectorSource(x))

inspect(x[1])
inspect(x[300])

#Data Cleansing
x1 <- tm_map(x, tolower)

x1 <- tm_map(x1, removePunctuation)
inspect(x1[15])

x1 <- tm_map(x1, removeNumbers)

x1 <- tm_map(x1, removeWords, stopwords('english'))

# Remove URL's from corpus
# removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
# x1 <- tm_map(x1, content_transformer(removeURL))
# inspect(x1[1])

#striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[15])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm)  ### t stands for transform

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

tdm[90:100, 1:20]
# View(tdm)

inspect(x[3])


# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 25)
w_sub

barplot(w_sub, las=2, col = rainbow(30))
## From barplot we can see that people talk about maximum time phone, second 
## heat, product, amzaon. etc. 

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

#installed.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
#windows()

wordcloud2(w1, size=0.5, shape='circle')
?wordcloud2
### wordcloud in circle
wordcloud2(w1, size=0.5, shape = 'triangle')

####

######################### emotion mining ###############
## packages required for ##
#install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)

txt = readLines(file.choose())
x <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(x)
head(s)
get_nrc_sentiment('unfriendly')

# Bar plot for emotion mining

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')
####### we can check that proportion of positive and negative words that used in "review".

####################Extract reviews from snapdeal #####################################

surl_1 <- "https://www.snapdeal.com/product/voltas-15-ton-5-star/754375241/reviews?page=2&sortBy=HELPFUL"

snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"fridge.txt",row.names = FALSE)
getwd()

############################# Lets explore the data ####################################
x = readLines(file.choose())
library(tm)
x <- stemDocument(x)
View(x)

################# Corpus ###################
## Corpus is collection of documents##
x <- Corpus(VectorSource(x))

inspect(x[1])
inspect(x[300])

#Data Cleansing
x1 <- tm_map(x, tolower)

x1 <- tm_map(x1, removePunctuation)
inspect(x1[15])

x1 <- tm_map(x1, removeNumbers)

x1 <- tm_map(x1, removeWords, stopwords('english'))

# Remove URL's from corpus
# removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
# x1 <- tm_map(x1, content_transformer(removeURL))
# inspect(x1[1])

#striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[15])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm)  ### t stands for transform

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

tdm[90:100, 1:20]
# View(tdm)

inspect(x[3])


# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 25)
w_sub

barplot(w_sub, las=2, col = rainbow(30))
## From barplot we can see that people talk about maximum time phone, second 
## heat, product, amzaon. etc. 

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

#installed.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
#windows()

wordcloud2(w1, size=0.5, shape='circle')
?wordcloud2
### wordcloud in circle
wordcloud2(w1, size=0.5, shape = 'triangle')

####

######################### emotion mining ###############
## packages required for ##
#install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)

txt = readLines(file.choose())
x <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(x)
head(s)
get_nrc_sentiment('unfriendly')

# Bar plot for emotion mining

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')
####### we can check that proportion of positive, negative, others  words that used in "review".
