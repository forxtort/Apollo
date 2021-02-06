
############# IMDB reviews Extraction ################

 avatar<-NULL
 url2 <- "https://www.imdb.com/title/tt0499549/reviews?ref_=tt_urv"
 for(i in 1:10){
  url<-read_html(as.character(paste(url2,i,sep="")))
   ava<-url %>%
     html_nodes(".show-more__control") %>%
     html_text() 
   avatar<-c(avatar,ava)
 }
 write.table(avatar,file="avatar.txt")
getwd()



################################### Packages required for ##################################
#install.packages("rvest")
#install.packages("XML")
library(rvest)
library(XML)
library(magrittr)


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
#?wordcloud2
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
