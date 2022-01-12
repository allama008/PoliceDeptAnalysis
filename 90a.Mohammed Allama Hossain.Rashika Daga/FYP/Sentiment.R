library(sentimentr)
library(plyr)
library(dplyr)
library(syuzhet)
library(RCurl)
library(ggplot2)
library(httr)
library(wordcloud)
library(stringr)
library(tm)

install.packages(c("cluster", "SnowballC", "fpc", "cluster", "Rgraphviz", "igraph"))
docs <- Corpus(DirSource("."))
summary(docs)
summary(docs[1:5])
names(docs)
length(docs)
inspect(docs[[1]])
meta(docs[[1]])
content(docs[[1]])

#3. Data Preprocessing

# To check the list of pre-defined text transformations
getTransformations()

# Remove whitespace
docs <- tm_map(docs, stripWhitespace)
inspect(docs[[1]])

#Remove URL - define the custom function/text transformation
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

# Remove email - define the custom function/text transformation
removeEmail <- function(x) gsub('\\$+@\\$+',"", x)
docs <- tm_map(docs, content_transformer(removeEmail))

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#Remove numbers
docs <-  tm_map(docs, removeNumbers)

# Define the custom transformation to remove special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@") # Remove @
docs <- tm_map(docs, toSpace, "/") # Remove /
docs <- tm_map(docs, toSpace, "\\|") # Remove |

#Remove whitespace again since we replaced the speical character with a whitespace
docs <- tm_map(docs, stripWhitespace)
inspect(docs[[1]])

#Convert to lower case
docs <- tm_map(docs, tolower)

#Remove stop words
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))
docs <- tm_map(docs, removeWords, c("traffic", "kolkata", "police", "polic"))
               
#Remove whitespace again since the stop words were replaced with a space
docs <- tm_map(docs, stripWhitespace)
inspect(docs[[1]])
#Stemming
library(SnowballC)
docs <- tm_map(docs, stemDocument)

# Term Frequencies
#Build the Document Term Matrix
dtm <- DocumentTermMatrix(docs)
dtm
freq <- colSums(as.matrix(dtm)) #Term frequence in all documents; each column is a term
freqSorted <- freq[order(-freq)]
head(freqSorted, 15)

length(freq)          #Number of distince terms in all documents
m <- as.matrix(dtm)   #Convert dtm to a matrix
dim(m)                #Display number of terms and number of documents
View(m)               #Preview the matrix
sum(m==0)             #Number of zero entries in a matrix
sum(m>=0)             #Number of elements in a matrix
sum(m==0)*100/sum(m>=0) # % of empty cells in a matrix sparsity

#List the frequent terms
findFreqTerms(dtm, lowfreq = 25) #Find the terms that appear 25 or more times in all documents
findMostFreqTerms(dtm, 5)        #Find the 5 most frequent terms in each document
termFreq(docs[[1]])              #Terms frequence for the first document
# Terms frequency for the first document. The term must contain no more than 4 character
termFreq(docs[[1]], control = list(wordLengths = c(-Inf, 4)))
# Terms frequency for the first document. The term must contain 8-10 characters
termFreq(docs[[1]], control = list(wordLengths = c(8, 10)))

#Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.2) #The matrix cannot be more than 80% empty (1-0.2)
dtms

#Several ways to list the document names and the terms
dtms$dimnames #Returns terms and document names
dimnames(dtms)  #Also returns terms and document names

#List only the terms
dtms$dimnames$Terms

#Word Association
# find the terms associates with correlation>=0.75
findAssocs(dtm, c("work"), corlimit = 0.25)

#Correlation plot
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library("Rgraphviz")
#Terms that appear at least 40 times with correlation>=0.2
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 120), corThreshold=0.5, weighting = T)
#Terms that appear at least 35 times with correlation>=0.3
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 35), corThreshold=0.3, weighting = T)
#Terms that appear at least 40 times with correlation>=0.4
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 40), corThreshold=0.4, weighting = T)               

#Word Frequencies plot
library(ggplot2)
wf <- data.frame(word = names(freq), freq=freq)
p <- ggplot(subset(wf, freq>50), aes(word, freq))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p <- p + xlab("Terms") + ylab("Count") + coord_flip()
p

# Build a word cloud
library(wordcloud)

wordcloud(names(freq), freq, min.freq = 5)
#dtms <- removeSparseTerms(dtm, 0.3) #Prepare the data (max 70% empty space)
#freq <- colSums(as.matrix(dtms))    # Find word frequencies
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq = 10, max.words = 400, rot.per = 0.2, colors = dark2)

# k-means clustering. Use k=4
dtms <- removeSparseTerms(dtm, 0.70) #Terms must appear in at least 55% of documents
d <- dist(t(dtms), method = "euclidian")
kfit <- kmeans(d, 4)
kfit
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color = T, shade = T, labels = 2, lines = 0)
kfit$size     #Number of instances in each cluster
kfit$withinss #Sum of squared distances within a cluster
kfit$betweenss
kfit$totss

# Cluster dendogram
library(cluster)
library(fpc)

dtms <- removeSparseTerms(dtm, 0.95)
d <- dist(t(dtms), method = "euclidian")
fit <- hclust(d = d, method = "ward.D2")
plot(fit, hang=-1)
