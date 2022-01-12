install.packages("tm")
library(tm)
library(wordcloud)

comm1 <- read.csv("Book1.csv", header = TRUE)

# The corpus statement actually converts it into a friendly structure 
# of texts. So a corpus is basically a structured set of texts
corpus <- Corpus(VectorSource(comm1$comment_message))

corpus[1][1]

#Text Cleaning

#Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
#Removes numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove Common English Stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#Remove Punctuations
corpus <- tm_map(corpus, removePunctuation)
#Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)

# We create a term document matrix, which is nothing but a 
# matrix which describes the frequency of the terms.

# Create TDM
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

wordcloud(d$word, d$freq, random.order = FALSE, rot.per = 0.3, scale = c(4, 1), max.words = 101, colors = brewer.pal(8, "Dark2"))
title(main = "Word Cloud - Unigram", font.main = 1, cex.main = 1.5)
