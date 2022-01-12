library(tm)
library(wordcloud)
library(readtext)
library(sentiment)
library(readtext)
library(ggplot2)
vis_pos <- read.csv("visitors_post.csv", header = TRUE)

some_comments <- lapply(posts$message, as.character)
some_txt <- unlist(some_comments)

some_txt1 <- gsub("http[^[:space:]]*", "", some_txt)
some_txt1 <- gsub('\\$+@\\$+',"", some_txt1)
some_txt1 = gsub("[[:punct:]]", "", some_txt1)
some_txt1 = gsub("[[:digit:]]", "", some_txt1)
some_txt1 = gsub("http\\w+", "", some_txt1)
some_txt1 = gsub("[ \t]{2,}", "", some_txt1)
some_txt1 = gsub("^\\s+|\\s+$", "", some_txt1)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
some_txt1 = sapply(some_txt1, try.error)
some_txt1 = some_txt1[!is.na(some_txt1)]
names(some_txt1) = NULL

#6. Polarity
class_emo = classify_emotion(some_txt1, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(some_txt1, algorithm="bayes")
polarity = class_pol[,4]


sent_df = data.frame(text=some_txt1, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")

# The corpus statement actually converts it into a friendly structure 
# of texts. So a corpus_vis is basically a structured set of texts
corpus_vis <- Corpus(VectorSource(vis_pos$message))

# To check the list of pre-defined text transformations
getTransformations()

# Remove whitespace
corpus_vis <- tm_map(corpus_vis, stripWhitespace)


#Remove URL - define the custom function/text transformation
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus_vis <- tm_map(corpus_vis, content_transformer(removeURL))

# Remove email - define the custom function/text transformation
removeEmail <- function(x) gsub('\\$+@\\$+',"", x)
corpus_vis <- tm_map(corpus_vis, content_transformer(removeEmail))

#Remove punctuation
corpus_vis <- tm_map(corpus_vis, removePunctuation)

#Remove numbers
corpus_vis <-  tm_map(corpus_vis, removeNumbers)

# Define the custom transformation to remove special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus_vis <- tm_map(corpus_vis, toSpace, "@") # Remove @
corpus_vis <- tm_map(corpus_vis, toSpace, "/") # Remove /
corpus_vis <- tm_map(corpus_vis, toSpace, "\\|") # Remove |

#Remove whitespace again since we replaced the speical character with a whitespace
corpus_vis <- tm_map(corpus_vis, stripWhitespace)


#Convert to lower case
corpus_vis <- tm_map(corpus_vis, tolower)

#Remove stop words
corpus_vis <- tm_map(corpus_vis, removeWords, stopwords("english"))
corpus_vis <- tm_map(corpus_vis, removeWords, stopwords("SMART"))
corpus_vis <- tm_map(corpus_vis, removeWords, c("traffic", "kolkata", "police", "polic", "sir"))

#Remove whitespace again since the stop words were replaced with a space
corpus_vis <- tm_map(corpus_vis, stripWhitespace)

#Stemming
library(SnowballC)
corpus_vis <- tm_map(corpus_vis, stemDocument)

# Term Frequencies
#Build the Document Term Matrix
dtm_vis <- DocumentTermMatrix(corpus_vis)
dtm_vis
freq_vis <- colSums(as.matrix(dtm_vis)) #Term frequence in all documents; each column is a term

wordcloud(names(freq_vis), freq_vis, min.freq = 5)
#dtms <- removeSparseTerms(dtm, 0.3) #Prepare the data (max 70% empty space)
#freq <- colSums(as.matrix(dtms))    # Find word frequencies
par(cex=1.0)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq_vis), freq_vis, min.freq = 40, max.words = 400, rot.per = 0.2, colors = dark2)

#Word Frequencies plot
library(ggplot2)
wf_vis <- data.frame(word = names(freq_vis), freq=freq_vis)
p_vis <- ggplot(subset(wf_vis, freq>150), aes(word, freq))
p_vis <- p_vis + geom_bar(stat = "identity")
p_vis <- p_vis + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_vis <- p_vis + xlab("Terms") + ylab("Count") + coord_flip()
p_vis

#Word Association
# find the terms associates with correlation>=0.75
findAssocs(dtm_vis, c("fine"), corlimit = 0.25)

#Correlation plot
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library("Rgraphviz")
#Terms that appear at least 40 times with correlation>=0.2
par(cex = 0.2)
plot(dtm_vis, terms = findFreqTerms(dtm_vis, lowfreq = 200), corThreshold=0.25, weighting = T)
par(cex = 1.0)
#Terms that appear at least 35 times with correlation>=0.3
plot(dtm_vis, terms = findFreqTerms(dtm, lowfreq = 35), corThreshold=0.3, weighting = T)
#Terms that appear at least 40 times with correlation>=0.4
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 40), corThreshold=0.4, weighting = T)  
# Cluster dendogram
library(cluster)
library(fpc)

dtms_vis <- removeSparseTerms(dtm_vis, 0.90)
d_vis <- dist(t(dtms_vis), method = "euclidian")
fit_vis <- hclust(d = d_vis, method = "ward.D2")
plot(fit_vis, hang=-1)
