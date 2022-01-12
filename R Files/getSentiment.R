# polarity_sentiAnalysis()          Finds out the sentiment of a sentence based on keywords
# Description                       Used to compare different dictionaries available and reach at a result most 
#                                   commonly accepted by dictionaries.
# Usage                             polarity_sentiAnalysis <- function(start_date, end_date, conn, reviews)
# 
# 
# start_date      Specifies the start date for the range of time the posts have to be processed for sentiment.
#                 Useful in case the user wants to compare sentiments on a weekly or monthly basis
# end_date        Specifies the end date for the range of time the posts have to be processed for sentiment.
#                 Useful in case the user wants to compare sentiments on a weekly or monthly basis
# conn            the name of the review file from which the data are to be read. Provides the .csv file which 
#                 contains the messages for which the sentiments are to be extracted
# reviews         TRUE if the file to be processed is Reviews of a particular page. FALSE in case the file to be
#                 processed is either page owner post or visitors post or comment
#
#install.packages("SentimentAnalysis", "tm", "rmngb")

# Load the packages
library(rmngb)
library(SentimentAnalysis)
library(tm)

polarity_sentiAnalysis <- function(message)
{
  
  #1. Creating the Corpus
  myCorpus <- Corpus(VectorSource(message))
  
  #2. Cleaning
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  #2.01 Remove 'at people' entities @ - define the custom function/text transformation
  removeTags <- function(x) gsub("@\\w+","", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeTags))
  
  #2.02 Remove URL - define the custom function/text transformation
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  
  #2.03 Remove email - define the custom function/text transformation
  removeEmail <- function(x) gsub('\\$+@\\$+',"", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeEmail))
  
  #2.04 Remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  
  #2.05 Remove numbers
  myCorpus <-  tm_map(myCorpus, removeNumbers)
  
  #2.06 Define the custom transformation to remove special characters
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  myCorpus <- tm_map(myCorpus, toSpace, "@") # Remove @
  myCorpus <- tm_map(myCorpus, toSpace, "/") # Remove /
  myCorpus <- tm_map(myCorpus, toSpace, "\\|") # Remove |
  
  #2.07 Remove whitespace again since we replaced the speical character with a whitespace
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  #2.08 Convert to lower case
  myCorpus <- tm_map(myCorpus, tolower)
  
  #2.09 Remove stop words
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("SMART"))
  
  #3. Term Frequencies
  #Build the Document Term Matrix
  dtm <- DocumentTermMatrix(myCorpus)
  
  # Perform sentiment analysis and store in dataframe sentimentanalysis
  sentimentanalysis <- analyzeSentiment(dtm, language = "english", aggregate = NULL
                                        ,removeStopwords = TRUE, stemming = FALSE)
  
  # Change the numerical values of sentiment into levels (Postive, Neutral and Negative)
  pol_sentiAnalysis <- convertToDirection(sentimentanalysis)
  for(i in 1:1)
  {
    
    # Initialize counters for the 3 levels and NA fields
    pos <- makeCounter()
    neg <- makeCounter()
    neut = makeCounter()
    na = makeCounter()
    
    # Choose the final sentiment based on the maximum occurring sentiment derived from different dictionaries.
    if(is.na(pol_sentiAnalysis[i, 2])) {
      na(increment)
    } else if(pol_sentiAnalysis[i, 2] == "neutral") {
      neut(increment)
    } else if(pol_sentiAnalysis[i, 2] == "negative") {
      neg(increment)
    } else if(pol_sentiAnalysis[i, 2] == "positive") {
      pos(increment) }
    if(is.na(pol_sentiAnalysis[i, 5])) {
      na(increment)
    } else if(pol_sentiAnalysis[i, 5] == "neutral") {
      neut(increment)
    } else if(pol_sentiAnalysis[i, 5] == "negative") {
      neg(increment)
    } else if(pol_sentiAnalysis[i, 5] == "positive") {
      pos(increment) }
    if(is.na(pol_sentiAnalysis[i, 8])) {
      na(increment)
    } else if(pol_sentiAnalysis[i, 8] == "neutral") {
      neut(increment)
    } else if(pol_sentiAnalysis[i, 8] == "negative") {
      neg(increment)
    } else if(pol_sentiAnalysis[i, 8] == "positive") {
      pos(increment) }
    if(is.na(pol_sentiAnalysis[i, 12])) {
      na(increment)
    } else if(pol_sentiAnalysis[i, 12] == "neutral") {
      neut(increment)
    } else if(pol_sentiAnalysis[i, 12] == "negative") { 
      neg(increment)
    } else if(pol_sentiAnalysis[i, 12] == "positive") {
      pos(increment) }
    if(na() > pos() & na() > neg() & na() > neut()) {
      pol_sentiAnalysis$finalSentiment[i] = "NA"
    } else if(neg() > pos() & neg() > neut()) {
      pol_sentiAnalysis$finalSentiment[i] = "negative"
    } else if(pos() > neg() & pos() > neut()) {
      pol_sentiAnalysis$finalSentiment[i] = "positive"
    } else if(neg() == neut()) {
      pol_sentiAnalysis$finalSentiment[i] = "negative"
    } else if(pos() == neut()) {
      pol_sentiAnalysis$finalSentiment[i] = "positive"
    } else {
      pol_sentiAnalysis$finalSentiment[i] = "neutral"
    }
  }
  
  # Store the sentiment in a separate dataframe along with the message for analysis
  sentimentanalysis_df <- data.frame(FinalSentiment = pol_sentiAnalysis$finalSentiment, stringsAsFactors = FALSE)
  
  return(sentimentanalysis_df)
}

#senti_df <- polarity_sentiAnalysis("I always prefer Kolkata Police to any police I have seen during my visit to almost all the states in India and Bangladesh. Being Kolkatan I feel proud for them. Keep it on sirs.")


