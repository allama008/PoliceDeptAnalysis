install.packages("NLP")
install.packages("openNLP")
install.packages("stringr")
install.packages("stringi")

# Load the packages
library(NLP)
library(openNLP)
library(stringr)
library(stringi)

# Load the files from which the messages/text are to be extracted
# Dictionary could be empty or it could be pre-defined with certain important words
police <- read.csv("traffic_reviews.csv", header = TRUE)
manual_dict <- read.csv("dictionary.csv", header = TRUE, stringsAsFactors = FALSE)

# Define all the dataframes to be used
# generated_dict will be used to store the list of important words
# tag contains the part of speech to aid in finding the important words
# words contains the corresponding word to the tag 
generated_dict <- data.frame(words = character(), count = integer(), stringsAsFactors = FALSE)
tag <- data.frame(words = character(), stringsAsFactors = FALSE)
words <- data.frame(words = character(), stringsAsFactors = FALSE)

# Intialize the variable. l will be used to iterate and store results in the generated_dict
l <- 1

# Running a for loop to process every row/message of the file.
for(i in 1:nrow(police))
{
  message <- police[i, 1]
  x <- as.String(message)
  
  # Before POS tagging, we need to do Sentence annotation followed by word annotation
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  
  # POS tag the words & extract the "words" from the output
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  
  # Extract the tags from the words
  tags <- sapply(POSwords$features, '[[', "POS")
  
  # Create a data frame with words and tags
  tokenizedAndTagged <- data.frame(Tokens = as.character(x[POSwords]), Tags = tags)
  
  # Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
  # In this case we only want Noun and Adjective tags (NN, JJ)
  # Note that this will also capture variations such as NNP, NNPS etc
  tokenizedAndTagged$Tags_mod = grepl("NN|JJ", tokenizedAndTagged$Tags)
  
  # Initialize a vector to store chunk indexes
  chunk = vector()

  # Iterate thru each word and assign each one to a group
  # if the word doesn’t belong to NN|JJ tags (i.e. tags_mod flag is 0) assign it to the default group (0)
  # If the ith tag is in “NN|JJ” (i.e. tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod 
  # flag is also 1; else assign it to a new group  
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  if(nrow(tokenizedAndTagged) > 1)
  {  
    for (j in 2:nrow(tokenizedAndTagged)) {
      
      if(!tokenizedAndTagged$Tags_mod[j]) {
        chunk[j] = 0
      } else if (tokenizedAndTagged$Tags_mod[j] == tokenizedAndTagged$Tags_mod[j-1]) {
        chunk[j] = chunk[j-1]
      } else {
        chunk[j] = max(chunk) + 1
      }
      
    }
  }
  
  # Split and chunk words
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  
  # Extract chunks matching pattern
  res = text_chunk[grepl("JJ-NN", names(text_chunk))]
  res = sapply(res, function(x) paste(x, collapse =  " "))
  #length(res)
  
  # Initialize the variable. k will be used to iterate through the res list
  k <- 1
  
  # The loop will be used to unlist all the noun available, and store them in the dictionary. 
  # If the dictionary already contains the word, then the count is updated else the word is inserted and 
  # the count is set to 1.
  while(k<=length(res) & length(res)!=0)
  {
    tag <- data.frame(words = unlist(stri_extract_all_words(names(res[k]))))
    words <- data.frame(words = unlist(stri_extract_all_words(res[[k]])))
    
    # For each word that is a NOUN (NN), TRUE will be stored in the list else FALSE
    trufals_response <- grepl("NN", tag$words)
    
    # Iterate through all the words present in a phrase, and either insert or update the word in the dictionary
    for(m in 1:nrow(tag))
    {  
      if(trufals_response[m])
      {
        index <- match(words[m, 1], generated_dict$words, nomatch = 0)
        if(index)
        {
          generated_dict[index, 2] <- generated_dict[index, 2] + 1       
        } else {
          generated_dict[l, 1] <- as.character(words[m, 1])
          generated_dict[l, 2] <- 1
          l <- l + 1
        }
      }
    }
    k <- k + 1
  }
}

# The dictionary will then be sorted in descending order so that words with higher counts are at the top.
generated_dict <- generated_dict[order(generated_dict$count, decreasing = TRUE), ]

# Initialize the variables. n will be used to iterate through the the words in the dictionary i.e. generated_dict
# p will be used to iterate through the final dictionary present i.e. manual_dict
# flag1 will be to enlist all words with count above a specific threshold value set
n <- 1
p <- nrow(manual_dict) + n
flag1 <- 1

# Running a loop through all the words present in the generated_dict. Basically all the distinct nouns that
# occurred in the messages.
# If the count of the words fall below a specific threshold then the flag will be set and loop terminated.
while(n <= nrow(generated_dict) & flag1)
{
  
  # If the count of words fall below threshold value (in this case 7) the flag is set
  if(generated_dict[n, 2] < 7)
    flag1 <- 0
  
  # If a new word above the threshold value is encountered in the generated_dict, then it is appended to 
  # manual_dict
  if(match(generated_dict[n, 1], manual_dict$topic, nomatch = 0) == 0)
  {
    manual_dict[p, 1] <- generated_dict[n, 1]
    p <- p + 1
  }
  n <- n + 1
  
}
write.csv(x = manual_dict, file = "generated_dictionary.csv", row.names = FALSE)
