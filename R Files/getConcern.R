# **********************************************************************************************

# START

# **********************************************************************************************


# getConcerns()           Extracts the main essence of the text
# Description             Processes the text and carries out key phrase extraction with the help of domain
#                         dictionary to find the main concerns and insights of a text
# Usage                   getConcerns(conn1, conn2)
#
# conn1     the name of the review file from which the data are to be read. Provides the .csv file which contains
#           the messages for which the concerns are to be extracted
# conn2     the name of the dictionary file from which the data are to be read. Provides the .csv file which 
#           contains the domain words significant to the messages.

# Install the packages
#install.packages("NLP")
#install.packages("openNLP")
#install.packages("stringr")

# Load the packages
library(NLP)
library(openNLP)
library(stringr)

get_concerns <- function(message)
{
  
  # Reading the .csv files
  dictionary <- read.csv("C:/Users/DELL/Desktop/csvs/generated_dictionary.csv", 
                         header = TRUE, stringsAsFactors = FALSE)
  
  # Creating a new dataframe to store the final result
  review_result <- data.frame(concerns = character(), primary_concerns = character(), 
                              secondary_concerns = character(), stringsAsFactors = FALSE)
  
  # Running a for loop to process every row/message of the file.
  for(i in 1:1)
  {
    strng <- message
    x <- NLP::as.String(strng)
    
    # Before POS tagging, we need to do Sentence annotation followed by word annotation
    wordAnnotation <- NLP::annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
    
    # POS tag the words & extract the "words" from the output
    POSAnnotation <- NLP::annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
    POSwords <- subset(POSAnnotation, type == "word")
    
    # Extract the tags from the words
    tags <- sapply(POSwords$features, '[[', "POS")
    
    # Create a data frame with words and tags
    tokenizedAndTagged <- data.frame(Tokens = as.character(x[POSwords]), Tags = tags, stringsAsFactors = FALSE)
    
    # Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
    # In this case we only want Noun and Adjective tags (NN, JJ)
    # Note that this will also capture variations such as NNP, NNPS etc
    tokenizedAndTagged$Tags_mod = grepl("NN|JJ", tokenizedAndTagged$Tags)
    
    # Initialize a vector to store chunk indexes
    chunk = vector()
    
    # Iterate through each word and assign each one to a group
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
    res = sapply(res, function(x) paste(x))
    
    # Initialize the variables. k to iterate the temporary primary result dataframe - rev_result.
    # n to iterate the temporary secondary result dataframe - rev_result_secondary.
    k <- 1
    n <- 1
    rev_result <- data.frame(concerns = character(), stringsAsFactors = FALSE)
    rev_result_secondary <- data.frame(concerns = character(), stringsAsFactors = FALSE)
    
    # If res contains values, then concerns have been obtained, otherwise there were no significant concerns
    # available
    if(length(res))
    {  
      review_result[i, 1] <- as.character("Obtained")
      
      # Iterates through every key phrase obtained and decides whether it is a primary concern or secondary concern
      # Also checks whether the phrase has not already been stored to avoid redundancy
      for(l in 1:length(res))
      {
        var1 <- word(res[[l]], start = -1, sep = fixed(" "))
        
        # Concerns are categorized into primary if the NN (noun) is present in the domain dictionary.
        if(match(var1, dictionary$topic, nomatch = 0) & match(res[[l]], rev_result$concerns, nomatch = 0) == 0)
        {
          rev_result[k, 1] <- res[[l]]
          k <- k + 1
        } else if(match(res[[l]], rev_result_secondary$concerns, nomatch = 0) == 0) {
          rev_result_secondary[n, 1] <- as.character(res[[l]])
          n <- n + 1
        }
      }
    } else {
      review_result[i, 1] <- as.character("No significant concern")
    }
    
    # Stores the message as it is into the final result dataframe - review_result
    #review_result[i, 1] <- message
    
    # If rev_result or rev_result_secondary contains values then store them in review_result else leave blank
    if(nrow(rev_result) | nrow(rev_result_secondary))
    {
      
      # Store only the first concern from primary and secondary concern list into the review_result to avoid NA
      # while using paste function later
      if(nrow(rev_result))
        review_result[i, 2] <- rev_result[1, 1]
      else
        review_result[i, 2] <- as.character("")
      if(nrow(rev_result_secondary))
        review_result[i, 3] <- rev_result_secondary[1, 1]
      else
        review_result[i, 3] <- as.character("")
      
      # Initialize the variables. m to iterate the rev_result and o to iterate the rev_result_secondary 
      m <- 2
      o <- 2
      flag <- 0
      
      # Keep copying the concerns from primary and secondary list to the review_result until the last.
      # Once both the list are complete, flag is set 1 and the loop terminates
      # paste function is used to place all the concerns side by side in review_result
      while((nrow(rev_result)>1 | nrow(rev_result_secondary)>1) & flag == 0)
      {
        if(m<=nrow(rev_result))
        {
          review_result[i, 2] <- paste(review_result[i, 2], rev_result[m, 1], sep = ", ")
          m <- m + 1
        } else if(o<=nrow(rev_result_secondary)) {
          review_result[i, 3] <- paste(review_result[i, 3], rev_result_secondary[o, 1], sep = ", ")
          o <- o + 1
        } else {
          flag <- 1
        }
      }
    } else {
      if(nrow(rev_result)==0)
        review_result[i, 2] <- as.character("")
      if(nrow(rev_result_secondary)==0)
        review_result[i, 3] <- as.character("")
    }
    
  }
  
  return(review_result)
}

#df <- get_concerns("they r doing excellent job for us...hats off all of u")


# **********************************************************************************************

# END

# **********************************************************************************************