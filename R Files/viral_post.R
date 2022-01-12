# getViralPost()          Extracts the id of the posts which were viral for a given period
# Description             getViralPost() retrieves the id of all posts which generated more than 5% of the 
#                         total likes generated during the given period of a month.
# Usage                   getViralPost(start_date, end_date, conn)

# start_date      Specifies the start date for the range of time the posts have to be processed
# end_date        Specifies the end date for the range of time the posts have to be processed
# conn            the name of the page owner post file from which the data are to be read. Provides the .csv 
#                 file which contains the data extracted using Facebook API
#

# The following function (getViralPost()) is dependent on the month_engagement_update.R file and the both the files 
# should be stored in the same working directory

getViralPost <- function(start_date, end_date)
{
  
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from page_owner_post"
  posts<-dbGetQuery(mydb,query)
  
  # Call upon the month_engagement_update.R file to invoke the getEngagementMonthly() function
  source("month_engagement_update.R")
  engagement_df <- getEngagementMonthly(startdate = start_date, enddate = end_date)
  
  # Change the Facebook timestamp to Date object for processing
  dates <- posts$created_time
  dates <- as.Date(dates)
  posts$time_created <- dates
  
  # Initialize variables and dataframe
  total_likes <- integer()
  viral_threshold <- integer()
  total_likes <- 0
  k <- 1
  
  viral_post <- data.frame(start_date = character(), end_date = character(), postid = character(),
                           type=character(),stringsAsFactors = FALSE)
  
  # Define conditions for loop termination
  if(match("NA", table = engagement_df$enddate, nomatch = 1)) {
    counter <- nrow(engagement_df)-1
  } else {
    counter <- nrow(engagement_df)
  }
  
  # Run the loop that iterates through engagement_df, i.e. the dataframe generated from the getEngagementMonthly()
  # function
  for(i in 1:counter)#
  {
    # Subset the posts for month intervals between start_date and end_date
    post_range <- subset(posts, posts$time_created >= as.Date(engagement_df$startdate[i]) & 
                           posts$time_created <= as.Date(engagement_df$enddate[i]))
    
    # Compute total_likes generated during the interval and the threshold likes for viral posts
    total_likes <- engagement_df$likes[i]
    total_comments <- engagement_df$comments[i]
    total_shares <- engagement_df$shares[i]
    likes_threshold = 0.1 * total_likes
    comments_threshold = 0.1 * total_comments
    shares_threshold = 0.1 * total_shares

    # Run loop to iterate through the posts of post_range and store the records that have like_count greater 
    # than viral_threshold
    for (j in 1:nrow(post_range)) #
    { 
      variable <- post_range$likes_count[j]
      if(post_range$likes_count[j] > likes_threshold | post_range$comments_count[j] > comments_threshold 
         | post_range$shares_count[j]> shares_threshold)
      {
        viral_post[k, 1] <- toString(engagement_df[i, 1])
        viral_post[k, 2] <- toString(engagement_df[i, 2])
        viral_post[k, 3] <- toString(post_range$id[j])
        viral_post[k, 4] <- toString(post_range$type[j])
        k <- k + 1
      }
    }
  }
  dbDisconnect(mydb)
  #write.csv(viral_post,'C:/Users/DELL/Desktop/csvs/viral_post.csv',row.names=FALSE)
  return(viral_post)
}

