# noEngagementPost()          Extracts the id of the posts which did not generate engagement for a given period and         
#                             also keeps a count of no engagement post
# Description                 noEngagementPost() retrieves the id of all posts which generated less than 10 likes 
#                             during the given period of a month.
#
#
#
#
#
#
#
#
#
#
#
#
#
#
noEngagementPost <- function(start_date, end_date, conn)
{
  #posts <- read.csv(conn, header = TRUE)
  
  #Retrieve posts from DB
  
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from page_owner_post"
  posts<-dbGetQuery(mydb,query)
  
  source("month_engagement_update.R")
  engagement_df <- getEngagementMonthly(startdate = start_date, enddate = end_date)
  
  dates <- posts$created_time
  dates <- as.Date(dates)
  posts$time_created <- dates
  
  noEngagePost <- data.frame(startdate = character(), enddate = character(), id = character(), 
                             type = character(), stringsAsFactors = FALSE)
  noEngageRatio <- data.frame(startdate = character(), enddate = character(), noEngagementPost = integer(),
                              totalPosts = integer(), stringsAsFactors = FALSE)
  
  k <- 1
  
  
  if(match("NA", engagement_df$enddate, nomatch = 1)) {
    counter <- nrow(engagement_df) - 1
  } else {
    counter <- nrow(engagement_df)
  }
  
  for(i in 1:counter)
  {
    
    noEngageCount <- integer()
    noEngageCount <- 0
    
    post_range <- subset(posts, posts$time_created >= as.Date(engagement_df$startdate[i]) & 
                           posts$time_created <= as.Date(engagement_df$enddate[i]))
    
    for(j in 1:nrow(post_range))
    {
      if(post_range$likes_count[j] < 10 & post_range$comments_count[j] <5 & post_range$shares_count[j]<5)
      {
        noEngagePost[k, 1] <- toString(engagement_df[i, 1])
        noEngagePost[k, 2] <- toString(engagement_df[i, 2])
        noEngagePost[k, 3] <- toString(post_range$id[j])
        noEngagePost[k, 4] <- toString(post_range$type[j])
        noEngageCount <- noEngageCount + 1
        k <- k + 1
      }
    }
    noEngageRatio[i, 1] <- toString(engagement_df[i, 1])
    noEngageRatio[i, 2] <- toString(engagement_df[i, 2])
    noEngageRatio[i, 3] <- noEngageCount
    noEngageRatio[i, 4] <- nrow(post_range)
  }
  dbDisconnect(mydb)
  return(noEngageRatio)
}
