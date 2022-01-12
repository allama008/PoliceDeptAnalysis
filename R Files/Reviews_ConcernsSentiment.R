getFinalConcern <- function(startdate, enddate)
{
  if(as.Date(startdate)>Sys.Date() | as.Date(enddate)>Sys.Date())
  {
    print("Enter a valid date")
    return("NA")
  }
  else
  {
    mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
    query<- "select * from review_analysis"
    posts<-dbGetQuery(mydb,query)
    dates <- posts$created_time
    dates <- as.Date(dates)
    posts$date_created <- dates
    required_posts <- subset(posts, posts$date_created >= as.Date(startdate) & 
                               posts$date_created <= as.Date(enddate))
    concerns_df <- required_posts[, c(1, 2, 5, 6)]
    
    return(concerns_df)
  }
}

getFinalSentiment <- function(startdate, enddate)
{
  if(as.Date(startdate)>Sys.Date() | as.Date(enddate)>Sys.Date())
  {
    print("Enter a valid date")
    return("NA")
  }
  else
  {
    mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
    query<- "select * from review_analysis"
    posts<-dbGetQuery(mydb,query)
    dates <- posts$created_time
    dates <- as.Date(dates)
    posts$date_created <- dates
    required_posts <- subset(posts, posts$date_created >= as.Date(startdate) & 
                               posts$date_created <= as.Date(enddate))
    
    sentiment_df <- required_posts[, c(1, 2, 4)]
    return(sentiment_df)
  }
}

getFinalVPSentiment <- function(startdate, enddate)
{
  if(as.Date(startdate)>Sys.Date() | as.Date(enddate)>Sys.Date())
  {
    print("Enter a valid date")
    return("NA")
  }
  else
  {
    mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
    query<- "select * from sentiment_vp"
    posts<-dbGetQuery(mydb,query)
    dates <- posts$created_time
    dates <- as.Date(dates)
    posts$date_created <- dates
    required_posts <- subset(posts, posts$date_created >= as.Date(startdate) & 
                               posts$date_created <= as.Date(enddate))
    
    sentiment_df <- required_posts[, c(1, 2, 4)]
    return(sentiment_df)
  }
}