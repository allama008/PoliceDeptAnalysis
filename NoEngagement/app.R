# NO ENGAGEMENT POSTS
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RMySQL)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  div("Kolkata Traffic Police", class = "panel panel-default"),
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cyborg"),  # <--- To use a theme, uncomment this
  #"Kolkata Traffic Police",
  # Application title
  titlePanel("No Engagement/Viral Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 1, end = Sys.Date(), 
                     min = Sys.Date() - 730, max = Sys.Date()),
      actionButton(inputId = "input_action", label = "Show Results"),
      # Horizontal line ----
      tags$hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("No Engagement Posts", h4("Posts"), tableOutput("noengage_post")),
        tabPanel("No Engagement Ratio", h4("Ratio"), tableOutput("noengage_ratio")),
        tabPanel("No Engagement Plot", h4("Plot"), plotOutput("noengage_plot")),
        tabPanel("Viral Posts", h4("Posts"), tableOutput("viral_post"))
      )
    )
  )
)

getViralPost <- function(start_date, end_date)
{
  
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from page_owner_post"
  posts<-dbGetQuery(mydb,query)
  
  # Call upon the month_engagement_update.R file to invoke the getEngagementMonthly() function
  source("C:/Users/DELL/Documents/Final FYP/month_engagement_update.R")
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

noEngagementPost <- function(start_date, end_date)
{
  #posts <- read.csv(conn, header = TRUE)
  
  #Retrieve posts from DB
  
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from page_owner_post"
  posts<-dbGetQuery(mydb,query)
  
  source("C:/Users/DELL/Documents/Final FYP/month_engagement_update.R")
  engagement_df <- getEngagementMonthly(startdate = start_date, enddate = end_date)
  
  dates <- posts$created_time
  dates <- as.Date(dates)
  posts$time_created <- dates
  
  noEngagePost <- data.frame(startdate = character(), enddate = character(), id = character(), 
                             type = character(), stringsAsFactors = FALSE)
  
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
  }
  dbDisconnect(mydb)
  return(noEngagePost)
}

noEngagementRatio <- function(start_date, end_date)
{
  #posts <- read.csv(conn, header = TRUE)
  
  #Retrieve posts from DB
  
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from page_owner_post"
  posts<-dbGetQuery(mydb,query)
  
  source("C:/Users/DELL/Documents/Final FYP/month_engagement_update.R")
  engagement_df <- getEngagementMonthly(startdate = start_date, enddate = end_date)
  
  dates <- posts$created_time
  dates <- as.Date(dates)
  posts$time_created <- dates
  
  noEngageRatio <- data.frame(startdate = character(), enddate = character(), noEngagementPost = integer(),
                              totalPosts = integer(), ratio = double(), stringsAsFactors = FALSE)
  
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
        noEngageCount <- noEngageCount + 1
      }
    }
    noEngageRatio[i, 1] <- toString(engagement_df[i, 1])
    noEngageRatio[i, 2] <- toString(engagement_df[i, 2])
    noEngageRatio[i, 3] <- as.integer(noEngageCount)
    noEngageRatio[i, 4] <- nrow(post_range)
    noEngageRatio[i, 5] <- as.double(as.numeric(noEngageCount)/as.numeric(nrow(post_range)))
    noEngageRatio[i, 5] <- as.double(format(round(noEngageRatio[i, 5],2),nsmall=2))
    
  }
  dbDisconnect(mydb)
  return(noEngageRatio)
}

noEngagementPlot <- function(start_date, end_date)
{
  #posts <- read.csv(conn, header = TRUE)
  
  #Retrieve posts from DB
  
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from page_owner_post"
  posts<-dbGetQuery(mydb,query)
  
  source("C:/Users/DELL/Documents/Final FYP/month_engagement_update.R")
  engagement_df <- getEngagementMonthly(startdate = start_date, enddate = end_date)
  
  dates <- posts$created_time
  dates <- as.Date(dates)
  posts$time_created <- dates
  
  noEngageRatio <- data.frame(startdate = character(), enddate = character(), noEngagementPost = integer(),
                              totalPosts = integer(), ratio = double(), stringsAsFactors = FALSE)
  
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
        noEngageCount <- noEngageCount + 1
      }
    }
    noEngageRatio[i, 1] <- toString(engagement_df[i, 1])
    noEngageRatio[i, 2] <- toString(engagement_df[i, 2])
    noEngageRatio[i, 3] <- as.integer(noEngageCount)
    noEngageRatio[i, 4] <- nrow(post_range)
    noEngageRatio[i, 5] <- as.double((as.numeric(noEngageCount)/as.numeric(nrow(post_range))*100))
    noEngageRatio[i, 5] <- as.double(format(round(noEngageRatio[i, 5],2),nsmall=2))
    
  }
  noEngageRatio$dates <- paste(noEngageRatio$startdate, noEngageRatio$enddate, sep = "\n")
  dbDisconnect(mydb)
  return(noEngageRatio)
}


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  posts <- eventReactive(input$input_action, {noEngagementPost(input$dateRange[1], input$dateRange[2])})
  ratio <- eventReactive(input$input_action, {noEngagementRatio(input$dateRange[1], input$dateRange[2])})
  viral <- eventReactive(input$input_action, {getViralPost(input$dateRange[1], input$dateRange[2])})
  plotting <- eventReactive(input$input_action, {noEngagementPlot(input$dateRange[1], input$dateRange[2])})
  
  output$noengage_post <- renderTable({
    posts()
  })
  
  output$noengage_ratio <- renderTable({
    ratio()
  })
  
  output$noengage_plot <- renderPlot({
    ggplot(plotting(), aes(x = dates, y = noEngagementPost, group = 1)) + 
      geom_line(color = "blue", size = 1.4) + geom_point(color = "black", size = 2.5) +
      labs(x = "Time Interval", y = "No Engagement Post") +
      ggtitle("Number of Posts") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold"))
  })
  
  output$viral_post <- renderTable({
    viral()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

