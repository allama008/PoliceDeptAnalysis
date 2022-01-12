# ENGAGEMENT STATISTICS (Likes/Comments/Shares/)
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
  titlePanel("Monthly Engagement Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 1, end = Sys.Date(), 
                     min = Sys.Date() - 730, max = Sys.Date()),
      actionButton(inputId = "input_action", label = "Show Engagement Statistics"),
      # Horizontal line ----
      tags$hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Likes Distribution", h4("Plot"), plotOutput("likes")),
        tabPanel("Comments Distribution", h4("Plot"), plotOutput("comments")),
        tabPanel("Shares Distribution", h4("Plot"), plotOutput("shares"))
      )
    )
  )
)


getEngagementMonthly<-function(startdate,enddate)
{
  if(as.Date(startdate)>Sys.Date() | as.Date(enddate)>Sys.Date())
  {
    print("Enter a valid date")
    return("NA")
  }
  else
  {
    mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
    query<- "select * from page_owner_post"
    posts<-dbGetQuery(mydb,query)
    #posts <- read.csv(conn, stringsAsFactors = FALSE, header = TRUE)
    df<-data.frame(matrix(ncol = 26, nrow = 0))
    x <- c("startdate", "enddate","likes","comments","shares","status","photo","link","video","status_likes","photo_likes",
           "link_likes","video_likes","status_comments","photo_comments","link_comments",
           "video_comments","status_shares","photo_shares","link_shares","video_shares",
           "wend_posts","wday_posts","wend_likes","wday_likes","no")
    colnames(df)<-x
    dt=startdate
    flag=0
    nextdate=toString(seq(as.Date(dt), by = "month", length = 2)[2])
    if((as.Date(enddate)-as.Date(nextdate))<0)
    {
      flag=1
    }
    else
    {
      flag=0
    }
    while(flag==0)
    {
      likes=0
      comments=0
      shares=0
      status=0
      photo=0
      link=0
      video=0
      status_likes=0
      photo_likes=0
      video_likes=0
      link_likes=0
      status_comments=0
      photo_comments=0
      video_comments=0
      link_comments=0
      status_shares=0
      photo_shares=0
      video_shares=0
      link_shares=0
      wend_posts=0
      wend_likes=0
      wday_likes=0
      wday_posts=0
      no=0
      nextdate=toString(seq(as.Date(dt), by = "month", length = 2)[2]);
      if((as.Date(enddate)-as.Date(nextdate))==0)
      {
        post<-subset(posts,as.Date(posts$created_time)>=dt & as.Date(posts$created_time)<nextdate)
        if(nrow(post)!=0)
        {
          for(j in 1:nrow(post))
          {
            no=no+1;
            if(identical(toString(post$type[j]),"status")==TRUE)
            {
              status=status+1
              status_likes=status_likes+as.numeric(post$likes_count[j])
              status_comments=status_comments+as.numeric(post$comments_count[j])
              status_shares=status_comments+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"photo")==TRUE)
            {
              photo=photo+1
              photo_likes=photo_likes+as.numeric(post$likes_count[j])
              photo_comments=photo_comments+as.numeric(post$comments_count[j])
              photo_shares=photo_shares+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"video")==TRUE)
            {
              video=video+1
              video_likes=video_likes+as.numeric(post$likes_count[j])
              video_comments=video_comments+as.numeric(post$comments_count[j])
              video_shares=video_shares+as.numeric(post$shares_count[j])
            }
            else
            {
              link=link+1
              link_likes=link_likes+as.numeric(post$likes_count[j])
              link_comments=link_comments+as.numeric(post$comments_count[j])
              link_shares=link_shares+as.numeric(post$shares_count[j])
            }
            if(identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE | identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE)
            {
              wend_posts=wend_posts+1
              wend_likes=wend_likes+as.numeric(post$likes_count[j])
            }
            else
            {
              wday_posts=wday_posts+1
              wday_likes=wday_likes+as.numeric(post$likes_count[j])
            }
          }
          likes=status_likes+photo_likes+video_likes+link_likes
          comments=status_comments+photo_comments+video_comments+link_comments
          shares=status_shares+photo_shares+video_shares+link_shares
        }
        de<- data.frame(toString(dt),toString(as.Date(nextdate)-1),likes,comments,shares,status,photo,link,video,status_likes,photo_likes,link_likes,
                        video_likes,status_comments,photo_comments,link_comments,video_comments,
                        status_shares,photo_shares,link_shares,video_shares,wend_posts,wday_posts,
                        wend_likes,wday_likes,no)
        names(de)<-c("startdate", "enddate","likes","comments","shares","status","photo","link","video","status_likes","photo_likes",
                     "link_likes","video_likes","status_comments","photo_comments","link_comments",
                     "video_comments","status_shares","photo_shares","link_shares","video_shares",
                     "wend_posts","wday_posts","wend_likes","wday_likes","no")
        df=rbind(df,de)
        #print(dt); print("-"); print(as.Date(nextdate)-1)
        flag=1
        break;
      }
      if((as.Date(enddate)-as.Date(nextdate))<0)
      {
        flag=1
        break
      }
      if((as.Date(enddate)-as.Date(nextdate))>0 & flag==0)
      {
        post<-subset(posts,as.Date(posts$created_time)>=dt & as.Date(posts$created_time)<nextdate)
        if(nrow(post)!=0)
        {
          for(j in 1:nrow(post))
          {
            no=no+1;
            if(identical(toString(post$type[j]),"status")==TRUE)
            {
              status=status+1
              status_likes=status_likes+as.numeric(post$likes_count[j])
              status_comments=status_comments+as.numeric(post$comments_count[j])
              status_shares=status_comments+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"photo")==TRUE)
            {
              photo=photo+1
              photo_likes=photo_likes+as.numeric(post$likes_count[j])
              photo_comments=photo_comments+as.numeric(post$comments_count[j])
              photo_shares=photo_shares+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"video")==TRUE)
            {
              video=video+1
              video_likes=video_likes+as.numeric(post$likes_count[j])
              video_comments=video_comments+as.numeric(post$comments_count[j])
              video_shares=video_shares+as.numeric(post$shares_count[j])
            }
            else
            {
              link=link+1
              link_likes=link_likes+as.numeric(post$likes_count[j])
              link_comments=link_comments+as.numeric(post$comments_count[j])
              link_shares=link_shares+as.numeric(post$shares_count[j])
            }
            if(identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE | identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE)
            {
              wend_posts=wend_posts+1
              wend_likes=wend_likes+as.numeric(post$likes_count[j])
            }
            else
            {
              wday_posts=wday_posts+1
              wday_likes=wday_likes+as.numeric(post$likes_count[j])
            }
          }
          likes=status_likes+photo_likes+video_likes+link_likes
          comments=status_comments+photo_comments+video_comments+link_comments
          shares=status_shares+photo_shares+video_shares+link_shares
        }
        de<- data.frame(toString(dt),toString(as.Date(nextdate)-1),likes,comments,shares,status,photo,link,video,status_likes,photo_likes,link_likes,
                        video_likes,status_comments,photo_comments,link_comments,video_comments,
                        status_shares,photo_shares,link_shares,video_shares,wend_posts,wday_posts,
                        wend_likes,wday_likes,no)
        names(de)<-c("startdate", "enddate","likes","comments","shares","status","photo","link","video","status_likes","photo_likes",
                     "link_likes","video_likes","status_comments","photo_comments","link_comments",
                     "video_comments","status_shares","photo_shares","link_shares","video_shares",
                     "wend_posts","wday_posts","wend_likes","wday_likes","no")
        df=rbind(df,de)
        #print(dt); print("-"); print(as.Date(nextdate)-1)
      } 
      
      dt=nextdate;
    }
    if(flag==1)
    {
      likes=0
      comments=0
      shares=0
      status=0
      photo=0
      link=0
      video=0
      status_likes=0
      photo_likes=0
      video_likes=0
      link_likes=0
      status_comments=0
      photo_comments=0
      video_comments=0
      link_comments=0
      status_shares=0
      photo_shares=0
      video_shares=0
      link_shares=0
      wend_posts=0
      wend_likes=0
      wday_likes=0
      wday_posts=0
      no=0
      if((as.Date(enddate)-as.Date(nextdate))==0)
      {
        
        post<-subset(posts,as.Date(posts$created_time)==nextdate)
        if(nrow(post)!=0)
        {
          for(j in 1:nrow(post))
          {
            no=no+1;
            if(identical(toString(post$type[j]),"status")==TRUE)
            {
              status=status+1
              status_likes=status_likes+as.numeric(post$likes_count[j])
              status_comments=status_comments+as.numeric(post$comments_count[j])
              status_shares=status_comments+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"photo")==TRUE)
            {
              photo=photo+1
              photo_likes=photo_likes+as.numeric(post$likes_count[j])
              photo_comments=photo_comments+as.numeric(post$comments_count[j])
              photo_shares=photo_shares+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"video")==TRUE)
            {
              video=video+1
              video_likes=video_likes+as.numeric(post$likes_count[j])
              video_comments=video_comments+as.numeric(post$comments_count[j])
              video_shares=video_shares+as.numeric(post$shares_count[j])
            }
            else
            {
              link=link+1
              link_likes=link_likes+as.numeric(post$likes_count[j])
              link_comments=link_comments+as.numeric(post$comments_count[j])
              link_shares=link_shares+as.numeric(post$shares_count[j])
            }
            if(identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE | identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE)
            {
              wend_posts=wend_posts+1
              wend_likes=wend_likes+as.numeric(post$likes_count[j])
            }
            else
            {
              wday_posts=wday_posts+1
              wday_likes=wday_likes+as.numeric(post$likes_count[j])
            }
          }
          likes=status_likes+photo_likes+video_likes+link_likes
          comments=status_comments+photo_comments+video_comments+link_comments
          shares=status_shares+photo_shares+video_shares+link_shares
        }
        de<- data.frame(toString(enddate),"NA",likes,comments,shares,status,photo,link,video,status_likes,photo_likes,link_likes,
                        video_likes,status_comments,photo_comments,link_comments,video_comments,
                        status_shares,photo_shares,link_shares,video_shares,wend_posts,wday_posts,
                        wend_likes,wday_likes,no)
        names(de)<-c("startdate", "enddate","likes","comments","shares","status","photo","link","video","status_likes","photo_likes",
                     "link_likes","video_likes","status_comments","photo_comments","link_comments",
                     "video_comments","status_shares","photo_shares","link_shares","video_shares",
                     "wend_posts","wday_posts","wend_likes","wday_likes","no")
        df=rbind(df,de)
        #print(enddate); print("-"); print("NA")
      }
      else
      {
        post<-subset(posts,as.Date(posts$created_time)>=as.Date(dt) & as.Date(posts$created_time)<=as.Date(enddate))
        if(nrow(post)!=0)
        {
          for(j in 1:nrow(post))
          {
            
            no=no+1;
            if(identical(toString(post$type[j]),"status")==TRUE)
            {
              status=status+1
              status_likes=status_likes+as.numeric(post$likes_count[j])
              status_comments=status_comments+as.numeric(post$comments_count[j])
              status_shares=status_comments+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"photo")==TRUE)
            {
              photo=photo+1
              photo_likes=photo_likes+as.numeric(post$likes_count[j])
              photo_comments=photo_comments+as.numeric(post$comments_count[j])
              photo_shares=photo_shares+as.numeric(post$shares_count[j])
            }
            else if(identical(toString(post$type[j]),"video")==TRUE)
            {
              video=video+1
              video_likes=video_likes+as.numeric(post$likes_count[j])
              video_comments=video_comments+as.numeric(post$comments_count[j])
              video_shares=video_shares+as.numeric(post$shares_count[j])
            }
            else
            {
              link=link+1
              link_likes=link_likes+as.numeric(post$likes_count[j])
              link_comments=link_comments+as.numeric(post$comments_count[j])
              link_shares=link_shares+as.numeric(post$shares_count[j])
            }
            if(identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE | identical(weekdays(as.Date(post$created_time[j])),"Saturday")==TRUE)
            {
              wend_posts=wend_posts+1
              wend_likes=wend_likes+as.numeric(post$likes_count[j])
            }
            else
            {
              wday_posts=wday_posts+1
              wday_likes=wday_likes+as.numeric(post$likes_count[j])
            }
          }
          likes=status_likes+photo_likes+video_likes+link_likes
          comments=status_comments+photo_comments+video_comments+link_comments
          shares=status_shares+photo_shares+video_shares+link_shares
        }
        de<- data.frame(toString(dt),toString(enddate),likes,comments,shares,status,photo,link,video,status_likes,photo_likes,link_likes,
                        video_likes,status_comments,photo_comments,link_comments,video_comments,
                        status_shares,photo_shares,link_shares,video_shares,wend_posts,wday_posts,
                        wend_likes,wday_likes,no)
        names(de)<-c("startdate", "enddate","likes","comments","shares","status","photo","link","video","status_likes","photo_likes",
                     "link_likes","video_likes","status_comments","photo_comments","link_comments",
                     "video_comments","status_shares","photo_shares","link_shares","video_shares",
                     "wend_posts","wday_posts","wend_likes","wday_likes","no")
        df=rbind(df,de)
        #print(dt); print("-"); print(enddate)
      }
    }
    #write.csv(df,'C:/Users/DELL/Desktop/csvs/month_engagement.csv',row.names=FALSE)
    df$avg = as.double(as.numeric(df$no)/as.numeric(difftime(as.Date(df$enddate),as.Date(df$startdate))))
    df$avg=as.double(format(round(df$avg,2),nsmall=2))
    df$dates <- paste(df$startdate, df$enddate, sep = "\n")
    dbDisconnect(mydb)
    return(df)
  }
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  engagement_df <- eventReactive(input$input_action, {getEngagementMonthly(input$dateRange[1], input$dateRange[2])})
  
  output$likes <- renderPlot({
    ggplot(engagement_df(), aes(x = dates, y = as.numeric(likes))) +
      geom_bar(stat = "identity") +
      labs(x = "Time Interval", y = "Likes Count") +
      ggtitle("Distribution of Likes") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label=as.numeric(likes)), vjust=-0.3, color="black", size=3)
  })
  
  output$comments <- renderPlot({
    ggplot(engagement_df(), aes(x = dates, y = as.numeric(comments))) +
      geom_bar(stat = "identity",fill="darkblue") +
      labs(x = "Time Interval", y = "Comment Count") +
      ggtitle("Distribution of Comments") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label=as.numeric(comments)), vjust=-0.3, color="black", size=3)
  })
  
  output$shares <- renderPlot({
    ggplot(engagement_df(), aes(x = dates, y = as.numeric(shares))) +
      geom_bar(stat = "identity") +
      labs(x = "Time Interval", y = "Share Count") +
      ggtitle("Distribution of Shares") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label=as.numeric(shares)), vjust=-0.3, color="black", size=3)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

