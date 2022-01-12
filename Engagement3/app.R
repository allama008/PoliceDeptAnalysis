# WEEKLY ENGAGEMENT STATISTICS (Total Posts/Avg Posts/Post Type Division)
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RMySQL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  div("Kolkata Traffic Police", class = "panel panel-default"),
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cyborg"),  # <--- To use a theme, uncomment this
  #"Kolkata Traffic Police",
  # Application title
  titlePanel("Weekly Engagement Statistics"),
  
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
        tabPanel("Total Number of Posts", h4("Plot"), plotOutput("total_post")),
        tabPanel("Average Number of Posts", h4("Plot"), plotOutput("avg_post")),
        tabPanel("Post Type Division", h4("Plot"), plotOutput("post_type"))
      )
    )
  )
)


getEngagementWeekly<-function(startdate,enddate)
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
    nextdate=toString(seq(as.Date(dt), by = "week", length = 2)[2])
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
      nextdate=toString(seq(as.Date(dt), by = "week", length = 2)[2]);
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
    #write.csv(df,'C:/Users/DELL/Desktop/csvs/week_engagement.csv',row.names=FALSE)
    df$avg = as.double(as.numeric(df$no)/as.numeric(difftime(as.Date(df$enddate),as.Date(df$startdate))))
    df$avg=as.double(format(round(df$avg,2),nsmall=2))
    df$dates <- paste(df$startdate, df$enddate, sep = "\n")
    dbDisconnect(mydb)
    return(df)
  }
}

post_type_util <- function(engaging_df)
{
  type = c("Status", "Photo", "Link", "Video")
  post_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(post_df) <- c("date", "freq", "type")
  k <- 1
  for(i in 1:nrow(engaging_df))
  {
    for(j in 1:4)
    {
      date_range <- paste(engaging_df[i, 1],engaging_df[i, 2],sep="\n")
      post_df[k, 1] <- date_range
      post_df[k, 2] <- engaging_df[i, 5+j]
      post_df[k, 3] <- type[j]
      k <- k + 1
    }
  }
  return(post_df)
}



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  engagement_df <- eventReactive(input$input_action, {getEngagementWeekly(input$dateRange[1], input$dateRange[2])})
  graphics_df <- eventReactive(input$input_action, {post_type_util(engagement_df())})
  
  output$total_post <- renderPlot({
    ggplot(engagement_df(), aes(x = dates, y = as.numeric(no))) +
      geom_bar(stat = "identity") +
      labs(x = "Time Interval", y = "No. of Post") +
      ggtitle("Number of Posts") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label=as.numeric(no)), vjust=-0.3, color="black", size=3)
  })
  
  output$avg_post <- renderPlot({
    ggplot(engagement_df(), aes(x = dates, y = as.double(avg))) +
      geom_bar(stat = "identity") +
      labs(x = "Time Interval", y = "Average Count") +
      ggtitle("Average No. of Posts") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label=as.double(avg)), vjust=-0.3, color="black", size=3)
  })
  
  
  output$post_type <- renderPlot({
    ggplot(graphics_df(), aes(x=date, y = freq, fill = type)) +
      geom_bar(stat="identity")+
      labs(x = "Time Interval", y = "Frequency") +
      ggtitle("Type of Posts") +
      theme(axis.title.x = element_text(), axis.title.y = element_text(), 
            plot.title = element_text(size = 10, face = "bold")) +
      guides(color=guide_legend("Type:"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

