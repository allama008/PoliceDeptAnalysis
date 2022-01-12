# Reviews Concerns and Sentiment
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RMySQL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  div("Kolkata Traffic Police", class = "panel panel-default"),
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cyborg"),  # <--- To use a theme, uncomment this
  #"Kolkata Traffic Police",
  # Application title
  titlePanel("User Review Concerns and Sentiment"),
  
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
        tabPanel("Main Concerns", h4("Concerns"), tableOutput("concerns")),
        tabPanel("Sentiment", h4("Sentiment Analysis"), tableOutput("sentiment")),
        tabPanel("Sentiment Plot", h4("Sentiment Plot"), plotOutput("sentiment_plot"))
      )
    )
  )
)

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
    dbDisconnect(mydb)
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
    dbDisconnect(mydb)
    return(sentiment_df)
  }
}


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  main_concerns <- eventReactive(input$input_action, {getFinalConcern(input$dateRange[1], input$dateRange[2])})
  sentiment_reviews <- eventReactive(input$input_action, {getFinalSentiment(input$dateRange[1], input$dateRange[2])})
  
   output$concerns <- renderTable({
     main_concerns()
   })
   
   output$sentiment <- renderTable({
     sentiment_reviews()
   })
   
   output$sentiment_plot <- renderPlot({
     input_startdate <- format(input$dateRange[1], "%d %B %Y")
     input_enddate <- format(input$dateRange[2], "%d %B %Y")
     ggplot(sentiment_reviews(), aes(x = sentiment)) +
       geom_bar(aes(y = ..count.., fill = sentiment)) +
       scale_fill_brewer(palette = "Dark2") +
       labs(x="Polarity", y="Count") +
       ggtitle(paste0("Polarity Categorization for ", input_startdate, " to ", input_enddate)) +
       theme(plot.title = element_text(size = 18, face = "bold")) + 
       theme(text = element_text(size = 16), axis.title.x = element_text(size = 16))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

