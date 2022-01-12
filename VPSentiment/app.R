# Visitor Post Sentiment
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
  titlePanel("Visitor Post Sentiment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 1, end = Sys.Date(), 
                     max = Sys.Date()),
      actionButton(inputId = "input_action", label = "Show Sentiment"),
      # Horizontal line ----
      tags$hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Table", h4("Sentiment Table"), tableOutput("sentiment")),
        tabPanel("Sentiment Plot", h4("Sentiment Plot"), plotOutput("sentiment_plot"))
      )
    )
  )
)

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
    dbDisconnect(mydb)
    return(sentiment_df)
  }
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sentiment_visitor <- eventReactive(input$input_action, {getFinalVPSentiment(input$dateRange[1], input$dateRange[2])})

  
  output$sentiment <- renderTable({
    sentiment_visitor()
  })
  
  output$sentiment_plot <- renderPlot({
    input_startdate <- format(input$dateRange[1], "%d %B %Y")
    input_enddate <- format(input$dateRange[2], "%d %B %Y")
    ggplot(sentiment_visitor(), aes(x = sentiment)) +
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

