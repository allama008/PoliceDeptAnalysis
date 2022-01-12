# LOCATION 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggmap)
library(splitstackshape)
library(dplyr)
library(RMySQL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  div("Kolkata Traffic Police", class = "panel panel-default"),
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cyborg"),  # <--- To use a theme, uncomment this
  #"Kolkata Traffic Police",
  # Application title
  titlePanel("Congested Location Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 1, end = Sys.Date(), 
                     min = Sys.Date() - 730, max = Sys.Date()),
      actionButton(inputId = "input_action", label = "Show Location"),
      # Horizontal line ----
      tags$hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(h4("Plot"), plotOutput("plot")
    )
  )
)

plotLocation<-function(startdate,enddate)
{
  mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
  query<- "select * from locations"
  posts<-dbGetQuery(mydb,query)
  location<-subset(posts,as.Date(posts$created_time)>=as.Date(startdate) & as.Date(posts$created_time)<=as.Date(enddate))
  df<-cSplit(location, "location", direction = "long") %>%
    group_by(location) %>%
    summarise(Count = n())
  x<-merge(df,location)
  map = get_map(location = c(lon = 88.3639, lat = 22.57265), zoom = 12, maptype = 'roadmap')
  plot=ggmap(map, extent = "device")
  for(i in 1:4)
  {
    y=subset(x,Count==i)
    sz=i*0.5
    if(nrow(y)!=0)
    {
      #print(sz)
      plot<-plot+geom_point(data=y,aes(x=longitude,y=latitude),color="red",size=sz)
    }
  }
  for(i in 5:11)
  {
    y=subset(x,Count==i)
    sz=i*0.3
    if(nrow(y)!=0)
    {
      #print(sz)
      plot<-plot+geom_point(data=y,aes(x=longitude,y=latitude),color="red",size=sz)
    }
  }
  #return_plot<-plot+geom_point(data=x,aes(x=longitude,y=latitude),color="red",size=0.1*as.numeric(x$count))
  #ggsave("C:/Users/DELL/Desktop/csvs/plot/location.jpg")
  dbDisconnect(mydb)
  #return(return_plot)
  return(plot)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  loc_plot <- eventReactive(input$input_action, {plotLocation(input$dateRange[1], input$dateRange[2])})
  
  output$plot <- renderPlot({
    loc_plot()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

