library(ggmap)
library(splitstackshape)
library(dplyr)
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
  plot=ggmap(map)
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
  ggsave("C:/Users/DELL/Desktop/csvs/plot/location.jpg")
  dbDisconnect(mydb)
  #return(return_plot)
  return(plot)
}
