library(RMySQL)
library(ggmap)
library(stringr)
require("tm")
library(cld3)
require("tm")
source('C:/Users/DELL/Documents/FYP/location.R')
mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
query<- "select * from page_owner_post"
post<-dbGetQuery(mydb,query)
for(i in 2221:nrow(post))
{
  query<- paste("select * from locations where post_id LIKE'",post$id[i],"'",sep="")
  res<-dbGetQuery(mydb,query)  
  if(nrow(res)==0)
  {
    print(i)
    x<-getLocation(post$message[i])
    if(identical(toString(x$location[1]),"NA")==FALSE & identical(toString(x$latitude[1]),"NA")==FALSE 
       & identical(toString(x$longitude[1]),"NA")==FALSE)
    {
      for(j in 1:nrow(x))
      {
      if(identical(toString(x$location[i]),"NA")==FALSE & identical(toString(x$latitude[i]),"NA")==FALSE 
         & identical(toString(x$longitude[i]),"NA")==FALSE)
      {
      q<- paste("INSERT IGNORE INTO locations values('",post$id[i],"','",post$created_time[i],"','",x$location[j],"',",x$latitude[j],",",x$longitude[j],")",sep="")
      dbGetQuery(mydb,q) 
      }
      }
    }
  }
}
dbDisconnect(mydb)