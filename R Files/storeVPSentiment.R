source('C:/Users/DELL/Documents/FYP/getSentiment.R')
library(stringr)
mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
query<- "select * from visitor_post"
post<-dbGetQuery(mydb,query)
for(i in 1370:nrow(post))
{
  query<-paste("select * from sentiment_vp where id LIKE '",post$id[i],"'",sep="")
  res<-dbGetQuery(mydb,query)
  if(nrow(res)==0)
  {
    print(i)
    sentiment<-polarity_sentiAnalysis(toString(post$message[i]))
    str <- gsub("'","''",toString(post$message[i]))
    query<-paste("INSERT IGNORE INTO sentiment_vp VALUES('",post$id[i],"','",str,"','",
                 post$created_time[i],"','",sentiment$FinalSentiment,"')",sep="")
    dbGetQuery(mydb,query)
  }
}
dbDisconnect(mydb)