source('C:/Users/DELL/Documents/FYP/getConcern.R')
source('C:/Users/DELL/Documents/FYP/getSentiment.R')
library(stringr)
mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
query<- "select * from reviews"
review<-dbGetQuery(mydb,query)
for(i in 1:nrow(review))
{
  str <- gsub("'","''",toString(review$message[i]))
  query<-paste("select * from review_analysis where id=",review$id[i],sep="")
  res<-dbGetQuery(mydb,query)
  if(nrow(res)==0)
  {
  concern<-get_concerns(toString(review$message[i]))
  sentiment<-polarity_sentiAnalysis(toString(review$message[i]))
  print(review$id[i])
  if(identical(toString(concern$concerns),"No significant concern")==FALSE)
  {
    query<-paste("INSERT IGNORE INTO review_analysis VALUES(",review$id[i],",'",str,"','",
                 review$created_time[i],"','",sentiment$FinalSentiment,"','",concern$primary_concern,
                 "','",concern$secondary_concerns,"')",sep="")
    dbGetQuery(mydb,query)
  }
  }
}
dbDisconnect(mydb)
