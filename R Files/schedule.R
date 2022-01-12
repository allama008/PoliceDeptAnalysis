#Loading Libraries

library(RMySQL)
library(cld3)

#Connect to Database

mydb = dbConnect(MySQL(), user='root', password='', dbname='ktp', host='127.0.0.1')
token<-"EAAZAOqJapwNgBABJDwaJFSdVSKp7jw8DIvXTye1ZCDblB08MgqKe3fZC0h6Bb7nflPJ6yZCZCGhZAsNaKdjmNlwB6ZAxIU6K7EXhLZCZCnQZAKChefDp4pcYRZApM41jVjHbUZCZB2kcQLDVKuAt5qiNcNDhD9Efs3ZCTZArIbrnEt4zzeMrQZDZD"

#Extract Page Owner Post

post<-Rfacebook::getPage("KolkataTrafficPolice",token,n=25,feed=FALSE)
write.csv(post,"C:/Users/DELL/Desktop/csvs/pageowner_post.csv")
post<-read.csv("C:/Users/DELL/Desktop/csvs/pageowner_post.csv")

#Removing Duplicated Post

post<-post[!duplicated(post$message),]

#Removing post in vernacular languages

post$language=detect_language(toString(post$message))
post=post[post$language!="bn" & post$language!="hi",]
post$language=NULL

#Inserting/Updating records in Page owner post DB

for(i in 1:nrow(post))
{
  str <- gsub("'","''",toString(post$message[i]))
  query_pop<-paste("Insert into page_owner_post values(","'",Sys.Date(),"','",post$id[i],"','",str,
               "','",post$type[i],"','",toString(post$created_time[i]),"','",post$link[i],"',",post$likes_count[i],",",
               post$comments_count[i],",",post$shares_count[i],",",1,")"," ON DUPLICATE KEY Update sequence_key='",
               Sys.Date(),"',likes_count=",post$likes_count[i],",comments_count=",post$comments_count[i],
               ",shares_count=",post$shares_count[i],sep="")
  dbGetQuery(mydb,query_pop)
}

#Extracting Visitor Post

post1<-Rfacebook::getPage("KolkataTrafficPolice",token,n=100,feed=TRUE)
postvp<- subset(post1,is.na(from_name))
write.csv(postvp,"C:/Users/DELL/Desktop/csvs/visitor_post.csv")
postvp<-read.csv("C:/Users/DELL/Desktop/csvs/visitor_post.csv")

#Removing Duplicate Visitor posts

postvp<-postvp[!duplicated(postvp$message),]

#Removing post in vernacular language

postvp$language=detect_language(toString(postvp$message))
postvp=postvp[postvp$language!="bn" & postvp$language!="hi",]
postvp$language=NULL

#Storing data in Visitor Post DB

for(i in 1:nrow(postvp))
{
  str <- gsub("'","''",toString(postvp$message[i]))
  query_vp<-paste("Insert into visitor_post values(","'",Sys.Date(),"','",postvp$id[i],"','",str,
               "','",toString(postvp$created_time[i]),"',",postvp$likes_count[i],",",
               postvp$comments_count[i],",",postvp$shares_count[i],",",1,")"," ON DUPLICATE KEY Update sequence_key='",
               Sys.Date(),"',likes_count=",postvp$likes_count[i],",comments_count=",postvp$comments_count[i],
               ",shares_count=",postvp$shares_count[i],sep="")
  dbGetQuery(mydb,query_vp)
}

#Storing reviews in DB

review<-read.csv("C:/Users/DELL/Desktop/csvs/traffic_reviews.csv")
for(i in 1:nrow(review))
{
  str <- gsub("'","''",toString(review$review_message[i]))
  query<-paste("Insert IGNORE into reviews(sequence_key,message,rating,created_time) VALUES('",Sys.Date(),
               "','",str,"',",review$rating[i],",'",review$created_time[i],"')",sep="")
  dbGetQuery(mydb,query)
}
print("Done till here")
dbDisconnect(mydb)
