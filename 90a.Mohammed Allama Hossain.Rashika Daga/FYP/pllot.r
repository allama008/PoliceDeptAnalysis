function(a,kol){
a<-gsub('[[:punct:]]+', ' ',a)

a<-gsub("[\r\n]", "",a)
str<- removeWords(c(a), stopwords("english"))
stir<- gsub("\\s+", " ", toString(str))
s<- toString(stir)
no<- sapply(strsplit(s, " "), length)
flag=0
final1<- "NA"
final2<- "NA"
final3<- "NA"
if(((strsplit(s, " ")[[1]][1]=="Traffic")|(strsplit(s, " ")[[1]][1]=="traffic")) & ((strsplit(s, " ")[[1]][2]=="Update")|(strsplit(s, " ")[[1]][2]=="update"))){
for(i in 3:no){
z<-strsplit(s, " ")[[1]][i]
if(z== "Road" | z=="Street" | z== "Avenue" | z=="Lane" | z=="Park" | z=="flyover" | z=="Bazar"){
final1<- paste(strsplit(s," ")[[1]][i-1],z,sep=" ")
final2<- paste(final1,"Kolkata",sep=",")
final3<- paste(strsplit(s," ")[[1]][i-2], final2, sep=" ")
geo2<- geocode(final2)
geo3<- geocode(final3)
if(!(is.na(geo3$lat))){
d<- data.frame(lat= c(geo3$lat), lon=c(geo3$lon))
kol<-kol + geom_point(data = d, aes(x = lon, y = lat), color="red", size=3, alpha=0.5)
print(final3)
flag=1
}
else if(!(is.na(geo2$lat))){
d<- data.frame(lat= c(geo2$lat), lon=c(geo2$lon))
kol<-kol + geom_point(data = d, aes(x = lon, y = lat), color="red", size=3, alpha=0.5)
print(final2)
flag=1
}
else{
flag=0
}
}
}
if(flag==0){
for(i in 3:no){
z<-strsplit(s," ")[[1]][i]
temp<- read.csv("Desktop/locations.csv")
temp1<-as.matrix(temp)
for(k in 1:nrow(temp1)){
if(length(grep(temp1[k],c(z)))!=0){
fin<- paste(z,"Kolkata",sep=",")
geoc<- geocode(fin)
d<- data.frame(lat= c(geoc$lat), lon=c(geoc$lon))
kol<-kol + geom_point(data = d, aes(x = lon, y = lat), color="red", size=3, alpha=0.5)
print(fin)
}
}
}
}
}
return(kol)
}
