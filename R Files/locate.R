library(ggmap)
locationFunction<- function(s2)
{
  s2<-paste(s2,"Kolkata",sep=", ")
  gg<-geocode(s2)
  return(gg)
}