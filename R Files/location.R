getLocation<- function(msg){
  locs<-read.csv('C:/Users/DELL/Desktop/csvs/locations.csv',header= TRUE, stringsAsFactors = FALSE)
  msg<- gsub('[[:punct:] ]+',' ',msg)
  s<-sapply(msg, tolower)
  msg<-removeWords(toString(s),stopwords('english'))
  df<-data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("location","latitude","longitude")
  colnames(df)<-x
  msg<-gsub("\\s+"," ",msg)
  l=""
  s=""
  wrd2=""
  wrd3=""
  wrd4=""
  flag=0
  len=vapply(strsplit(msg, "\\W+"), length, integer(1))
  words_initial<-c("traffic", "update", "alert")
  word_loc<-c("road", "street", "crossing", "sarani", "ghat", "flyover", "block", "lane", "avenue", "bazar", "bazaar", "stadium","rd","st")
  if(((sapply(words_initial, grepl, msg)[1]=="TRUE")) & ((sapply(words_initial, grepl, msg)[2]=="TRUE") | (sapply(words_initial, grepl, msg)[3]=="TRUE")))
  {
    if(sapply("school", grepl, msg)[1]=="TRUE")
    {
      flag=0
      de<-data.frame("NA","NA","NA")
      names(de)<- c("location","latitude","longitude")
      df<-rbind(df,de)
    }
    else
    {
    for(i in 1:nrow(locs))
    {
      if(length(grep(toString(locs$location[i]),msg))==1)
      {
        de<-data.frame(toString(locs$location[i]),locs$latitude[i],locs$longitude[i])
        names(de)<- c("location","latitude","longitude")
        df<-rbind(df,de)
      }
    }
    
      i=1;
      while(i<=len)
      {
      str= word(msg,i)
      if((identical(str,toString(word_loc[[1]]))==TRUE) | (identical(str,toString(word_loc[[2]]))==TRUE) | 
         (identical(str,toString(word_loc[[3]]))==TRUE) | (identical(str,toString(word_loc[[4]]))==TRUE) | 
         (identical(str,toString(word_loc[[5]]))==TRUE) | (identical(str,toString(word_loc[[6]]))==TRUE) | 
         (identical(str,toString(word_loc[[7]]))==TRUE) | (identical(str,toString(word_loc[[8]]))==TRUE) | 
         (identical(str,toString(word_loc[[9]]))==TRUE) | (identical(str,toString(word_loc[[10]]))==TRUE) | 
         (identical(str,toString(word_loc[[11]]))==TRUE) |(identical(str,toString(word_loc[[12]]))==TRUE) | 
         (identical(str,toString(word_loc[[13]]))==TRUE) |(identical(str,toString(word_loc[[14]]))==TRUE))
      {
        if((identical(str,toString(word_loc[[1]]))==TRUE) | (identical(str,toString(word_loc[[2]]))==TRUE) | 
           (identical(str,toString(word_loc[[4]]))==TRUE) | 
           (identical(str,toString(word_loc[[5]]))==TRUE) | (identical(str,toString(word_loc[[6]]))==TRUE) | 
           (identical(str,toString(word_loc[[7]]))==TRUE) | (identical(str,toString(word_loc[[8]]))==TRUE) | 
           (identical(str,toString(word_loc[[9]]))==TRUE) | (identical(str,toString(word_loc[[10]]))==TRUE) | 
           (identical(str,toString(word_loc[[11]]))==TRUE) |(identical(str,toString(word_loc[[12]]))==TRUE) | 
           (identical(str,toString(word_loc[[13]]))==TRUE) |(identical(str,toString(word_loc[[14]]))==TRUE))
        {
        str1=word(msg,i+1)
        if((identical(str1,toString(word_loc[[3]]))==TRUE))
        {
          if(i>=2)
            st2= word(msg,i-1)
          else
            st2="NA"
          if(i>=3)
            st3=word(msg,i-2,i-1)
          else
            st3="NA"
          if(i>=4)
            st4=word(msg,i-3,i-1)
          else
            st4="NA"
          str=paste(str,str1,sep=" ")
          i=i+1
        }
        else
        {
          if(i>=2)
            st2=word(msg,i-1)
          else
            st2="NA"
          if(i>=3)
            st3=word(msg,i-2,i-1)
          else
            st4="NA"
          if(i>=4)
            st4=word(msg,i-3,i-1)
          else
            st4="NA"
        }
        }
        else
        {
          if(i>=2)
            st2=word(msg,i-1)
          else
            st2="NA"
          if(i>=3)
            st3=word(msg,i-2,i-1)
          else
            st3="NA"
          if(i>=4)
            st4=word(msg,i-3,i-1)
          else
            st4="NA"
        }
        if(identical(toString(st2),"NA")==FALSE)
        {
        st2<- removeWords(toString(st2),words_initial)
        st2<-removeWords(toString(st2),word_loc)
        st2<-gsub("\\s+"," ",st2)
        if(identical(st2,"")==FALSE && identical(st2," ")==FALSE)
          wrd2=paste(st2,str,sep=" ")
        else
          wrd2=""
        }
        else
        {
          wrd2=""
        }
        if(identical(toString(st3),"NA")==FALSE)
        {
          st3<- removeWords(toString(st3),words_initial)
          st3<-removeWords(toString(st3),word_loc)
          st3<-gsub("\\s+"," ",st3)
          if(identical(st3,"")==FALSE && identical(st3," ")==FALSE)
            wrd3=paste(st3,str,sep=" ")
          else
            wrd3=""
        }
        else
        {
          wrd3=""
        }
        if(identical(toString(st4),"NA")==FALSE)
        {
          st4<- removeWords(toString(st4),words_initial)
          st4<-removeWords(toString(st4),word_loc)
          st4<-gsub("\\s+"," ",st4)
          if(identical(st4,"")==FALSE && identical(st4," ")==FALSE)
            wrd4=paste(st4,str,sep=" ")
          else wrd4=""
        }
        else
        {
          wrd4=""
        }
        flag=0
        for(j in 1:nrow(locs))
        {
          if(identical(toString(locs$location[j]),wrd2)==TRUE | identical(toString(locs$location[j]),wrd2)==TRUE | identical(toString(locs$location[j]),wrd2)==TRUE)
          {
            flag=1
            de<-data.frame(toString(locs$location[j]),locs$latitude[j],locs$longitude[j])
            names(de)<- c("location","latitude","longitude")
            df<-rbind(df,de)
          }
        }
          if(flag==0)
          {
            k=1;
            while(k<5 && flag==0 && (identical(wrd2,"")==FALSE) && (identical(wrd2,"NA")==FALSE))
            {
              s<-locationFunction(wrd2)
              if(identical(toString(s$lat),"NA")==TRUE)
              {
                k=k+1
                Sys.sleep(7)
              }
              else 
              {
                l=wrd2;
                de<-data.frame(toString(wrd2),s$lat,s$lon)
                names(de)<- c("location","latitude","longitude")
                df<-rbind(df,de)
                locs<-rbind(locs,de)
                k=8;
                flag=1;
              }
            }
            k=1;
            while(k<5 && flag==0 && (identical(wrd3,"")==FALSE) && (identical(wrd3,"NA")==FALSE))
            {
              s<-locationFunction(wrd3)
              if(identical(toString(s$lat),"NA")==TRUE)
              {
                k=k+1
                Sys.sleep(7)
              }
              else 
              {
                l=wrd3;
                de<-data.frame(toString(wrd3),s$lat,s$lon)
                names(de)<- c("location","latitude","longitude")
                df<-rbind(df,de)
                k=8;
                flag=1;
              }
            }
            k=1;
            while(k<5 && flag==0 && (identical(wrd4,"")==FALSE) && (identical(wrd4,"NA")==FALSE))
            {
              s<-locationFunction(wrd4)
              if(identical(toString(s$lat),"NA")==TRUE)
              {
                k=k+1
                Sys.sleep(7)
              }
              else 
              {
                l=wrd4;
                de<-data.frame(wrd4,s$lat,s$lon)
                names(de)<- c("location","latitude","longitude")
                df<-rbind(df,de)
                k=8;
                flag=1;
              }
            }
          }
      }
      i=i+1;
      }
      if(flag==1)
      {
        df<-df[!duplicated(df$location),]
        locs<-rbind(locs,df)
        locs<-locs[!duplicated(locs$location),]
        write.csv(locs,'C:/Users/DELL/Desktop/csvs/locations.csv',row.names= FALSE)
      }
      else
      {
        de<-data.frame("NA","NA","NA")
        names(de)<- c("location","latitude","longitude")
        df=rbind(df,de)
      }
    }
  }
  else
  {
    de<-data.frame("NA","NA","NA")
    names(de)<- c("location","latitude","longitude")
    df=rbind(df,de)
  }
  return(df)
}