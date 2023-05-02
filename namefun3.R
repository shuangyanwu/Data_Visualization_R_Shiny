library(tidyverse)
library(dplyr)

load(file="US_saved")

linep<-function(loc, sex, range, t1,t2,t3,t4,t5,t6,t7,t8){
  
  all<-c('US', 'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 
         'Colorado', 'Connecticut', 'Delaware','District of Columbia', 'Florida', 
         'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas',
         'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 
         'Minnesota', 'Mississippi', 'Missouri', 'Montana', 
         'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 
         'North Carolina', 'North Dakota', 'Ohio', 
         'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 
         'South Carolina', 'South Dakota', 'Tennessee', 'Texas',
         'Utah', 'Vermont', 'Virginia', 'Washington', 
         'West Virginia', 'Wisconsin', 
         'Wyoming')
  
  short = c("US",'AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE','DC', 'FL', 
            'GA', 'HI', 'ID', 'IL', 'IN','IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 
            'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 
            'NY', 'NC', 'ND','OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 
            'UT','VT', 'VA', 
            'WA','WV', 'WI', 'WY')
  
  
  c<-c('F','M')
  b<-c('girls','boys')
  d<-c('Girls','Boys')
  
  
  if(loc=="US"){
    
    df2<-df[df$X2==sex,];df2
    df21<-df2[df2$Year>=min(range) & df2$Year<=max(range),];df21
    
    sm<-summarise (group_by(df21,Year), tot= sum(X3,na.rm=T))
    rept<-table(df21$Year);
    tc<-rep(sm$tot,rept);
    df22<-mutate (df21, Pct= df21$X3/tc*100);df22

    df3<-df22[df22$X1%in% c(t1, t2, t3, t4, t5, t6, t7, t8), ];
    if (dim(df3)[1] == 0){
      return (NULL)
    }
    else {
    whe<-which(c==sex)
    lab<-b[whe]
    lab2<-d[whe]
    names(df3) <- c("name", "X2", "X3", "Year","Pct")
    
    ggplot(data = df3) + 
      geom_line(mapping = aes(x = Year, y =Pct, color=name),size=2)+   
      xlab("year")+
      ylab(c(paste("percent of all", lab, "with name")))+
      ggtitle(c(paste("Popularity of", lab2, "Names in US")))+
      theme(axis.text=element_text(size=16),
            title =element_text(size=20),
            axis.title=element_text(size=20),
            legend.title = element_text(size=19),
            legend.text = element_text(size=17))
    }
  }
  
  else {

    abb<-short[which(all==loc)];abb
    file=paste(abb,".TXT",sep="")
    
    namedata=read_csv(file,na=c(" ","NA","NULL"),col_names = FALSE);
    
    namedata<- namedata %>%
      arrange(X2, X3, desc(X5));namedata
    
    namedata00<-namedata[namedata$X2==sex,];namedata00
    
    namedata0<-namedata00[namedata00$X3>=min(range) & namedata00$X3<=max(range),];namedata0
    
    sm<-summarise (group_by(namedata0,X3), tot= sum(X5,na.rm=T));
    rept<-table(namedata0$X3);
    tc<-rep(sm$tot,rept);
    namedata1<-mutate (namedata0, X6=namedata0$X5/tc*100);namedata1
    
    namedata2=filter(namedata1,X4==t1|X4==t2|X4==t3|X4==t4|X4==t5|X4==t6|X4==t7|X4==t8) ;namedata2
    
    if (dim(namedata2)[1] == 0){
      return (NULL)
    }
    else{
    names(namedata2) <- c("X1","X2","X3","name","X5","X6")
    whe<-which(c==sex)
    lab<-b[whe]
    lab2<-d[whe]
    
    ggplot(data = namedata2) + 
      geom_line(mapping = aes(x = X3, y = X6, color=name),size=2)+  
      xlab("year")+
      ylab(c(paste("percent of all", lab, "with name")))+
      ggtitle(c(paste("Popularity of", lab2, "Names in", loc)))+
      theme(axis.text=element_text(size=16),
            title =element_text(size=20),
            axis.title=element_text(size=20),
            legend.title = element_text(size=19),
            legend.text = element_text(size=17))
    }
  }
}

# linep('US', "F", c(1880: 2021),'Kyle','Joe','Anne',"Eva", "Amy","","Jessica","LauE")
