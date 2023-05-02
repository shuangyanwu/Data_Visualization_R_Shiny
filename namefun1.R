library(tidyverse)
library(dplyr)

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

name<- function (loc, sex, value, year) 
  {
  
  if (loc == 'US') {
    
    file=paste("yob",year,".txt",sep="")
    
    nstate <- read_csv(file, col_names = FALSE, na=c(" ","NA","NULL")); nstate
    nstate0 <- nstate [nstate$X2==sex,]; 
    sn<-sum(nstate0$X3)
    nstate1 <- nstate0 [1:value,] 
    nstate2<-arrange(nstate1, X3)
    nstate2$X1 <- factor(nstate2$X1, levels = nstate2$X1)
    
    ggplot(data=nstate2, aes(x=X1, y=(X3/sn*100))) +
      geom_bar(stat="identity",fill="red")  + coord_flip()+
      labs(title=c(paste("Top",value,"Most Popular Names in",loc,"in",year)),
           y="Percent of babies", x= "names")+
      theme(axis.text=element_text(size=16),
            title =element_text(size=20),
            axis.title=element_text(size=19,))
  }
  
  else {
    
    abb<-short[which(all==loc)]
    file=paste(abb,".TXT",sep="")
    
    nstate <- read_csv(file, col_names = FALSE, na=c(" ","NA","NULL")); 
    nstate0 <- nstate [nstate$X2==sex & nstate$X3==year,]; 
    sn<-sum(nstate0$X5)
    nstate1 <- nstate0 [1:value,];  
    nstate2<-arrange(nstate1, X5);
    nstate2$X4 <- factor(nstate2$X4, levels = nstate2$X4)
    
    ggplot(data=nstate2, aes(x=X4, y=(X5/sn*100))) +
      geom_bar(stat="identity",fill="red")  + coord_flip()+
      labs(title=c(paste("Top",value,"Most Popular Names in",loc,"in",year)),
           y="Percent of babies", x= "names")+
      theme(axis.text=element_text(size=16),
            title =element_text(size=20),
            axis.title=element_text(size=19,))
  }
} 


