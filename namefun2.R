library(tidyverse)

df<-NULL
for (y in c(1880:2021))
{
  file=paste("/Users/wushuangyan/Desktop/Projects/Names/National/yob",y,".txt",sep="")
  ndata=read_csv(file,na=c(" ","NA","NULL"), col_names = FALSE)

  ndata1<-mutate(ndata, Year=rep(y,length(ndata$X1)))
  df<-rbind(df, ndata1)                                  
} 

save(df,file="US_saved")

load(file="US_saved")



