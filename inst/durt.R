
# # # # # library(rgdal)
st.list=read.table("F:/testSFM/stationlistA3.txt",header=TRUE,colClasses = c("character","character","numeric"))
library(extRemes)
library(GGally)
durt=NULL
j=1
for(i in st.list$St.){

  pathmain=paste0("F:/testSFM/",i)
  this.station=i
  setwd(pathmain)


  P=read.table("PJ3.txt",header = TRUE)
  colnames(P)=c("date","Pr")
  P$date=as.Date(P$date)
  Q=read.table(paste0(i,".csv"))
  colnames(Q)=c("date","Q")
  Q$date=as.Date(Q$date,format="%Y%m%d/ %H%M")
  a=criticalduration(Q=Q,P=P,qtT=0.9,PDFplots=FALSE,intEvent=7,writeResults=FALSE)
  #criticalduration(pathmain = pathmain,durt=72,qtT=0.9)
  durt[j]=a$d
  
  j=j+1
}
  