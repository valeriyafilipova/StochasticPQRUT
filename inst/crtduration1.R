#'@title Critical duration
#'@description Extract observed POT (Peak over Threshold) flood events and define the critical duration based on the correlation between a set
#'of flood events over a threshold and the precipitation of the preceding days
#' @param pathmain  path to files, not needed if writeResults=FALSE
#' @param Q discharge data;  dataframe with columns date ("\%Y-\%m-\%d") and Q, if writeResults=TRUE path to csv file with columns date and Q
#' @param P precipitation data; dataframe with columns date and Pr,if writeResults=TRUE path to txt file with columns date and Pr
#' @param qtT threshold quantile for  peak over threshold events
#' @param intEvent separation time between flood events in days
#' @param PDFplots if TRUE pdf file with plots is saved in the same directory
#' @return  if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains duration and dataframes that lists the POT for each season is returned
#' @import Hmisc
#' @import cowplot
#' @import GGally
#' @import ggplot2
#' @examples
#' \dontrun{
#'  criticalduration(Q="62.5QJ.csv",P="PJ1.txt",qtT=0.9,PDFplots=TRUE,intEvent=7,writeResults=TRUE)
#'  criticalduration(Q=Qd,P=P,qtT=0.9,PDFplots=TRUE,intEvent=7,writeResults=FALSE)
#'}
#' @export

criticalduration<- function(Q,P,qtT=0.9,PDFplots=TRUE,intEvent=7,writeResults=TRUE) {


  #read in the data and create the dataframe
  if(writeResults==TRUE){
    Q=read.csv(file.path(pathmain,QJ),sep=" ")

    P=read.csv(file.path(pathmain,PJ),sep=" ")
  }

  PQ=merge(Q,P,by="date",keep="all")

  
  
  #correlate with P lag 1,2,3 and 4

  PQ$P1=filter(PQ$Pr, filter=rep(1,2), sides=1)
  PQ$P2= filter(PQ$Pr, filter=rep(1,3), sides=1)
  PQ$P3=filter(PQ$Pr, filter=rep(1,4), sides=1)
  PQ$P4=filter(PQ$Pr, filter=rep(1,5), sides=1)


  #split by month and select POT events for each season, seperated by IntEvent time

  #PQ$m=month(PQ$date)

  PQ$m=as.numeric(format(as.Date(PQ$date),"%m"))
  prptFt=subset(PQ,PQ$m==9|PQ$m==10|PQ$m==11)
  q1Ft=quantile(prptFt$Q,qtT,na.rm = TRUE)
  #Not the best way to treat missing values but for the given stations
  #will not make much difference
  prptFt=na.omit(prptFt)
  prptFt$pt2=decluster(prptFt$Q,q1Ft,r=intEvent)
  prptFt=prptFt[prptFt$pt2>q1Ft,]

  prptWt=subset(PQ,PQ$m==12|PQ$m==1|PQ$m==2)
  q1Wt=quantile(prptWt$Q,qtT,na.rm = TRUE)
  prptWt=na.omit(prptWt)
  prptWt$pt2=decluster(prptWt$Q,q1Wt,r=intEvent)
  prptWt=prptWt[prptWt$pt2>q1Wt,]


  prptSpt=subset(PQ,PQ$m==3|PQ$m==4|PQ$m==5)
  q1Spt=quantile(prptSpt$Q,qtT,na.rm = TRUE)
  prptSpt=na.omit(prptSpt)
  prptSpt$pt2=decluster(prptSpt$Q,q1Spt,r=intEvent)
  prptSpt=prptSpt[prptSpt$pt2>q1Spt,]


  prptSt=subset(PQ,PQ$m==6|PQ$m==7|PQ$m==8)
  q1St=quantile(prptSt$Q,qtT,na.rm = TRUE)
  prptSt=na.omit(prptSt)
  prptSt$pt2=decluster(prptSt$Q,q1St,r=intEvent)
  prptSt=prptSt[prptSt$pt2>q1St,]


  # Plot the correlation between the flood events
  if(PDFplots==TRUE){
    dev.set()
    pdf("crtdur.pdf")
    #hydropairs(prptFt[,1:7],main="POT events Autumn")
    ggpairs(prptFt[,2:7],title="POT events Autumn")
    ggpairs(prptSpt[,2:7],title="POT events Spring")
    ggpairs(prptWt[,2:7],title="POT events Winter")
    ggpairs(prptSt[,2:7],title="POT events Summer")

}
  #multi peak flood events can be classified to occur in two seasons,  this part of the script removes these events

  prptall=rbind(prptFt[,1:7],prptSpt[,1:7],prptWt[,1:7],prptSt[,1:7])
  prptall=prptall[order(as.Date(prptall$date, format="%Y-%m-%d")),]
  prptall$diffr=c(0,diff(prptall$date,1))
  prptall1=prptall[prptall$diffr>=0&prptall$diffr<=4,]
  prptall1=prptall1[-1,]
 # prptall1$m=month(prptall1$date)
  prptall1$m=as.numeric(format(as.Date(prptall1$date),"%m"))

  #checks the time window between the events and if less than 4, deletes the row
  if(nrow(prptall1)>0){
    nm=rep(0,nrow(prptall1))
    for(i in 1:nrow(prptall1)){
      if(prptall1$m[i]==12|prptall1$m[i]==1|prptall1$m[i]==2){
        nm[i]=match(prptall1$date[i],prptWt$date)
        if(!is.na(nm[i])){
          prptWt=prptWt[-nm[i],]
        }
      }
    }
    nm=rep(0,nrow(prptall1))
    for(i in 1:nrow(prptall1)){
      if(prptall1$m[i]==9|prptall1$m[i]==10|prptall1$m[i]==11){
        nm[i]=match(prptall1$date[i],prptFt$date)
        if(!is.na(nm[i])){
          prptFt=prptFt[-nm[i],]
        }
      }
    }
  }
  #Plot correlation for annual events
  prptallm=rbind(prptFt[,1:7],prptSpt[,1:7],prptWt[,1:7],prptSt[,1:7])
  prptallm=prptallm[order(as.Date(prptallm$date, format="%Y-%m-%d")),]
  prptallm$diffr=c(0,diff(prptallm$date,1))

  print(ggpairs(prptallm[,2:7],title ="POT events Annual"))
  cornm=as.data.frame(cor(prptallm[,2:7]))

  durtm=which.max(cornm[1,2:5])
  #durt=24
  if(durtm==1){
    durt=24
  }
  if(durtm==2){
   durt=48 
  }
  if(durtm==3){
    durt=72 
  }
  if(writeResults==TRUE){
    dir.create(paste0(pathmain,"/Exp",durt,"new"))

    write.table(durt,file.path(pathmain,"duration.txt"),row.names = FALSE)
    write.table(prptFt,paste0(pathmain,"/Exp",durt,"new/Q",durt,"Ftnew.txt"))
    write.table(prptSpt,paste0(pathmain,"/Exp",durt,"new/Q",durt,"Sptnew.txt"))
    write.table(prptWt,paste0(pathmain,"/Exp",durt,"new/Q",durt,"Wtnew.txt"))
    write.table(prptSt,paste0(pathmain,"/Exp",durt,"new/Q",durt,"Stnew.txt"))
  }else{
    res=list(d=durt,POTF=prptFt,POTW=prptWt,POTSp=prptSpt,POTS=prptSt,m=cornm)
    return(res)
  }
  if(PDFplots==TRUE){
    dev.off()
  }
}





#corSpp3=rcorr(as.matrix(nsimq[1:9]),type="spearman")$P


