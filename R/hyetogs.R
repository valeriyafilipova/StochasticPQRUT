

#' @title Extracts a set of hyetographs for POT events
#' @description The function extracts hyetographs for the POT rainfall events
#' @param pathmain path to data, not needed if writeResults=FALSE
#' @param durt critical duration
#' @param prp output from rainPOT
#' @param prpFt,prpWt,prpSpt,prpSt  dataframes that contain the POT for each season, not needed if writeResults=TRUE path
#' @param p1 dataframe of precipitation data with 1 hour timestep with columns Date "\%Y-\%m-\%d", time=\%H:\%M:\%S" and value)
#' @return if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains  dataframes of hyetographs for each season is returned
#' @examples
#' \dontrun{
#'   h=stormp(prpFt=d$pF,prpWt=d$PW,prpSpt=d$PSp,prpSt=d$PS,p1,durt=24,writeResults=FALSE,PDFplots=FALSE)
#' }
#' @export
stormp <- function(pathmain=NULL,prpFt,prpWt,prpSpt,prpSt,p1,durt,writeResults=TRUE,PDFplots=TRUE) {
  #stormp(pathmain="F:/FlomQ/62.5",this.station="62.5",durt=24)

  if(writeResults==FALSE){
    alist=list(prpFt,prpWt,prpSpt,prpSt)
  }
  seasnlist=1

  if(durt=="24"){
    seasn=c("Ft","Wt","Spt","St")

    for(sesn in seasn){

      if(writeResults==TRUE){
        setwd(pathmain)
        prp=read.table(paste("./Exp24new/P24",sesn,"new.txt",sep=""),sep=",",header=TRUE)
        p1=read.table(paste0("3-timerP/",this.station,"hp1.txt"),stringsAsFactors = TRUE,skip=1)
        colnames(p1) = c("V1","V2", "V3")
      }else{
       prp=alist[[seasnlist]]
       colnames(p1) = c("V1","V2", "V3")
      }
      pM=p1
      prp$Date=strptime(prp$Date,format="%Y-%m-%d")

      pM$Date=paste(pM$V1,pM$V2)
      pM$Date=strptime(pM$Date,format="%Y-%m-%d %H:%M:%S")
      pM$Day=trunc.POSIXt(pM$Date,unit="day")
      pM$V1=NULL
      pM$V2=NULL

      #Match timeseries
      prp=prp[prp$Date>=pM$Date[1]&prp$Date<=pM$Date[nrow(pM)],]
      n=match(prp$Date,pM$Day)
      if(writeResults==TRUE){
      dir.create("./tpstormExp24")
      setwd("./tpstormExp24/")
      dir.create(paste("./",sesn,sep=""))
      setwd(paste("./",sesn,sep=""))
    }
      par(mfrow=c(5,5))
      if(writeResults==FALSE){
        a=list()

    }
      j=1
      for(i in 1:nrow(prp)){

        p2=pM[(n[i]-(18)):(n[i]+5),]

        sump1=sum(p2$V3,na.rm=TRUE)
        if(sump1>1){
          j=j+1
          p2$p=p2$V3
          if(PDFplots==TRUE){
            pdf("stpl.pdf",onefile=TRUE)
            x1=barplot(p2$p)
            axis(1,at=x1,labels=seq(1:24))
            dev.off()
          }
          if(writeResults==TRUE){
          write.table(p2,paste("st",j,".txt",sep=""))
          }else{
          a[i]=p2
          }
        }
      }
      if(writeResults==TRUE){
      write.table(j,"ns.txt")
      }else{
      assign(x = sesn,value = a)
      seasnlist= seasnlist+1

      }
    }

  }else{
    seasn=c("Ft","Wt","Spt","St")
    for(sesn in seasn){
      #match timeseries and plot events

      prp$Date=strptime(prp$Date,format="%Y-%m-%d")

      pM$Date=paste(pM$V1,pM$V2)
      pM$Date=strptime(pM$Date,format="%Y-%m-%d %H:%M:%S")
      pM$Day=trunc.POSIXt(pM$Date,unit="day")
      pM$V1=NULL
      pM$V2=NULL
      prp=prp[prp$Date>=pM$Date[1]&prp$Date<=pM$Date[nrow(pM)],]
      n=match(prp$Date,pM$Day)
      if(writeResults==TRUE){
      dir.create(paste0("./tpstormExp",durt))
      setwd(paste0("./tpstormExp",durt,"/"))
      dir.create(paste("./",sesn,sep=""))
      setwd(paste("./",sesn,sep=""))

      }
    par(mfrow=c(1,1))
      pdf("stpl.pdf",onefile=TRUE)

      j=0
      m=NULL
      for(i in 1:nrow(prp)){

        p2=pM[(n[i]-(18+24)):(n[i]+5+24),]
        mdurt=rep(24,(durt/24-2))
        if(length(mdurt)!=0){
          p2=pM[(n[i]-(18+rep(24,(durt/24-2)))):(n[i]+5+24),]
        }else{
          p2=pM[(n[i]-(18)):(n[i]+5+24),]
        }
        sump1=sum(p2$V3,na.rm=TRUE)
        if(sump1>1){


          pnb=p2
          if(PDFplots==TRUE){
            x1=barplot(p2$V3)
            axis(1,at=x1,labels=seq(1:durt))
            dev.off()
          }

          j=j+1
          if(writeResults==TRUE){
          write.table(p2,paste("st",j,".txt",sep=""))
          }else{
            a[i]=p2
          }
          m=c(m,j)
        }
      }

      if(writeResults==TRUE){
      write.table(j,"ns.txt")
      }else{
      assign(x = sesn,value = a)
      seasnlist= seasnlist+1
    }

  }
  }
  if(writeResults==FALSE){
     res=list(HFt=Ft,HWt=Wt,HSpt=Spt,HSt=St)
  }

}

