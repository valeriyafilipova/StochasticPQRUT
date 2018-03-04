

#' Extracts temperature sequence for POT events
#'
#' @param pathmain path to files , not needed if writeResults=FALSE
#' @param durt critical duration in hours
#' @param incondFt,incondWt,incondSpt,incondSt dataframes that contain the POT for each season, not needed if writeResults=TRUE
#' @param tm dataframe of temperature data with 1 hour timestep with columns Date "\%Y-\%m-\%d", time=\%H:\%M:\%S" and value)
#' @return returns csv files that contain temperature series for POT events
#' @export
#' @examples
#' \dontrun{
#' h1=temprsim(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S,durt=24,tm,writeResults=FALSE,PDFplots=FALSE)
#' }
temprsim <- function(pathmain=NULL,incondFt,incondWt,incondSpt,incondSt,durt,tm,writeResults=FALSE,PDFplots=FALSE) {
 # temprsim("F:/FlomQ/62.5",durt=24)
  ## Set path to datafiles
  if(writeResults==TRUE){
  setwd(pathmain)
  dir.create("./TE1")
  setwd("./TE1")
  }

  seasn1=c("Ft","Wt","Spt","St")
  if(writeResults==FALSE){
    alist=list(incondFt,incondWt,incondSpt,incondSt)
  }
  seasnlist=1
  for(seasn in seasn1){
    if(writeResults==TRUE){
  int1=read.table(paste(pathmain,"/initcondExp",durt,"/initcond1",seasn,".txt",sep=""))
    }else{
      int1=alist[[seasnlist]]

    }

 #match with hourly time series
    if(writeResults==TRUE){
nptath=paste(pathmain,"/3-timerP/ht1.txt",sep="")
 tm=read.csv(nptath,row.names = NULL,sep=" ")
    }
    tmM=tm
    tmM$Date=strptime(tmM[,1],"%Y-%m-%d")

     int1$Date=strptime(int1$date,"%Y-%m-%d")
   int1=subset(int1,int1$Date>=tmM$Date[1]&int1$Date<=tmM$Date[nrow(tmM)])
    tmM=data.frame(date=tmM$Date,E=tmM$T)
    row.names(int1)=NULL
    if(writeResults==FALSE){
      a=list()

    }
    for(k in 1:nrow(int1)){
  nsnd=match(as.Date(int1$Date[k]),as.Date(tmM$date))
  tmM1=try(tmM[(nsnd-20-durt+48):(nsnd+3+24),])
  if(class(tmM1)!="try-error"){
  row.names(tmM1)=NULL

  if(writeResults==TRUE){
    dir.create(paste("./",seasn,sep=""))
  write.table(tmM1,paste("./",seasn,"/","ts",k,".txt",sep=""))
  }else{
    a[[k]]=tmM1$E
  }
  }
    }
    if(writeResults==FALSE){
      assign(x = seasn,value = a)
      seasnlist= seasnlist+1
    }
  }
  if(writeResults==FALSE){
    res=list(HFt=Ft,HWt=Wt,HSpt=Spt,HSt=St)
  }
}
