#' @title Extracts initial conditions for POT events
#' @description Initial condions (initial discharge, soil moisture deficit and SWE) for POT flood events
#' @param pathmain path to files, not needed if writeResults=FALSE
#' @param this.station station number,, not needed if writeResults=FALSE
#' @param durt critical duration
#' @param Qobs discharge data; dataframe with columns date ("\%Y-\%m-\%d") and Q,if writeResults=TRUE path to txt file with columns date and Q
#' @param Td temperature data; dataframe with columns date("\%Y-\%m-\%d") and Td,if writeResults=TRUE path to txt file with columns date and Td
#' @param sl soil moisture deficit data; dataframe with columns date("\%Y-\%m-\%d") and sl,if writeResults=TRUE path to txt file with columns date and sl
#' @param SWE snow water equivalent data; dataframe with columns date("\%Y-\%m-\%d") and swe,if writeResults=TRUE path to txt file with columns date and swe
#' @param POTF,POTW,POTSp,POTS  results from criticalduration, not needed if writeResults=TRUE
#' @param PDFplots if TRUE pdf file with plots is saved in the same directory
#' @return if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains data tables of initial conditions for the POT flood events is returned
#' @examples
#'  \dontrun{
#'  b=initcond(Qobs=Q,sl=sl,POTF=a$POTF,POTW=a$POTW,POTSp=a$POTSp,POTS=a$POTS,SWE=SWE,Td=Td,durt=a$d,PDFplots=FALSE,writeResults=FALSE)
#'  }
#' @export
#' @include Asmoothcor.R
initcond <- function(pathmain=NULL,this.station=NULL,Qobs, Td,sl,POTF,POTW,POTSp,POTS, SWE,durt,PDFplots=FALSE,writeResults=TRUE) {


  # if(PDFplots==TRUE){
  #   dev.set()
  #   pdf(paste0("./initcondExp",durt,"/corsn.pdf"),onefile=TRUE)
  # }

  #read values and subset by season

  if(writeResults==TRUE){
    setwd(pathmain)
    dir.create(paste0("./initcondExp",durt))
    SWE=read.table(file.path(pathmain,"/SeNorge/swe.csv"),header=TRUE)
    sl=read.csv(file.path(pathmain,"/sm1.csv"),header=TRUE,sep=" " )
    sl$X=NULL
    Qobs=read.csv(file.path(pathmain,paste0(this.station,"QJ.csv")),sep=" ")
    Td=read.table(file.path(pathmain,"Td.txt"),header=TRUE)
    POTF=read.table(paste0("./Exp",durt,"new/Q",durt,"Ftnew.txt"))
    POTW=read.table(paste0("./Exp",durt,"new/Q",durt,"Wtnew.txt"))
    POTSp=read.table(paste0("./Exp",durt,"new/Q",durt,"Sptnew.txt"))
    POTS=read.table(paste0("./Exp",durt,"new/Q",durt,"Stnew.txt"))
  }

  #Merge into a dataframe
  sl$date=as.Date(sl$date,"%Y-%m-%d")
 SWE$date=as.Date(SWE$date,"%Y-%m-%d")
  
  seNorge1=merge(Qobs,SWE,by="date",keep="all")
  seNorge2=merge(seNorge1,sl,by="date",keep="all")
  smn1=merge(seNorge2,Td,by="date",keep="all")
  smn1$Date=as.Date(smn1$date,"%Y-%m-%d")


  POTF$Date=as.Date(POTF$date,"%Y-%m-%d")
  POTW$Date=as.Date(POTW$date,"%Y-%m-%d")
  POTSp$Date=as.Date(POTSp$date,"%Y-%m-%d")
  POTS$Date=as.Date(POTS$date,"%Y-%m-%d")

 #Match the POT events to the initial conditions
  ndateF=match(POTF$Date,smn1$Date)
  ndateW=as.numeric(na.omit(match(POTW$Date,smn1$Date)))
  ndateSp=as.numeric(na.omit(match(POTSp$Date,smn1$Date)))
  ndateS=as.numeric(na.omit(match(POTS$Date,smn1$Date)))

  ntp2=data.frame(qobs=numeric(),SWE=numeric(),sl=numeric(),date=character())
  ntp1=data.frame(qobs=numeric(),SWE=numeric(),sl=numeric(),date=character())

  ntp2F=ntp2
  ntp1F=ntp1
  ntp2W=ntp2
  ntp1W=ntp1
  ntp2Sp=ntp2
  ntp1Sp=ntp1
  ntp2S=ntp2
  ntp1S=ntp1

  #extract the values for the dates where the POT events are observed

  for(i in 2:length(ndateF)){
    ntp2F=cbind(smn1[(ndateF[i]-(durt/24+1)),1:5])
    ntp1F=rbind(ntp1F,ntp2F)

  }
  #if(PDFplots==TRUE){
    #hydropairs(ntp1F[c(2:4)])
  p1=  ggpairs(ntp1F[c(2:4)],title="season=9-11")
#  }
  for(i in 2:length(ndateW)){
    ntp2W=cbind(smn1[(ndateW[i]-(durt/24+1)),1:5])
    ntp1W=rbind(ntp1W,ntp2W)
  }
 # if(PDFplots==TRUE){
   p2= ggpairs(ntp1W[c(2:4)],title="season=12-2")
 # }
  for(i in 2:length(ndateSp)){
    ntp2Sp=cbind(smn1[(ndateSp[i]-(durt/24+1)),1:5])
    ntp1Sp=rbind(ntp1Sp,ntp2Sp)
  }
 # if(PDFplots==TRUE){
   p3=ggpairs(ntp1Sp[c(2:4)],title="season=3-5")
 # }
  for(i in 2:length(ndateS)){
    ntp2S=cbind(smn1[(ndateS[i]-(durt/24+1)),1:5])
    ntp1S=rbind(ntp1S,ntp2S)
  }
 # if(PDFplots==TRUE){
    p4=ggpairs(ntp1S[c(2:4)],title="season=6-8")
 #   dev.off()
 # }
   print( plot_grid(
      ggmatrix_gtable(p1),
      ggmatrix_gtable(p2),
      ggmatrix_gtable(p3),
      ggmatrix_gtable(p4),
      nrow = 2
    ))
  ntp1F=na.omit(ntp1F)
  ntp1W=na.omit(ntp1W)
  ntp1Sp=na.omit(ntp1Sp)
  ntp1S=na.omit(ntp1S)

  if(writeResults==TRUE){
    dir.create(paste0(pathmain,"/initcondExp",durt))
    write.table(ntp1F,paste0(pathmain,"/initcondExp",durt,"/initcond1Ft.txt"))
    write.table(ntp1W,paste0(pathmain, "/initcondExp",durt,"/initcond1Wt.txt"))
    write.table(ntp1Sp,paste0(pathmain,"/initcondExp",durt,"/initcond1Spt.txt"))
    write.table(ntp1S,paste0(pathmain,"/initcondExp",durt,"/initcond1St.txt"))
  }else{

    res=list(ntp1F=ntp1F,ntp1W=ntp1W,ntp1Sp=ntp1Sp,ntp1S=ntp1S)
    return(res)
}
}




