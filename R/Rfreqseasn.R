
#' @title Fit precipitation model
#' @description Fits the GP/Exponential distribution to peak over threshold precipitation data by splitting the data into different seasons
#'
#' @param datarainfall  precipitation data; dataframe with columns date ("\%Y-\%m-\%d") and Pr,if writeResults=TRUE path to txt file with columns date and Pr
#' @param pathmain path to file , not needed if writeResults=FALSE
#' @param durt critical duration in hours
#' @param qFtset quantile threshold for Autumn
#' @param qWtset quantile threshold for Winter
#' @param qSptset quantile threshold for Spring
#' @param qStset quantile threshold for Summer
#' @param distfunc  distribution function GP or Exponential
#'
#' @param PDFplots if TRUE pdf file with plots is saved in the same directory
#'
#' @import extRemes
#' @return if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains parameters and dataframes that lists the POT for each season is returned
#'
#' @examples
#' \dontrun{
#' d=rainPOT(datarainfall=P,durt=24,qFtset=0.9,qWtset=0.95,qSptset=0.92,qStset=0.9,distfunc="GP",writeResults=FALSE,PDFplots=FALSE)
#' }
#' @export

rainPOT <- function(pathmain=NULL,datarainfall="P.txt",durt,qFtset=0.9,qWtset=0.95,qSptset=0.92,qStset=0.9,distfunc,writeResults=TRUE,PDFplots=TRUE) {
  if(PDFplots==FALSE){
    par(mfrow=c(2,2))
  }

  if(writeResults==TRUE){
    dir.create(paste0(pathmain,"/Exp",durt,"new"))
  pt=read.table(file.path(pathmain,datarainfall),header=TRUE,stringsAsFactors = FALSE)


  }else{
    pt=datarainfall
  }

  pt$Date=strptime(pt$date,format="%Y-%m-%d")


 #Aggregate data if critical duration > 1day
  if(durt>24){
    P3=c(0,rollapply(pt$Pr,durt/24,sum))
    if(length(P3)==nrow(pt)){
      pt$P3=P3
    }else {
      P3=c(P3,rep(0,(nrow(pt)-length(P3))))
    pt$P3=P3
    }
  }else{
    pt$P3=pt$Pr
  }
    #pt$m=month(pt$Date)
    pt$m=as.numeric(format(pt$Date,"%m"))


    #Subset for seasons
  #  pt$year=year(pt$Date)
    pt$year=as.numeric(format(pt$Date,"%Y"))
    ptF=subset(pt,pt$m==9|pt$m==10|pt$m==11)
    ptW=subset(pt,pt$m==12|pt$m==1|pt$m==2)
    ptSp=subset(pt,pt$m==3|pt$m==4|pt$m==5)
    ptS=subset(pt,pt$m==6|pt$m==7|pt$m==8)

    #EXponential,GP  distribution for each season
    ptFq=quantile(ptF$P3,qFtset)

    if(PDFplots==TRUE){
    mrlplot(ptF$P3)
    threshrange.plot(ptF$P3)
    }

    ptF$Pcl=decluster(x=ptF$P3,threshold=ptFq,r=4)

    #around 92 days for a season (three months)
    a=as.character(92)
    if(distfunc=="Exponential"){
      ptFGPexp=fevd(x=ptF$Pcl,threshold=ptFq,type="Exponential",time.units=paste0(a,"/year"))
      a1F=summary(ptFGPexp)
      a1F=a1F$par

    }else{
      ptFGPexp=fevd(x=ptF$Pcl,threshold=ptFq,type="GP",time.units=paste0(a,"/year"),method="Lmoments")
      a1F=summary(ptFGPexp)
    }
    if(PDFplots==FALSE){
      par(mfrow=c(2,2))
    }
    ptFt=ptF[ptF$Pcl>ptFq,]
    plot(ptFGPexp,type = "rl",main="09-11")

      #Seasonal Max
    ptWq=quantile(ptW$P3,qWtset)
    if(PDFplots==TRUE){
    mrlplot(ptW$P3)
    threshrange.plot(ptW$P3)
    }
    ptW$Pcl=decluster(x=ptW$P3,threshold=ptWq,r=4)
    ptWt=ptW[ptW$Pcl>ptWq,]
    if(distfunc=="Exponential"){
      ptWGPexp=fevd(x=ptW$Pcl,threshold=ptWq,type="Exponential",time.units=paste0(a,"/year"))
      a1W=summary(ptWGPexp)
      a1W=a1W$par
      }else{
      ptWGPexp=fevd(x=ptW$Pcl,threshold=ptWq,type="GP",time.units=paste0(a,"/year"),method="Lmoments")
      a1W=summary(ptWGPexp)
      }
    plot(ptWGPexp,type="rl",main="12-02")


    ptSpq=quantile(ptSp$P3,qSptset)
    if(PDFplots==TRUE){
    mrlplot(ptSp$P3)
    }

    ptSp$Pcl=decluster(x=ptSp$P3,threshold=ptSpq,r=4)
    if(distfunc=="Exponential"){
      ptSpGPexp=fevd(x=ptSp$Pcl,threshold=ptSpq,type="Exponential",time.units=paste0(a,"/year"))
      a1Sp=summary(ptSpGPexp)
      a1Sp=a1Sp$par

      }else{
      ptSpGPexp=fevd(x=ptSp$Pcl,threshold=ptSpq,type="GP",time.units=paste0(a,"/year"),method="Lmoments")
      a1Sp=summary(ptSpGPexp)
      #a1Sp=a1Sp$par
      }
    plot(ptSpGPexp,type="rl",main="03-05")
    ptSpt=ptSp[ptSp$Pcl>ptSpq,]
   # a1Sp=summary(ptSpGPexp)


    ptSq=quantile(ptS$P3,qStset)
    if(PDFplots==TRUE){
    mrlplot(ptS$P3)
    threshrange.plot(ptS$P3)
    }
    ptS$Pcl=decluster(x =ptS$P3,threshold=ptSq,r=4)
    if(distfunc=="Exponential"){
      ptSGPexp=fevd(x=ptS$Pcl,threshold=ptSq,type="Exponential",time.units=paste0(a,"/year"))
      a1S=summary(ptSGPexp)
      a1S=a1S$par
      }else{
      ptSGPexp=fevd(x=ptS$Pcl,threshold=ptSq,type="GP",time.units=paste0(a,"/year"),method="Lmoments")
      a1S=summary(ptSGPexp)

      }
    plot(ptSGPexp,type="rl",main="06-08")
    ptSt=ptS[ptS$Pcl>ptSq,]
   # a1S=summary(ptSGPexp)



    if(is.na(a1F[2])){
      par72=data.frame(Seas=c("F","W","Sp","S"),scale=c(a1F,a1W,a1Sp,a1S),shape=c(0,0,0,0)
                       ,thr=c(ptFq,ptWq,ptSpq,ptSq))
    }else{
      par72=data.frame(Seas=c("F","W","Sp","S"),scale=c(a1F[1],a1W[1],a1Sp[1],a1S[1]),
                       shape=c(a1F[2],a1W[2],a1Sp[2],a1S[2])
                       ,thr=c(ptFq,ptWq,ptSpq,ptSq))
    }

    #calculate return levels
    rtl100F=return.level(ptFGPexp)
    rtl100W=return.level(ptWGPexp)
    rtl100Sp=return.level(ptSpGPexp)
    rtl100S=return.level(ptSGPexp)

    if(writeResults==TRUE){

    write.table(par72,paste0(pathmain,"/Exp",durt,"new/","Par",durt,"expseasnew.txt"),sep=",",row.names=FALSE)
    write.table(ptFt,paste0(pathmain,"/Exp",durt,"new/","P",durt,"Ftnew.txt"),sep=",",row.names=FALSE)
    write.table(ptWt,paste0(pathmain,"/Exp",durt,"new/", "P",durt,"Wtnew.txt"),sep=",",row.names=FALSE)
    write.table(ptSpt,paste0(pathmain,"/Exp",durt,"new/","P",durt,"Sptnew.txt"),sep=",",row.names=FALSE)
    write.table(ptSt,paste0(pathmain,"/Exp",durt,"new/","P",durt,"Stnew.txt"),sep=",",row.names=FALSE)



    rtl100=c(SepNov=rtl100F[3],DecFeb=rtl100W[3],MarMay=rtl100Sp[3],JunAug=rtl100S[3])
    write.table(rtl100,paste0(pathmain,"/Exp",durt,"new/","rtlvl.txt"))

    }else{

    res=list(par=par72,pF=ptFt,PW=ptWt,PSp=ptSpt,PS=ptSpt,rl=c(rtl100F=rtl100F,rtl100W=rtl100W,rtl100Sp=rtl100Sp, rtl100S= rtl100S))
    return(res)
    }
    if(PDFplots==TRUE){
    dev.off()
    }
  }






