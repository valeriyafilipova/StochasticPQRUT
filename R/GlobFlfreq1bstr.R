

#' Plots the frequency curve and calculates the return periods
#'
#' @param pathmain path to files, not needed if writeResults=FALSE
#' @param qtT threshold quantile for POT (Peak over threshold) events
#' @param Nsim number of simulations
#' @param durt critical duration
#' @param qobs observed streamflow data;  dataframe with columns Date ("\%Y-\%m-\%d \%H:\%M:\%S") and Q, if writeResults=TRUE path to csv file with columns date and Q
#' @param Qsim dataframe of simulated streamflow and snowmel data (otput from the function hydrolsim) , not needed if writeResults=TRUE
#' @param Pint dataframe of simulated precipitation values (daily values)
#' @param incond dataframe of initial conditions (soil moisture deficit, Qin and SWE) for each season
#' @param nsy dataframe of extracted POT rain events for each season
#' @param plotpos te formula which is used for the pplotting positions, possible choices are only  Gringorten and Weibull
#' @return csv file with return periods
#' @export
#' @examples
#' \dontrun{
#'  d1=d[2:5]
#'  m1=annualfreqplot(qtT=0.9,Nsim=1000,durt=24,qobs,Pint=h,incond=gincon,nsy=d1,Qsim=gsim,writeResults=FALSE,PDFplots=FALSE)
#' }
annualfreqplot <- function(pathmain=NULL,qtT,Nsim,durt,qobs,Qsim,Pint,nsy,incond,plotpos="Gringorten",writeResults=TRUE,PDFplots=TRUE) {
  if(PDFplots==TRUE){
    setwd(pathmain)
    pdf("simGl1.pdf",onefile=TRUE)
  }

  seasn1=c("Ft","Wt","Spt","St")
  if(writeResults==TRUE){
    mqFt=read.csv(file.path(pathmain,"/sim/Ft/SWEsnm.txt"),sep=" ")
    mqWt=read.csv(file.path(pathmain,"/sim/Wt/SWEsnm.txt"),sep=" ")
    mqSpt=read.csv(file.path(pathmain,"/sim/Spt/SWEsnm.txt"),sep=" ")
    mqSt=read.csv(file.path(pathmain,"/sim/St/SWEsnm.txt"),sep=" ")
    Qsim1=c(mqFt[,1],mqWt[,1],mqSpt[,1],mqSt[,1])
    qobs=read.csv(paste0(pathmain,"/",this.station,".csv"),sep=" ",skip=1,stringsAsFactors = FALSE)
  }else{
    Qsim1=do.call(rbind,Qsim)

  }
  qobs$Date=as.POSIXct( qobs$Date)
  qobs1=qobs
  seasnm=c(rep(1,Nsim),rep(2,Nsim),rep(3,Nsim),rep(4,Nsim))
  Qsim1=cbind(Qsim1,seasnm)


 # qobs1$month=month(qobs1$Date)
  qobs1$month=as.numeric(format(as.Date(qobs1$Date),"%m"))

  if(writeResults==TRUE){
    nsy1=read.csv(paste0(pathmain,"/Exp",durt,"new/P",durt,"Ftnew.txt"))
    nsy2=read.csv(paste0(pathmain,"/Exp",durt,"new/P",durt,"Wtnew.txt"))
    nsy3=read.csv(paste0(pathmain,"/Exp",durt,"new/P",durt,"Sptnew.txt"))
    nsy4=read.csv(paste0(pathmain,"/Exp",durt,"new/P",durt,"Stnew.txt"))
    nsy=rbind(nsy1,nsy2,nsy3,nsy4)
  }else{

    nsy=do.call(rbind,nsy)
  }

  qobs1=na.omit(qobs1)
  qobs1$q1=decluster(qobs1$Q,threshold=quantile(qobs1$Q,qtT),r=24*4)
  q1=qobs1[qobs1$q1>quantile(qobs1$Q,qtT),]

 # qobs1$date1=year(qobs1$Date)
  qobs1$date1=as.numeric(format(as.Date(qobs1$Date),"%Y"))

  r1=nrow(Qsim1)+1-rank(Qsim1[,1])
  r3=nrow(q1)+1-rank(q1$Q)
 #Qsim2=cbind(Qsim1[,1],r1,T1,T2)
  ##Find number of years
  i=1
  m2=NULL
  n=NULL
  nd=NULL
  dt1=seq(qobs1$date1[1],qobs1$date1[length(qobs1$date1)],by=1)
  for(i in 1:(qobs1$date1[length(qobs1$date1)]-qobs1$date1[1])){
    qobs2=subset(qobs1,qobs1$date1==dt1[i])
    if(sum(is.na(qobs2$Q))<20*24){
      m2[i]=max(qobs2$Q,na.rm=TRUE)
      n[i]=match(m2[i],qobs2$Q)
      nd[i]=as.character(qobs2$Date[n[i]])
    } else{
      m2[i]=NA
      n[i]=NA
      nd[i]=NA
    }
  }
  yr1=as.numeric(format(as.Date(nsy$Date[nrow(nsy)]),"%Y"))
  yr0=as.numeric(format(as.Date(nsy$Date[1]),"%Y"))
  # Plotting positions
  if(plotpos=="Gringorten"){
 # T1=(nrow(Qsim1)+0.12)/(r1-0.44)*(year(nsy$Date[nrow(nsy)])-year(nsy$Date[1]))/nrow(nsy)
    T1=(nrow(Qsim1)+0.12)/(r1-0.44)*(yr1-yr0)/nrow(nsy)
  T2=(nrow(q1)+0.12)/(r3-0.44)*(length(nd)/nrow(q1))
  } else{

  T1=((nrow(Qsim1)+1)/r1)/(nrow(nsy)/(yr1-y0))
  T2=(nrow(q1)+1)/(r3)*(length(nd)/nrow(q1))
  #T2=T1
}
  nrt=(1:(Nsim*4))
  mqP=cbind(Qsim1,r1,T1,row.pos=as.numeric(nrt))
 # nY=nrow(Qsim1)/(nrow(nsy)/((year(nsy$Date[nrow(nsy)])-year(nsy$Date[1]))))


  mqP100=subset(mqP,T1>=90&T1<=110)
  if(writeResults==TRUE){
    intFt=read.table(paste(pathmain,"/EvtExp/Ft/inctmv/incondFt1.txt",sep=""))
    intWt=read.table(paste(pathmain,"/EvtExp/Wt/inctmv/incondWt1.txt",sep=""))
    intSpt=read.table(paste(pathmain,"/EvtExp/Spt/inctmv/incondSpt1.txt",sep=""))
    intSt=read.table(paste(pathmain,"/EvtExp/St/inctmv/incondSt1.txt",sep=""))
    incond=rbind(intFt,intWt,intSpt,intSt)
  }
  incond=do.call(rbind,incond)
  row.names(incond)=NULL

  if(writeResults==TRUE){
    PintFt=read.table(paste(pathmain,"/Exp",durt,"new/Ft",Nsim,".txt",sep=""))
    PintWt=read.table(paste0(pathmain,"/Exp",durt,"new/Wt",Nsim,".txt"))
    PintSpt=read.table(paste0(pathmain,"/Exp",durt,"new/Spt",Nsim,".txt"))
    PintSt=read.table(paste0(pathmain,"/Exp",durt,"new/St",Nsim,".txt"))
    Pint1=rbind(PintFt,PintWt,PintSpt,PintSt)
  }else{
    Pint1=do.call(c,Pint)
  }
  row.names( Pint1)=NULL
  sWEn=(Qsim1[,2])

  int100=(incond[mqP100[,5],])
  intP100= Pint1[mqP100[,5]]
  sWEn=sWEn[mqP100[,5]]
  if(PDFplots==TRUE){
    try( hist(int100[1],main="Qobs",breaks=10))
    try( hist(int100[2],main="SWE",breaks=10))
    try( hist(sWEn,main="Snowmelt",breaks=10))
    try( hist(int100[3],main="Soil Moisture Deficit"))
    try( hist(intP100,main="P"))
  }
  Tt=c(0.5,1,2,5,10,20,50,100,200,500,1000,2000)

  plot(spline(T1,Qsim1[,1]),from=Tt[1],to =T1,log="x",xlab="",ylab=expression("Q ("*m^3/s*")")
       ,xaxt="n",type="l",panel.first=grid(equilogs=FALSE),xlim=c(0.3,1500))
  points(T2,q1$Q,type="p",pch=16,col="red")
  axis(1,at=Tt,labels=Tt)
 #  points(T2,Qsim1[,1],col="orange")
  legend("bottomright",c("obs POT","sim POT"),pch=c(16,20),col=c("red","black"))

  t1t=data.frame(approx(x=mqP[,4],y=mqP[,1],Tt))
  if(writeResults==TRUE){
    write.table(t1t,paste(pathmain,"/sim/t1.txt",sep=""))
  }else{
    return(t1t)
  }
  if(PDFplots==TRUE){
    dev.off()
  }
}
