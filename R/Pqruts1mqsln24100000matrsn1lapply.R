## Set working directory
#' @title Simulates discharge values from a set of input P,T files
#' and initial conditions
#'
#' @description Uses the PQRUT model hydrological model and a degree day snowmodel to simulate discharge values
#' By default it uses parallel processing using 4 cores
#' @param pathmain path to files, not needed if writeResults=FALSE
#' @param seasn season
#' @param int1 dataframe of initial conditions (SWE,soil moisture deficit and initial discharge ) for events (otput from the function initconditions)
#' @param Pt a matrix with each column giving the input P sequence for a flood event (otput from the function rainsimulate1) not needed if writeResults=TRUE
#' @param E a matrix with each column giving the input T sequence for a flood event (otput from the function rainsimulate1) not needed if writeResults=TRUE
#' @param param.station station number
#' @param Nsim number of simulations
#' @param durt critical duration
#' @param Area1 watershed Area
#' @param kd coefficient for melting (usually 2-5)
#' @param modelsnow type of snow model (Snow.sim or snow)
#' @param slconst loss due to infiltration (2 constant loss, 1 all of the precipitation infiltrates until a reservoir is filled up) )
#' @param snpSpt percent snow above which the snowmodel is activated
#' @param ttsnow temperature threshold for snow, below this temperature the snowmodel is activated
#' @param Tmax max T for snow melt
#' @param Tmin min T for snowmelt
#' @param ncl number of cluster, used for parallel processing
#' @return csv file with columns Q and snowmelt
#' @export
#' @examples
#' \dontrun{
#' g= hydrolsim(seasn="Ft",param.station=c(0.2,0.1,10),Nsim=1000,int1=g$SWEFt,Pt=as.matrix(h[[1]]),E=h[[5]],durt=24,Area1=500,kd=2,modelsnow="Snow.sim",slconst=1,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
#' }
#' @include PQRUTmodln1a.R
hydrolsim <- function(pathmain=NULL,ncl=4,seasn,param.station,Nsim,int1,Pt,E,durt
                      ,Area1,kd,slconst=1,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE) {

  mq=NULL
  snmSWE=NULL
  #read input files
  if(writeResults==TRUE){
  setwd(pathmain)
  dir.create(file.path(pathmain,"/sim"))
  dir.create(paste(pathmain,"/sim/",seasn,sep=""))

  int1=read.table(paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
  path1=file.path(pathmain,"/evtExp",seasn,"/P")

  Pt=matrix(scan(paste(path1,"/tp.txt",sep=""),n=durt*Nsim),durt,Nsim,byrow=TRUE)
  E=matrix(scan(paste(pathmain,"/evtExp/",seasn,"/T/tn.txt",sep=""),n=durt*Nsim),durt,Nsim,byrow=TRUE)
  }
  cl=makeCluster(ncl)
  registerDoParallel(cl)
    if(ncl>1){
    qm=foreach(k=1:Nsim,.combine = "rbind") %dopar% {
      tm1=data.frame(P=Pt[,k],E=E[,k])
      ## hydrological model
      int2=int1[k,]
     a= PQRUT(int1=int1,tm1=tm1,param.station=param.station,kd=kd,durt=durt,Area1=Area1,slconst=slconst,snpSpt = snpSpt
            ,ttsnow = ttsnow,Tmax=Tmax,Tmin=Tmin)
      return(max(a[[1]]))
    }
    }else{
      qm=rep(NA,Nsim)
        for(k in 1:Nsim){
        tm1=data.frame(P=Pt[,k],E=E[,k])
        ## hydrological model
        int2=int1[k,]
        a= PQRUT(int1=int1,tm1=tm1,param.station=param.station,kd=kd,durt=durt,Area1=Area1,slconst=slconst,snpSpt = snpSpt
                 ,ttsnow = ttsnow,Tmax=Tmax,Tmin=Tmin)
        qm[k]=(max(a[[1]]))
    }
    }
    if(writeResults==TRUE){
    write.table(qm,paste(pathmain,"/sim/",seasn,"/SWEsnm.txt",sep=""))
    }else{
      return(qm)
    }
    stopCluster(cl)

}

