

#' @title Simulates precipitation and temperature sequence.
#' @description First, values with daily time resoultion are simulated using either GP or exponential distribution, then temporal patterns are used to
#' disaggregate the values
#' By default it uses parallel processing using 4 cores
#'
#'
#' @param pathmain path to files, not needed if writeResults=FALSE
#' @param Nsim number of simulations
#' @param durt critical duration
#' @param distfunc statistical distribution :GP or Exponential
#' @param  Pexp parameters of the fitted rainfall distribution not needed if writeResults=TRUE
#' @param hyet list of dataframes of  hyetographs for each season, output from  stormp not needed if writeResults=TRUE
#'  @param TempSeq list of dataframes of temperature series for each season, output from  temprsim not needed if writeResults=TRUE
#'   @param ncl number of cluster, used for parallel processing
#' @import extRemes
#' @import  foreach
#' @import  parallel
#' @import  doParallel
#' @return a matrix with each column giving the P/T sequence for a flood event
#' @export
#' @examples
#'\dontrun{
#' h=simulateP(Nsim=1000,durt=24,distfunc="GP",hyet=h,TempSeq=h1,Pexp=d$par,writeResults=FALSE,PDFplots=FALSE)
#' }
simulateP <- function(pathmain=NULL,Nsim,durt,distfunc,Pexp, hyet,TempSeq,ncl=4,writeResults=FALSE,PDFplots=FALSE) {
  #  rainsimulate1("F:/FlomQ/62.5",Nsim=100000,durt=24,distfunc="GP")

  seasn1=c("Ft","Wt","Spt","St")
  if(writeResults==TRUE){
  setwd(pathmain)
  }else{
    res=list()
  }
  #reads in the parameters of the distribution and simulates precipitation depth
  if(distfunc=="Exponential"){
    if(writeResults==TRUE){
    Pexp=read.table(paste0(pathmain,"/Exp",durt,"new","/Par",durt,"expseasnew.txt"),sep=",",header=TRUE)
    }
    Ft=revd(Nsim,scale=Pexp$scale[1],threshold=Pexp$thr[1],type="GP")
    Wt=revd(Nsim,scale=Pexp$scale[2],threshold = Pexp$thr[2],type="GP")
    Spt=revd(Nsim,scale = Pexp$scale[3],threshold = Pexp$thr[3],type="GP")
    St=revd(Nsim,scale=Pexp$scale[4],threshold = Pexp$thr[4],type = "GP")
  }
  if(distfunc=="GP"){
    if(writeResults==TRUE){
    Pexp=read.table(paste0(pathmain,"/Exp",durt,"new","/Par",durt,"expseasnew.txt"),sep=",",header=TRUE)
  }
    Ft=revd(Nsim,scale=Pexp$scale[1],shape=Pexp$shape[1],threshold=Pexp$thr[1],type="GP")
    Wt=revd(Nsim,scale=Pexp$scale[2],shape=Pexp$shape[2],threshold = Pexp$thr[2],type="GP")
    Spt=revd(Nsim,scale = Pexp$scale[3],shape=Pexp$shape[3],threshold = Pexp$thr[3],type="GP")
    St=revd(Nsim,scale=Pexp$scale[4],shape=Pexp$shape[4],threshold = Pexp$thr[4],type = "GP")
  }
  for(seasn2 in 1:4 ) {

    seasn=seasn1[seasn2]
    print(seasn)
    Ft=get(seasn)
    n1=NULL

    #uses parallel processing to disagregate to hourly timestep
    if(writeResults==TRUE){
    dir.create(paste(pathmain,"/evtExp/",seasn,"/P",sep=""))
    }
    cl=makeCluster(ncl)
    registerDoParallel(cl)
    mn=list()
    if(writeResults==TRUE){
    hyet=list.files(paste(pathmain,"/tpstormExp",durt,"/",seasn,sep=""),pattern = "st")
    hyet=hyet[1:(length(hyet)-1)]
    }
    tpmatrfunct=function(i){
      n=sample(2:length(hyet),1)
      if(writeResults==TRUE){
      st=read.table(paste(pathmain,"/tpstormExp",durt,"/",seasn,"/",hyet[n],sep=""),skip=1)
      st=st$V2
      }else{
        st=hyet[[seasn2]][[n]]
      }
      tp1=round(Ft[i]*st/sum(st),3)

      return(tp1)
    }
    tpmatr=foreach (i=1:Nsim,.combine= "cbind") %dopar% {
      tpmatrfunct(i)

    }


    stopCluster(cl)
    cl=makeCluster(4)
    registerDoParallel(cl)



    n2=NULL
    if(writeResults==TRUE){
    TempSeq=list.files(paste(pathmain,"/TE1/",seasn,sep=""))
    }
    t1matrfunc=function(i){

      n=sample(1:length(TempSeq),1)
      if(writeResults==TRUE){
      t1=read.table(paste(pathmain,"/TE1/",seasn,"/",TempSeq[n],sep=""),skip=1)

      t1=as.matrix(t1[,3])
      }else{
        t1=TempSeq[[seasn2]][[n]]
      }
      return(t1)

    }
    if(writeResults==TRUE){
    dir.create(paste(pathmain,"/evtExp/",seasn,"/T",sep=""))
    }
    t1matr=foreach(i=1:(Nsim) ,.combine="cbind")  %dopar% {
      t1matrfunc(i)

    }

    stopCluster(cl)
    if(writeResults==TRUE){
    write.table(tpmatr,paste(pathmain,"/evtExp/",seasn,"/P/tp.txt",sep=""),col.names = FALSE,row.names = FALSE)
    write.table(t1matr,paste(pathmain,"/evtExp/",seasn,"/T/tn.txt",sep=""),col.names=FALSE,row.names=FALSE)

    write.table(Ft,paste(pathmain,"/Exp",durt,"new/",seasn,Nsim,".txt",sep=""))
    }else{
     res[[seasn2]]= tpmatr
     res[[seasn2+4]]= t1matr
     res[[seasn2+9]]= Ft
    }
  }
  if(writeResults==FALSE){
return(res)
    }
}
