

#' @title Simulates initial conditions
#' @description Simulates initial conditions (initial Q, soil moisture deficit and SWE). The procedure uses truncated mulivariate normal distribution
#' to simulate values between the minimum and maximum observed. The values for initial Q and soil moisture deficit are first log transformed.
#'
#' @param pathmain path to files , not needed if writeResults=FALSE
#' @param incondFt,incondWt,incondSpt,incondSt dataframes that contain the POT for each season
#' @param Nsim Number of simulation
#' @param durt critical duration
#' @import RColorBrewer
#' @import tmvtnorm
#' @import corpcor
#' @return if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains  dataframes of initial conditions
#' (SWE,soil moisture deficit and initial discharge ) for events for each season is returned
#' @examples
#' \dontrun{
#' g=initconditions(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S, Nsim=10000,durt=a$d,writeResults=FALSE,PDFplots=TRUE)
#' }
#' @export
#'
#'
initconditions <- function(pathmain=NULL,incondFt,incondWt,incondSpt,incondSt, Nsim,durt,writeResults=TRUE,PDFplots=TRUE) {

  if(PDFplots==TRUE){

  setwd(pathmain)
  pdf("initcondhist.pdf",onefile = TRUE)
  }

  seasn1=c("Ft","Wt","Spt","St")
  if(writeResults==FALSE){
 alist=list(incondFt,incondWt,incondSpt,incondSt)
  }
  seasnlist=1
  for(seasn in seasn1){

    if(writeResults==TRUE){
  incondFt=read.table(paste(pathmain,"/initcondExp",durt,"/initcond1",seasn,".txt",sep=""))
    }else{
      incondFt=alist[[seasnlist]]

    }

  incondFt$Q[incondFt$Q<0]=0.01
 #set values of SWE<1 to 0
  incondFt$SWE=ifelse(incondFt$SWE<1,0,incondFt$SWE)
  incondFt=incondFt[,c(2:4)]
  incondFt2=incondFt[incondFt$SWE>0,]

  #positive SWE
  p0=1-nrow(incondFt2)/nrow(incondFt)

 #only positive soil moisture deficit

  incondFt2$sl[incondFt2$sl<0]=0

  incondFt2tr=incondFt2

  #transform to log values
  incondFt2tr$Q=log(incondFt2$Q+0.1)
  incondFt2tr$SWE=log(incondFt2$SWE+0.1)

  mu1=colMeans(incondFt2tr)
  sigma1=as.matrix(var(incondFt2tr))

  n1=as.integer(round(Nsim*(1-p0)))
 # All values for SWE are 0
  if(p0==1 | nrow(incondFt2tr)<5){
    SWE2=NULL

    #Simulate SWE values
  }else{
    if(sigma1[3]!=0) {
  SWE2=try(rtmvnorm(n=n1,mean=mu1,sigma = sigma1
      ,lower = apply(incondFt2tr,2,min),upper = apply(incondFt2tr,2,max)))
  if(class(SWE2)=="try-error"|is.na(SWE2[1,1])){

    #sometimes it is not positive definitive
  sigma1=make.positive.definite(sigma1)
  sigma1=sigma1+diag(ncol(sigma1))*0.01
  SWE2=rtmvnorm(n=n1,mean=mu1,sigma = sigma1
                    ,lower = apply(incondFt2tr,2,min),upper = apply(incondFt2tr,2,max))
  }
    }else{

      #if SWE is all equal to 0
      sigmam=sigma1[1:2,1:2]
      sigmam=make.positive.definite(sigmam)
      sigmam=sigmam+diag(ncol(sigmam))*0.01
      SWE2=rtmvnorm(n=n1,mean=mu1[1:2],sigma = sigmam
                    ,lower = apply(incondFt2tr[1:2],2,min),upper = apply(incondFt2tr[1:2],2,max))
      SWE2=cbind(SWE2,0)
    }

  SWE2[,1]=exp(SWE2[,1])-0.1
  SWE2[,2]=exp(SWE2[,2])-0.1
  SWE2[,3]=(SWE2[,3])

  }
  if(p0>0){

 #else p0 ==0
  incondFt3=incondFt[incondFt[,2]==0,]

  #SWE==0 less than 10 rows , use same parameters for the distribution
  if(nrow(incondFt3)<10){

    #not enough observations to fit the distribution
    if(sigma1[3]!=0) {
    SWE3=rtmvnorm(n=(Nsim-n1),mean=mu1,sigma = sigma1
                  ,lower = apply(incondFt2tr,2,min),upper = apply(incondFt2tr,2,max))
    SWE3[,1]=exp(SWE3[,1])-0.1
    SWE3[,2]=exp(SWE3[,2])-0.1
    SWE3[,3]=(SWE3[,3])
    }else{
      sigman=sigma1[1:2,1:2]
      sigman=make.positive.definite(sigman)
      sigman=sigman+diag(ncol(sigman))*0.01
      SWE3=rtmvnorm(n=(Nsim-n1),mean=mu1[1:2],sigma = sigman
                    ,lower = apply(incondFt2tr[1:2],2,min),upper = apply(incondFt2tr[1:2],2,max))
      SWE3[,1]=exp(SWE3[,1])-0.1
      SWE3[,2]=exp(SWE3[,2])-0.1

      SWE3=cbind(SWE3,0)
    }

  if(nrow(incondFt2tr)<5) n1=0
    n2=Nsim-n1
    SWE5=cbind(SWE3[,1],rep(0,n2),SWE3[,3])
   }else if(nrow(incondFt3)>=10)
  {
  incondFt3$SWE=NULL

  incondFt3$sl[incondFt3$sl<0]=0
  incondFt3$Q=log(incondFt3$Q+1)
  incondFt3$sl=(incondFt3$sl)
  if(nrow(incondFt2tr)<5) n1=0
 n2=Nsim-n1

  mu2=colMeans(incondFt3)
  sigma2=as.matrix(var(incondFt3))
  if(sigma2[2]!=0){
  SWE3=rtmvnorm(n=n2,mean=mu2,sigma = sigma2
         ,lower = apply(incondFt3,2,min),upper = apply(incondFt3,2,max))

  SWE3[,1]=exp(SWE3[,1])-1
  SWE3[,2]=(SWE3[,2])
  incondFt2$sl[incondFt2$sl<0]=0
  }else{
    SWE3=rnorm(n = n2,mean=mu2[1],sd=sigma2[1])
    SWE3=cbind(SWE3,0)
    SWE3[,1]=exp(SWE3[,1])-1
  }


  SWE5=cbind(SWE3[,1],rep(0,n2),SWE3[,2])
   }
  #
  SWE4=rbind(SWE2,SWE5)
  }else {
    SWE4=SWE2}

  if(nrow(SWE4)!=Nsim){
    stop(paste("SWE not equal to Nsim"),seasn)
  }
  colnames(SWE4)=c("Q","SWE","sl")
  #diamondplot(data.frame(SWE2[1:1000,]))
  # Simulate data
  my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) +
      stat_density2d(aes(fill=..density..), geom="tile", contour = FALSE) +
     scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral")))
    p
  }


  SWE4p=data.frame(SWE4)

 # smoothhydropairs(SWE4)
 p1= ggpairs(SWE4p, lower=list(continuous=my_fn), mapping = ggplot2::aes(fill = "blue"),
          diag=list(continuous=wrap("barDiag"),fill="blue"))+theme_classic()

  incondFt$sl[incondFt$sl<0]=0
 p2= ggpairs(incondFt)+theme_classic()
  #smoothScatter(SWE4,nbin = 50)
 p3=( plot_grid(
   ggmatrix_gtable(p1),
   ggmatrix_gtable(p2),
   ncol = 2
 ))
 if(PDFplots==TRUE){
print(p3)
}
  if(writeResults==TRUE){
  dir.create(file.path(pathmain,"/evtExp"))
  dir.create(file.path(pathmain,"/evtExp/",seasn))
  dir.create(file.path(pathmain,"/evtExp/",seasn,"/inctmv"))

   write.table(SWE4,file.path(pathmain,"/evtExp/",seasn,"/inctmv/",paste0("incond",seasn,"1.txt")))
  }else{

    assign(x = seasn,value = SWE4)

  }
 plotseasn=paste0("plot",seasn)

 assign(x= plotseasn,value = p3)
  seasnlist= seasnlist+1
  }
  #
  if(PDFplots==TRUE){
   dev.off()
  }
  if(writeResults==FALSE){
   res=list(SWEFt=Ft,SWEWt=Wt,SWESpt=Spt,SWESt=St,plot1=plotFt,plot2=plotWt,plot3=plotSpt,plot4=plotSt)

  }
}



