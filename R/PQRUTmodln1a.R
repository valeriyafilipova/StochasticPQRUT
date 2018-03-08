#' @export
#' @include snowsim.R
PQRUT=function(int1,tm1,kd,durt,param.station,snpSpt=snpSpt,Area1,slconst=1,ttsnow=ttsnow,Tmax,Tmin){

  # http://www.usask.ca/hydrology/papers/Marks_et_al_2013.pdf

 
  qo=int1[1]*3.6/Area1
  SWE=int1[2]

 colnames(tm1)=c("P","E")

  #uses snow.sim or snow function to simulate snow
  if(SWE>snpSpt*sum(tm1$P)|(min(tm1$E)<=ttsnow)){#no SWE but freezing

    snm=snow.sim(E=tm1$E,P=tm1$P,Tmax=Tmax,Tmin=Tmin,kd=kd/24,kf=0,rcap = 0.2,LSWE_0 = 0,ISWE_0 =SWE )
    Pt1=as.numeric(snm)
    tm1$Pt=Pt1
  }else{

    Pt1=tm1$P
    tm1$Pt=tm1$P
  }

  #soil saturation
  if(slconst==1){
    S1=rep(0,durt)
    lp=int1[3]
    if(lp<0){
      lp=0
    }

    tk1t=1
    for(t1 in 1:durt){

      if(sum(S1)<lp){
        S1[t1]=Pt1[t1]
        Pt1[t1]=0
      }


      if(sum(S1)>=lp&tk1t==1){
        Pt1[t1]=sum(S1)-lp
        tk1t=tk1t+1
      }
    }
    #sum(Pt1)+lp
    Pt1[Pt1<0]=0
    Pt1s=Pt1

  }
  #constant loss for soil saturation
  if(slconst==2){
    lp=int1[3]
    if(lp>0){
      lp=0
    }
    lp=lp/durt
    for(t1 in 1:durt){


      Pt1[t1]=Pt1[t1]-lp
      Pt1[Pt1<0]=0
      Pt1s=Pt1
    }
  }
  Pt1s[Pt1s<0]=0

  Pt1s=as.numeric(Pt1s)
  parameters=param.station

  ptverdier=data.frame(time=seq(1,durt),prec=Pt1s)
  tst=1

  #PQRUT model
  # set level in the parameter
  x=qo/(1-exp(-parameters[2]*tst))

  q=rep(0,durt)
  for(nev in 1:durt){

    if( x < parameters[3]){
      q[nev]=(parameters[2] * x)
      x = x +Pt1s[nev] - q[nev]
    }else {
      q[nev]=(parameters[1]*(x-parameters[3]) + parameters[2]*parameters[3])
      x <- x +Pt1s[nev] - q[nev]

    }
  }

  discharge <- (as.numeric(q) * Area1)/3.6

 # mq=max(discharge)
  #snow accumulation
  asnm=sum(tm1$Pt)-sum(tm1$P)
  snmSWE=asnm
  return(list(discharge,asnm))
}
