## ----include=FALSE-------------------------------------------------------
library(StochasticPQRUT)
 set.seed(567)
  data(Qd)
  data(P)
    data(SWE)
  data(sl)  
    data(p1)
  data(h1)
    data(Qh)
       data(Td)

## ----echo=TRUE-----------------------------------------------------------
library(StochasticPQRUT)

  Nsim=10000

## ----echo=TRUE, fig.height=5, fig.width=5,message=FALSE------------------
  data(Qd);data(P)
   head(Qd); head(P)

## ----fig.height=5, fig.width=5-------------------------------------------
a=criticalduration(Q=Qd,P=P,qtT=0.9,PDFplots=FALSE,intEvent=7,writeResults=FALSE)

## ----echo=TRUE, fig.height=5, fig.width=5,message=FALSE------------------

d=rainPOT(datarainfall=P,pathmain,durt=24,qFtset=0.98,qWtset=0.99,qSptset=0.99,qStset=0.98,distfunc="GP",
          writeResults=FALSE,PDFplots=FALSE)

## ----echo=TRUE-----------------------------------------------------------
  data(sl);data(SWE);data(Td)
   head(sl); head(SWE); head(Td)

## ----echo=TRUE, fig.height=10, fig.width=10, warning=FALSE,message=FALSE----

b=initcond(Qobs=Qd,sl=sl,
           POTF=a$POTF,POTW=a$POTW,POTSp=a$POTSp,POTS=a$POTS,SWE=SWE,Td=Td,durt=a$d,PDFplots=FALSE,writeResults=FALSE)


## ----echo=TRUE, fig.height=5, fig.width=5, warning=FALSE,message=FALSE----
#load data data(p1)
head(p1)
h=stormp(prpFt=d$pF,prpWt=d$PW,prpSpt=d$PSp,prpSt=d$PS,p1 = p1,durt=24,writeResults=FALSE,PDFplots=FALSE)

## ----echo=TRUE, fig.height=5, fig.width=5, warning=FALSE,message=FALSE----
#load data
data(h1)
head(h1)
#error due to NULL values in sequence
h1=temprsim(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S,durt=24,tm=h1,writeResults=FALSE,PDFplots=FALSE)

## ----echo=TRUE, fig.height=5, fig.width=10, warning=FALSE, message=FALSE----
layout(matrix(1:4, nrow=2))
gincon=initconditions(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S, Nsim=Nsim,durt=a$d,writeResults=FALSE,PDFplots=FALSE)
#plot for season "09-11"
print(gincon$plot1)

## ----echo=TRUE, fig.height=5, fig.width=5, warning=FALSE,message=FALSE----
 h=simulateP(Nsim=Nsim,durt=24,distfunc="GP",hyet=h,TempSeq=h1,Pexp=d$par,writeResults=FALSE,PDFplots=FALSE)

## ----message=FALSE, warning=FALSE----------------------------------------
library(hydroGOF)
#Prepare data
b1=b[[1]]
p1$date=as.Date(p1$date,format="%d/%m/%Y")
b2=match(as.character(b1$date+1),as.character(p1$date))
data(h1)
#extract Precipitation and temperature values
p_ev=lapply(1:length(b2), function(x) p1[(b2[x]-42):(b2[x]+5),] )

b3=match(as.character(b1$date+1),as.character(as.Date(Qh$Date)))
q_ev=lapply(1:length(b2), function(x) Qh[(b3[x]-42):(b3[x]+5),] )
h1$date=as.Date(h1$date,format="%d/%m/%Y")
b4=match(as.character(b1$date+1),as.character(as.Date(h1$date)))
t_ev=lapply(1:length(b2), function(x) h1[(b4[x]-42):(b4[x]+5),] )
PT_ev=lapply(1:length(b2), function(x) cbind(p_ev[[x]][3],t_ev[[x]][3]))
# 
bn=data.frame(b1[17,2:4])

#then take the medium of all values
  #in this case the model performance is poor
fn1=function(par,...){
qsim=PQRUT(int1=bn,tm1 = PT_ev[[17]],kd=3,durt =48,param.station =c(0.1,0.04,25),Area1 = 157, snpSpt=0.3,ttsnow=-3,Tmax=0.5,Tmin=0.5 )
KGE1=-KGE(qsim[[1]],q_ev[[17]][,2])

return(KGE1)
}
par1=optim(par = c(0.1,0.04,25),fn = fn1,method = "L-BFGS-B",lower=c(0.01,0.01,1),upper = c(0.5,0.5,100))
# 
# 
qsim=PQRUT(int1=bn,tm1 = PT_ev[[17]],kd=3,durt =48,param.station =par1$par,Area1 = 157, snpSpt=0.3,ttsnow=-3,Tmax=0.5,Tmin=0.5)
plot(q_ev[[1]][,2],type="l")
points(qsim[[1]],type="l",col="green")



## ----echo=TRUE, fig.height=5, fig.width=5, warning=FALSE,message=FALSE----

gFT= hydrolsim(seasn="Ft",ncl=2,param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[1]]),E=h[[5]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
gWT= hydrolsim(seasn="Ft",ncl=2,param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[2]]),E=h[[6]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
gSpT= hydrolsim(seasn="Ft",ncl=2,param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[3]]),E=h[[7]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
gST= hydrolsim(seasn="Ft",ncl=2,param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[4]]),E=h[[8]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)


## ----echo=TRUE, fig.height=5, fig.width=5,warning=FALSE,message=FALSE----

gsim=list(Ft=gFT,Wt=gWT,Spt=gSpT,St=gST)
  d1=d[2:5]
  m1=annualfreqplot(qtT=0.9,Nsim=Nsim,durt=24,qobs=Qh,Pint=h[10:13],incond=gincon,nsy=d1,Qsim=gsim,writeResults=FALSE,PDFplots=FALSE)

