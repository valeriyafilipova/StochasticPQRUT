---
title: "Stochastic PQRUT"
output: pdf_document
---

This R package implements the Stochastic PQRUT model, which performs flood frequency analysis of simulated discharge values.The model simulates Precipitation, temperature  and initial conditions and uses deterministic rainfall-runoff model PQRUT to obtain the final discharge values. The return periods are estimated using plotting positions. The analysis is performed using seasonal values but currently the seasonal split is hard coded to season 1= "9-11",season 2="12-2",season 3="3-5",season 4="6-8".


Workflow


1. Load the library and define number of simulations

```r
library(StochasticPQRUT)

  Nsim=10000
```


2. Find critical duration - the duration of the storm event that results in maximum discharge. For this Precipitation and Discharge with  daily timestep are used. Sample data for Hørte (stN 16.193) is included in this package.

```r
  data(Qd);data(P)
   head(Qd); head(P)
```

```
##         date  Q
## 1 1961-01-01 NA
## 2 1961-01-02 NA
## 3 1961-01-03 NA
## 4 1961-01-04 NA
## 5 1961-01-05 NA
## 6 1961-01-06 NA
```

```
##         date    Pr
## 1 1961-09-01  0.00
## 2 1961-09-02  0.00
## 3 1961-09-03  0.00
## 4 1961-09-04  0.00
## 5 1961-09-05  0.00
## 6 1961-09-06 42.15
```

POT events are extracted over the quantile, specified by qtT and seperated by number of days intEvent (using extRemes). This is performed for predifined seasons, as explained earlier. Then these events are correlated to the Precipition on 1,2,3 days before the peak. 

```r
a=criticalduration(Q=Qd,P=P,qtT=0.9,PDFplots=FALSE,intEvent=7,writeResults=FALSE)
```

![](C:\Users\vfili\AppData\Local\Temp\RtmpqYDPkg\preview-32782f94f67.dir\vignette_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

The function returns a list, containing the critical duration and dataframes of the extracted POT. 


3. Split the precipitation values into seasons and fit GPD or Exponential distribution to POT events using the extRemes package. The parameters qFtset,qWtset,qWtset,qSptset are the threshold quantiles that are used to extract the POT events. These events do not coincide with the events, extracted in step 1.

```r
d=rainPOT(datarainfall=P,pathmain,durt=24,qFtset=0.98,qWtset=0.99,qSptset=0.99,qStset=0.98,distfunc="GP",
          writeResults=FALSE,PDFplots=FALSE)
```

```
## fevd(x = ptF$Pcl, threshold = ptFq, type = "GP", method = "Lmoments", 
##     time.units = paste0(a, "/year"))
## [1] "GP  Fitted to  ptF$Pcl  using L-moments estimation."
##       scale       shape 
## 11.12196683 -0.06680288
```

```
## fevd(x = ptW$Pcl, threshold = ptWq, type = "GP", method = "Lmoments", 
##     time.units = paste0(a, "/year"))
## [1] "GP  Fitted to  ptW$Pcl  using L-moments estimation."
##       scale       shape 
##  9.85902001 -0.08674199
```

```
## fevd(x = ptSp$Pcl, threshold = ptSpq, type = "GP", method = "Lmoments", 
##     time.units = paste0(a, "/year"))
## [1] "GP  Fitted to  ptSp$Pcl  using L-moments estimation."
##      scale      shape 
## 7.49429687 0.04903761
```

```
## fevd(x = ptS$Pcl, threshold = ptSq, type = "GP", method = "Lmoments", 
##     time.units = paste0(a, "/year"))
## [1] "GP  Fitted to  ptS$Pcl  using L-moments estimation."
##     scale     shape 
## 12.096132 -0.146584
```

![](C:\Users\vfili\AppData\Local\Temp\RtmpqYDPkg\preview-32782f94f67.dir\vignette_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

4.Extract initial conditions (Snow water equivalent, soil moisture deficit and initial discharge) for the POT flood events, identified in step 1. In this case, the data has been generated using the hydrological model DDD. 


```r
  data(sl);data(SWE);data(Td)
   head(sl); head(SWE); head(Td)
```

```
##         date    sl
## 1 1999-09-01 31.57
## 2 1999-09-02 30.24
## 3 1999-09-03 31.64
## 4 1999-09-04 32.86
## 5 1999-09-05 33.23
## 6 1999-09-06 33.42
```

```
##         date SWE
## 1 1999-09-01   0
## 2 1999-09-02   0
## 3 1999-09-03   0
## 4 1999-09-04   0
## 5 1999-09-05   0
## 6 1999-09-06   0
```

```
##         date     Td
## 1 1999-09-01  8.743
## 2 1999-09-02 10.268
## 3 1999-09-03 14.592
## 4 1999-09-04 15.059
## 5 1999-09-05 14.923
## 6 1999-09-06 15.149
```



The function returns a list of initial conditions  for the given seasons.

```r
b=initcond(Qobs=Qd,sl=sl,
           POTF=a$POTF,POTW=a$POTW,POTSp=a$POTSp,POTS=a$POTS,SWE=SWE,Td=Td,durt=a$d,PDFplots=FALSE,writeResults=FALSE)
```

![](C:\Users\vfili\AppData\Local\Temp\RtmpqYDPkg\preview-32782f94f67.dir\vignette_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

5.Extract hyetographs for flood events. This function matches the dates for the POT events, extracted in step 3 to  hourly precipitation dataset. It is assumed that the daily data is recorded at 6 am.

```r
#load data data(p1)
head(p1)
```

```
##         date     time p
## 1 01/01/1979 00:00:00 0
## 2 01/01/1979 01:00:00 0
## 3 01/01/1979 02:00:00 0
## 4 01/01/1979 03:00:00 0
## 5 01/01/1979 04:00:00 0
## 6 01/01/1979 05:00:00 0
```

```r
h=stormp(prpFt=d$pF,prpWt=d$PW,prpSpt=d$PSp,prpSt=d$PS,p1 = p1,durt=24,writeResults=FALSE,PDFplots=FALSE)
```

6. Extract temperature sequence. Similarly to 4, the function matches the dates of the POT events to hourly Temperature data

```r
#load data
data(h1)
head(h1)
```

```
##         date     time         T
## 1 01/01/1979 00:00:00 -23.38190
## 2 01/01/1979 01:00:00 -23.48557
## 3 01/01/1979 02:00:00 -23.58923
## 4 01/01/1979 03:00:00 -23.69290
## 5 01/01/1979 04:00:00 -23.83850
## 6 01/01/1979 05:00:00 -23.98410
```

```r
#error due to NULL values in sequence
h1=temprsim(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S,durt=24,tm=h1,writeResults=FALSE,PDFplots=FALSE)
```

7. Simulate initial conditions. The values are simulated using truncated multivariate normal distribution. This ensures that the  simulated values are within the limits of the observed series. 

```r
layout(matrix(1:4, nrow=2))
gincon=initconditions(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S, Nsim=Nsim,durt=a$d,writeResults=FALSE,PDFplots=FALSE)
#plot for season "09-11"
print(gincon$plot1)
```

![](C:\Users\vfili\AppData\Local\Temp\RtmpqYDPkg\preview-32782f94f67.dir\vignette_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

8.Simulate precipitation and temperature values and disaggregate to 1 hour. This function uses the parameters of the GPD , obtained in step 2 and the results of step 5 and step 6.

```r
 h=simulateP(Nsim=Nsim,durt=24,distfunc="GP",hyet=h,TempSeq=h1,Pexp=d$par,writeResults=FALSE,PDFplots=FALSE)
```

```
## [1] "Ft"
## [1] "Wt"
## [1] "Spt"
## [1] "St"
```

9.Calibrate the hydrological model PQRUT for selected events. In this case optim is used , however a better option is to use ppso package.


```r
library(hydroGOF)
```

```
## Warning: package 'hydroGOF' was built under R version 3.4.3
```

```
## Loading required package: zoo
```

```
## Warning: package 'zoo' was built under R version 3.4.2
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
b1=b[[1]]
p1$date=as.Date(p1$date,format="%d/%m/%Y")
b2=match(as.character(b1$date+1),as.character(p1$date))
data(h1)
#extrct Precipitation and temperature values
p_ev=lapply(1:length(b2), function(x) p1[(b2[x]-42):(b2[x]+5),] )

b3=match(as.character(b1$date+1),as.character(as.Date(Qh$Date)))
q_ev=lapply(1:length(b2), function(x) Qh[(b3[x]-42):(b3[x]+5),] )
h1$date=as.Date(h1$date,format="%d/%m/%Y")
b4=match(as.character(b1$date+1),as.character(as.Date(h1$date)))
t_ev=lapply(1:length(b2), function(x) h1[(b4[x]-42):(b4[x]+5),] )
PT_ev=lapply(1:length(b2), function(x) cbind(p_ev[[x]][3],t_ev[[x]][3]))
# 
#then take the medium of all values
  #in this case the model performance is poor
fn1=function(par,...){
qsim=PQRUT(int1=b1[1,2:4],tm1 = PT_ev[[2]],kd=3,durt =48,param.station =par,Area1 = 157, snpSpt=0.3,ttsnow=-3,Tmax=0.5,Tmin=0.5 )
KGE1=-KGE(qsim[[1]],q_ev[[1]][,2])

return(KGE1)
}
par1=optim(par = c(0.1,0.04,25),fn = fn1,method = "L-BFGS-B",lower=c(0.01,0.01,1),upper = c(0.5,0.5,100))
# 
# 
qsim=PQRUT(int1=b1[1,2:4],tm1 = PT_ev[[2]],kd=3,durt =48,param.station =par1$par,Area1 = 157, snpSpt=0.3,ttsnow=-3,Tmax=0.5,Tmin=0.5)
plot(q_ev[[1]][,2],type="l")
points(qsim[[1]],type="l",col="green")
```

![](C:\Users\vfili\AppData\Local\Temp\RtmpqYDPkg\preview-32782f94f67.dir\vignette_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 



10. Simulate discharge values. The model PQRUT is used to simulate discharge values. The model was calibrated using method described in Filipova 2016.

```r
gFT= hydrolsim(seasn="Ft",param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[1]]),E=h[[5]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
gWT= hydrolsim(seasn="Ft",param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[2]]),E=h[[6]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
gSpT= hydrolsim(seasn="Ft",param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[3]]),E=h[[7]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
gST= hydrolsim(seasn="Ft",param.station=c(0.1138291,0.028429,19.85680),Nsim=Nsim,int1=gincon$SWEFt,Pt=as.matrix(h[[4]]),E=h[[8]],durt=24,Area1=157,kd=2.8,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5,writeResults=FALSE,PDFplots=FALSE)
```

11. Calculate the return levels. This function uses plotting positions to estimate the return periods and returns a list of return levels.

```r
gsim=list(Ft=gFT,Wt=gWT,Spt=gSpT,St=gST)
  d1=d[2:5]
  m1=annualfreqplot(qtT=0.9,Nsim=Nsim,durt=24,qobs=Qh,Pint=h[10:13],incond=gincon,nsy=d1,Qsim=gsim,writeResults=FALSE,PDFplots=FALSE)
```

![](C:\Users\vfili\AppData\Local\Temp\RtmpqYDPkg\preview-32782f94f67.dir\vignette_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 
