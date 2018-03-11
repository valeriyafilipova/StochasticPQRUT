#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(StochasticPQRUT)

shinyServer( function(input, output) {
  
  my_data=reactive({
    inFile <- input$PJ
    P <- read.csv(inFile$datapath)
  #  print(head(P))
  }

  )
  
 aR<-eventReactive(input$do1,{
   inFile <- input$QJ
   QJ<-  read.csv(inFile$datapath)
  # print(head(QJ))
   P=my_data()
   criticalduration(Q=QJ,P = P,qtT=input$qT,intEvent = 7,PDFplots = FALSE,writeResults = FALSE)
 })
  
    observeEvent(input$do1, {

 output$plot1 <- renderPlot({
    inFile <- input$QJ
    QJ<-  read.csv(inFile$datapath)
    print(head(QJ))
  P=my_data()
  print(head(P))
    qtT=input$qT

   criticalduration(Q=QJ,P = P,qtT=qtT,intEvent = 7,PDFplots = FALSE,writeResults = FALSE)
  })
    })
  observeEvent(input$do1,{
  output$text1 <- renderText({
 #   pathmain=input$stn
 a=aR()
 #print(a)
 paste("critical duration=",a$d)
})
 })
  
  
  hR<-eventReactive(input$do3,{
    withProgress(message = 'Generating P,T events ', value = 0, {
    inFile <- input$QJ
    QJ<-  read.csv(inFile$datapath)
     
    inFile <- input$PJ
    P =  read.csv(inFile$datapath)
    
    inFile <- input$h1
    h1 =  read.csv(inFile$datapath)
    
    inFile <- input$sl
    sl =  read.csv(inFile$datapath)

    
    inFile <- input$SWE
    SWE =  read.csv(inFile$datapath)

     a=aR()
     incProgress(1/5, detail = paste("loading data"))
    b=initcond(Qobs=Qd,sl=sl,
               POTF=a$POTF,POTW=a$POTW,POTSp=a$POTSp,POTS=a$POTS,SWE=SWE,Td=Td,durt=input$durt,PDFplots=FALSE,writeResults=FALSE)
   
     d=rainPOT(datarainfall = P,durt=input$durt,qFtset=input$qT1,qWtset=input$qT2,qSptset=input$qT3,qStset=input$qT4,distfunc=input$radio,writeResults = FALSE,PDFplots = FALSE)
     incProgress(2/5, detail = paste("extracting hyetographs"))
      h=stormp(prpFt=d$pF,prpWt=d$PW,prpSpt=d$PSp,prpSt=d$PS,p1 = p1,durt=input$durt,writeResults=FALSE,PDFplots=FALSE)
      incProgress(3/5, detail = paste("extracting T sequence"))
      h1=temprsim(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S,durt=input$durt,tm=h1,writeResults=FALSE,PDFplots=FALSE)
      incProgress(4/5, detail = paste("Generating T,P data"))
       h=simulateP(Nsim=input$Nsim,durt=input$durt,distfunc=input$radio,hyet=h,TempSeq=h1,Pexp=d$par,writeResults=FALSE,PDFplots=FALSE)
       incProgress(5/5, detail = paste("completed"))
     return(hbd=list(h=h,b=b,d=d))
    })
  })

  observeEvent(input$do2, {

output$plot2 <- renderPlot({

  inFile <- input$PJ
  P =  read.csv(inFile$datapath)
    pathmain=input$stn
    qtT1=input$qT1
    par(mfrow=c(2,2))
    rainPOT(datarainfall = P,durt=input$durt,qFtset=qtT1,qWtset=input$qT2,qSptset=input$qT3,qStset=input$qT4,distfunc=input$radio,writeResults = FALSE,PDFplots = FALSE)

  })
  })
  observeEvent(input$do3,{
  
  output$text2 <- renderUI({

  hR1=hR()
  d=hR1$d
  print(d$rl)

  })
  })
  
  gincon<-eventReactive(input$do4,{
    hR1=hR()
    b=hR1$b
    gincon=initconditions(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S, Nsim=input$Nsim,durt=input$durt,writeResults=FALSE,PDFplots=FALSE)
    
  })
  observeEvent(input$do4,{
  output$plot3<- renderPlot({
    gincon1=  gincon()
    if(input$radio2=="Ft"){
    print(gincon1$plot1)
    }
    if(input$radio2=="Wt"){
      print(gincon1$plot2)
    }
    if(input$radio2=="Spt"){
      print(gincon1$plot3)
    }
    if(input$radio2=="St"){
      print(gincon1$plot4)
    }
    # int1=read.table(paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
    #  StochasticPQRUTSeNorge:::smoothhydropairs(int1)
    #  if(input$radio1==2){
    #    for(seasn in c("Ft","Wt","Spt", "St")){
    #  int1=read.table(paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
    #  int1$sl=0
    # write.table(int1,paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
    #    }
     # StochasticPQRUTSeNorge:::smoothhydropairs(int1)
     # }
  }) 
  })
  
  
  gSt<-eventReactive(input$do5,{
    withProgress(message = 'Generating Q ', value = 0, {
    gincon1=  gincon()
    hR1=hR()
    h=hR1$h

    incProgress(0, detail = paste("season 09-11"))
    gFT= hydrolsim(seasn="Ft",param.station=c(input$K1,input$K2,input$T),Nsim=input$Nsim,int1=gincon1$SWEFt,Pt=as.matrix(h[[1]]),E=h[[5]],durt=input$durt,Area1=input$Area,kd=input$Kd,snpSpt=0.3,ttsnow=-1,Tmax=input$Tmelt,Tmin=input$Tmelt,writeResults=FALSE,PDFplots=FALSE)
    incProgress(1/4, detail = paste(" season 12-02"))
     gWT= hydrolsim(seasn="Wt",param.station=c(input$K1,input$K2,input$T),Nsim=input$Nsim,int1=gincon1$SWEFt,Pt=as.matrix(h[[2]]),E=h[[6]],durt=input$durt,Area1=input$Area,kd=input$Kd,snpSpt=0.3,ttsnow=-1,Tmax=input$Tmelt,Tmin=input$Tmelt,writeResults=FALSE,PDFplots=FALSE)
     incProgress(2/4, detail = paste("season 03-05"))
     gSpT= hydrolsim(seasn="Spt",param.station=c(input$K1,input$K2,input$T),Nsim=input$Nsim,int1=gincon1$SWEFt,Pt=as.matrix(h[[3]]),E=h[[7]],durt=input$durt,Area1=input$Area,kd=input$Kd,snpSpt=0.3,ttsnow=-1,Tmax=input$Tmelt,Tmin=input$Tmelt,writeResults=FALSE,PDFplots=FALSE)
     incProgress(3/4, detail = paste(" season 06-08"))
      gST= hydrolsim(seasn="St",param.station=c(input$K1,input$K2,input$T),Nsim=input$Nsim,int1=gincon1$SWEFt,Pt=as.matrix(h[[4]]),E=h[[8]],durt=input$durt,Area1=input$Area,kd=input$Kd,snpSpt=0.3,ttsnow=-1,Tmax=input$Tmelt,Tmin=input$Tmelt,writeResults=FALSE,PDFplots=FALSE)
    return(list( gFT= gFT,gWT=gWT,  gSpT=  gSpT,gST=gST))
  })
  
  })
  
  observeEvent(input$do5,{
  output$plot4<- renderPlot({
    inFile <- input$Qh
    Qh =  read.csv(inFile$datapath)
    gsim1=gSt()
    hR1=hR()
    d1=hR1$d[2:5]
    h=hR1$h
    gincon1=  gincon()
    
    m1=annualfreqplot(qtT=0.9,Nsim=input$Nsim,durt=input$durt,qobs=Qh,Pint=h[10:13],incond=gincon1,nsy=d1,Qsim=gsim1,writeResults=FALSE,PDFplots=FALSE)
                      
  #  withProgress(message = 'Generating flood events ', value = 0, {
  # # observeEvent(input$do5, {
  #    input$do5
  #    if (input$do5 == 0)
  #      return()
  #   this.station<<-gsub(pattern = "QJ.csv","",x = input$QJ)
  #   pathmain=input$stn
  #   kd=input$Kd
  #   param.station=isolate(c(input$K1,input$K2,input$T))
  #   print(param.station)
  #   Tmelt=input$Tmelt
  #   PrSn=input$PrSn
  #   incProgress(0, detail = paste("season 09-11"))
  #   seasn="Ft"
  #   isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="Ft",param.station=param.station,Nsim=input$Nsim,durt=input$durt
  #                   ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
  #   incProgress(1/4, detail = paste(" season 12-02"))
  #    seasn="Wt"
  #    isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="Wt",param.station=param.station,Nsim=input$Nsim,durt=input$durt
  #             ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
  #   incProgress(2/4, detail = paste("season 03-05"))
  #   seasn="Spt"
  #   isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="Spt",param.station=param.station,Nsim=input$Nsim,durt=input$durt
  #             ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
  #   incProgress(3/4, detail = paste(" season 06-08"))
  #   seasn="St"
  #   isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="St",param.station=param.station,Nsim=input$Nsim,durt=input$durt
  #             ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
  #   annualfreqplot(pathmain,qtT=input$qT,Nsim=input$Nsim,durt=input$durt,mult=FALSE,PDFplots = FALSE)
  #   #annualfreqplot(pathmain,qtT=0.9,Nsim=1000,durt=48,mult=FALSE,PDFplots = FALSE)
  #   })
  })
 })
  # observeEvent(input$do4, {
  #   output$plot5 <- renderPlot({ 
  #     pathmain=input$stn
  #     durt=as.numeric(as.character(read.table(file.path(  pathmain,"duration.txt"))$V1[2]))
  #     print(durt)
  #     kd=input$Kd
  #     param.station=c(input$K1,input$K2,input$T)
  #     print(input$k)
  #     Qm<<- checkHydrograph(pathmain=input$stn,seasn=input$radio7,param.station,Nsim=input$Nsim,durt,k=(input$k)
  #                           ,Area1=input$Area,kd,modelsnow="Snow.sim",slconst=1,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5)
  #     
  #     n2=Qm[[1]]
  #     evt1=data.frame(date=1:durt,P1=n2[,3],P2=n2[,2]+n2[,3],Q=n2[,1])
  #     
  #     plot(evt1[,4],type="l",ylab="Q",main=paste0(" soil moisture deficit=",round(Qm[[2]][3],1)))
  #     par(new = T)
  #     barplot(evt1[,2], yaxt = "n", space = NULL,
  #             ylim = rev(c(0, 4 * max(na.omit(evt1[,2])))),
  #             xaxt = "n",col=rgb(0, 0, 1, .3) )
  #     par(new = T)
  #     barplot(evt1[,3], yaxt = "n", space = NULL,
  #             ylim = rev(c(0, 4 * max(na.omit(evt1[,2])))),
  #             xaxt = "n",col=rgb(0, 0, 0, .1) )
  #     axis(side = 3, pos = 0, tck = 0,xaxt = "n")
  #     axis(side = 4, at = seq(0, floor(max(na.omit(evt1[,3])) +
  #                                        1), length = (1 + ifelse(floor(max(na.omit(evt1[,3])) +
  #                                                                         1) < 10, floor(max(na.omit(evt1[,3])) + 1),
  #                                                                 4))), labels = as.integer(seq(0, floor(max(na.omit(evt1[,3])) +
  #                                                                                                          1), length = (1 + ifelse(floor(max(na.omit(evt1[,3])) +
  #                                                                                                                                           1) < 10, floor(max(na.omit(evt1[,3])) + 1), 4)))))
  #     
  #     
  #     
  #   })
  #})
})

