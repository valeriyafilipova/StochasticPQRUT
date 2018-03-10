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


shinyServer( function(input, output) {
  #observeEvent(input$do1, {
  
  inFile <- input$QJ
  QJ=    read.csv(inFile$datapath)
  print(head(QJ))
  inFile <- input$PJ
  P =  read.csv(inFile$datapath)
  print(head(P))
  a<-eventReactive(input$do1, {
 # output$plot1 <- renderPlot({


    qtT=input$qT
(criticalduration(Q=QJ,P = P,qtT=qtT,intEvent = 7,PDFplots = FALSE,writeResults = FALSE))

 })
  output$text1 <- renderText({ 
 #   pathmain=input$stn

  isolate( paste("critical duration=",a[[1]]))
})
#  })
  observeEvent(input$do2, {

output$plot2 <- renderPlot({

  inFile <- input$PJ
  P =  read.csv(inFile$datapath)
    pathmain=input$stn
    qtT1=input$qT1
    par(mfrow=c(2,2))
    rainPOT(datarainfall = P,durt=input$durt,qFtset=qtT1,qWtset=input$qT2,qSptset=input$qT3,qStset=input$qT4,distfunc=input$radio,writeResults = FALSE,PDFplots = FALSE)
  #  observeEvent(input$do6, {
    if(input$do6) {
      (rainsimulate1(pathmain,Nsim=input$Nsim,durt=input$durt,distfunc = input$radio))
   # })
    }
  })
  })

  output$text2 <- renderUI({
   
    
    input$do2
    if (input$do2 == 0)
      return()
    pathmain=input$stn
   durt= input$durt
    sesn="Ft"
    stj1n=read.table(file.path(pathmain,paste0("/Exp", durt,"new/"),paste0("P", durt,sesn,"new.txt"),sep=""),sep=",",header = TRUE)
    
    str1 <- paste("Number of events 09-11", round(nrow(stj1n)/(stj1n$year[nrow(stj1n)]-stj1n$year[1]),2))
    sesn="Wt"
    stj2n=read.table(file.path(pathmain,paste0("/Exp",input$durt,"new/"),paste0("P",input$durt,sesn,"new.txt"),sep=""),sep=",",header = TRUE)
    
    str2 <- paste("Number of events 12-02",round(nrow(stj2n)/(stj2n$year[nrow(stj2n)]-stj2n$year[1]),2))
    sesn="Spt"
    stj3n=read.table(file.path(pathmain,paste0("/Exp", input$durt,"new/"),paste0("P", input$durt,sesn,"new.txt"),sep=""),sep=",",header = TRUE)
    
    str3 <- paste("Number of events 03-05", round(nrow(stj3n)/(stj3n$year[nrow(stj3n)]-stj3n$year[1]),2))
    sesn="St"
    stj4n=read.table(file.path(pathmain,paste0("/Exp", input$durt,"new/"),paste0("P", input$durt,sesn,"new.txt"),sep=""),sep=",",header = TRUE)
    
    str4 <- paste("Number of events 06-08", round(nrow(stj4n)/(stj4n$year[nrow(stj4n)]-stj4n$year[1]),2))
    str51=read.table(file.path(pathmain,paste0("/Exp",input$durt,"new/"),"rtlvl.txt",sep=""),sep=" ",header = TRUE)
    str5= paste("return level 09-11=",round(str51[1,1],2))
    str6= paste("return level 12-02=",round(str51[2,1],2))
    str7= paste("return level 03-05=",round(str51[3,1],2))
    str8= paste("return level 06-08=",round(str51[4,1],2))
    HTML(paste(str1, str2,str3,str4, str5, str6, str7, str8, sep = '<br/>'))

 # })  
  })  
  
  
  output$plot3<- renderPlot({

    durt=input$durt
    pathmain=input$stn
    this.station<<-gsub(pattern = "QJ.csv","",x = input$QJ)
    initcond(pathmain,this.station = this.station,durt=input$durt,PDFplots=FALSE)
    pathmain=input$stn
     Nsim=input$Nsim
    initconditions(pathmain,Nsim=Nsim,durt=durt,PDFplots=FALSE) 
    seasn=input$radio2
    int1=read.table(paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
     StochasticPQRUTSeNorge:::smoothhydropairs(int1)
     if(input$radio1==2){
       for(seasn in c("Ft","Wt","Spt", "St")){
     int1=read.table(paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
     int1$sl=0
    write.table(int1,paste(pathmain,"/evtExp/",seasn,"/inctmv/incond",seasn,"1.txt",sep=""))
       }
     StochasticPQRUTSeNorge:::smoothhydropairs(int1)
     }
  }) 
  
  
  
  output$plot4<- renderPlot({
   withProgress(message = 'Generating flood events ', value = 0, {
  # observeEvent(input$do5, {
     input$do5
     if (input$do5 == 0)
       return()
    this.station<<-gsub(pattern = "QJ.csv","",x = input$QJ)
    pathmain=input$stn
    kd=input$Kd
    param.station=isolate(c(input$K1,input$K2,input$T))
    print(param.station)
    Tmelt=input$Tmelt
    PrSn=input$PrSn
    incProgress(0, detail = paste("season 09-11"))
    seasn="Ft"
    isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="Ft",param.station=param.station,Nsim=input$Nsim,durt=input$durt
                    ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
    incProgress(1/4, detail = paste(" season 12-02"))
     seasn="Wt"
     isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="Wt",param.station=param.station,Nsim=input$Nsim,durt=input$durt
              ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
    incProgress(2/4, detail = paste("season 03-05"))
    seasn="Spt"
    isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="Spt",param.station=param.station,Nsim=input$Nsim,durt=input$durt
              ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
    incProgress(3/4, detail = paste(" season 06-08"))
    seasn="St"
    isolate(hydrolsim(pathmain=pathmain,mult=FALSE,sn="St",param.station=param.station,Nsim=input$Nsim,durt=input$durt
              ,Area1=input$Area,kd=kd,modelsnow="Snow.sim",slconst=1,snpSpt = PrSn,ttsnow = -1,Tmax=Tmelt,Tmin=Tmelt))
    annualfreqplot(pathmain,qtT=input$qT,Nsim=input$Nsim,durt=input$durt,mult=FALSE,PDFplots = FALSE)
    #annualfreqplot(pathmain,qtT=0.9,Nsim=1000,durt=48,mult=FALSE,PDFplots = FALSE)
    })
  })
#  })
  observeEvent(input$do4, {
    output$plot5 <- renderPlot({ 
      pathmain=input$stn
      durt=as.numeric(as.character(read.table(file.path(  pathmain,"duration.txt"))$V1[2]))
      print(durt)
      kd=input$Kd
      param.station=c(input$K1,input$K2,input$T)
      print(input$k)
      Qm<<- checkHydrograph(pathmain=input$stn,seasn=input$radio7,param.station,Nsim=input$Nsim,durt,k=(input$k)
                            ,Area1=input$Area,kd,modelsnow="Snow.sim",slconst=1,snpSpt=0.3,ttsnow=-1,Tmax=0.5,Tmin=0.5)
      
      n2=Qm[[1]]
      evt1=data.frame(date=1:durt,P1=n2[,3],P2=n2[,2]+n2[,3],Q=n2[,1])
      
      plot(evt1[,4],type="l",ylab="Q",main=paste0(" soil moisture deficit=",round(Qm[[2]][3],1)))
      par(new = T)
      barplot(evt1[,2], yaxt = "n", space = NULL,
              ylim = rev(c(0, 4 * max(na.omit(evt1[,2])))),
              xaxt = "n",col=rgb(0, 0, 1, .3) )
      par(new = T)
      barplot(evt1[,3], yaxt = "n", space = NULL,
              ylim = rev(c(0, 4 * max(na.omit(evt1[,2])))),
              xaxt = "n",col=rgb(0, 0, 0, .1) )
      axis(side = 3, pos = 0, tck = 0,xaxt = "n")
      axis(side = 4, at = seq(0, floor(max(na.omit(evt1[,3])) +
                                         1), length = (1 + ifelse(floor(max(na.omit(evt1[,3])) +
                                                                          1) < 10, floor(max(na.omit(evt1[,3])) + 1),
                                                                  4))), labels = as.integer(seq(0, floor(max(na.omit(evt1[,3])) +
                                                                                                           1), length = (1 + ifelse(floor(max(na.omit(evt1[,3])) +
                                                                                                                                            1) < 10, floor(max(na.omit(evt1[,3])) + 1), 4)))))
      
      
      
    })
  })
})

