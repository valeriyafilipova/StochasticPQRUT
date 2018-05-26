#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
options(shiny.maxRequestSize = 20*1024^2)
options(rsconnect.max.bundle.size=1004572800)
library(shinythemes)
shinyUI(
  navbarPage(theme=shinytheme(theme = "united"),"Stochastic PQRUT",
             tabPanel(
               "Critical duration",
               sidebarLayout(
                 sidebarPanel(
                  h3("Input data:"),
                   #"observed Q data;csv file with columns with columns date (YYYY-mm-dd) and Q"
                  fileInput("QJ",label = h5("observed Q data;",br(),"csv file with columns with columns date (YYYY-mm-dd) and Q"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                  
                  #"observed P data;csv file with columns with columns date (YYYY-mm-dd) and Pr")
                           #  label = h3("path to observed Q data"),"16.193QJ.csv"),
               (fileInput("PJ",label = h5("observed P data;",br(),"csv file with columns with columns date (YYYY-mm-dd) and Pr"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"))),
               
                  #  label = h3("path to observed Q data"),"16.193QJ.csv"),
                  #"soil moisture deficit data;csv file with columns with columns date (YYYY-mm-dd) and sl
                  fileInput("sl",label = h5("soil moisture deficit data;",br(),"csv file with columns with columns date (YYYY-mm-dd) and sl"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                  #"SWE data;csv file with columns with columns date (YYYY-mm-dd) and SWE"
                  fileInput("SWE",label = h5("SWE data;",br(),"csv file with columns with columns date (YYYY-mm-dd) and SWE"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                 numericInput("Nsim",
                           label = h5("Number of simulations"),1000),
               numericInput("qT",
                            label = h5("Threshold quantile"),value=0.9,step=0.01),
               actionButton("do1", "Submit")
               ),
                 mainPanel( helpText("Calculates critical duration,based on lag between Q and P values:"),
                   plotOutput("plot1"), 
                            textOutput("text1")
                            )
               )
             ),
             tabPanel("Fit distribution to Rainfall data",       
                      fluidPage(
                        fluidRow(
                          column(3,
      
                 # Make a list of checkboxes
                 # radioButtons("radio", label = h5("Statistical Distribution"),
                 #              choices = list("Exponential" = "Exponential", "Generalised Pareto" = "GP"),
                 #           ),   
                 numericInput("durt",
                                             label = h5("critical duration"),value=24,step=24),
                 actionButton("do2", "Fit Distribution"),
                 actionButton("do3", "Generate P,T values")),
                 column(3,
                 numericInput("qT1",
                              label = h5("Threshold quantile for season 09-11"),value=0.98,step=0.01),
                 numericInput("qT2",
                              label = h5("Threshold quantile for season 12-02"),value=0.99,step=0.01)),
                 column(3,
                 numericInput("qT3",
                              label = h5("Threshold quantile for season 03-05"),value=0.99,step=0.01),
                 numericInput("qT4",
                              label = h5("Threshold quantile for season 06-08"),value=0.98,step=0.01)),
                 column(3,
                
                 #" hourly P data,csv file with columns Date YYYY-mm-dd, time=HH:MM:SS and value"
                 fileInput("Ph",label = h5(" hourly P data,csv file with columns Date(dd/mm/YYYY), time=HH:MM:SS and value"),   accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")),
                 # "hourly T data,csv file with columns Date YYYY-mm-dd, time=HH:MM:SS and value"
                 fileInput("h1",
                           label = h5("hourly T data,csv file with columns Date(YYYY-mm-dd), time=HH:MM:SS and value"), accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
                 
                )
                 
               ),
               mainPanel( plotOutput("plot2"),
                        textOutput("text2")
                          )
             )
             ),
             tabPanel("Generate initial conditions",         
                      sidebarLayout(
               sidebarPanel(
                 # Make a list of checkboxes
              
                 # radioButtons("radio1", label = h3("soil moisture deficit"),
                 #              choices = list("based on SeNorge model" = 1, "Set to 0" = 2)
                 # ),
                 radioButtons("radio2", h5("Season"),
                              choices = list("Autumn" = "Ft", "Winter" = "Wt",
                                             "Spring" = "Spt","Summer" = "St"),selected = "Ft"),
                 actionButton("do4", "Submit")
               ),
              
               mainPanel( helpText("Comparison between generated values and input values:"),plotOutput("plot3") )
             )
             ),
             tabPanel("Simulate events",
                     fluidPage(
                       h4("PQRUT parameters:"),
                          # Make a list of checkboxes
                          fluidRow(
                            column(3, 
                                   numericInput("K1",
                                                label = h5("K1"),value = 0.1138291,step=0.01)),
                            column(3, 
                                   numericInput("K2",
                                                label = h5("K2"),value = 0.028429,step=0.01)),
                            column(3,
                                   numericInput("T",
                                                label = h5("T"),value = 19.85680,step=1)
                            )
                        ),
                        fluidRow(
                          column(3, 
                                 numericInput("Kd",
                                              label = h5("Kd"),value = 2.85,step=0.01)),
                    
                
                          column(3,
                                 numericInput("Area",
                                              label = h5("Area"),value = 157,step=1)
                          ),
                          column(3,
                                 #" hourly Q data,csv file with columns Date (dd/mm/YYYY HH:MM:SS) and Q"
                          fileInput("Qh",label = h5("hourly Q data,csv file with columns Date (dd/mm/YYYY HH:MM:SS) and Q"),   accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"))
                          ),
                          actionButton("do5", "Submit")
                        ),
                        mainPanel( helpText("Calculate CDF:"),plotOutput("plot4") )
                      )
             )
             # tabPanel("Explore events",
             #          sidebarLayout(
             #            sidebarPanel(
             #              # Make a list of checkboxes
             #              numericInput("k",
             #                           label = h3("Event number"),1),
             #       
             #              radioButtons("radio7", h3("Season"),
             #                           choices = list("Autumn" = "Ft", "Winter" = "Wt",
             #                                          "Spring" = "Spt","Summer" = "St"),selected = "Ft"),
             #              actionButton("do6", "Submit")
             #            ),
             #            mainPanel( plotOutput("plot5") )
             #          )
             # )
  )
)

