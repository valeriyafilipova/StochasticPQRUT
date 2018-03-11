#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
options(shiny.maxRequestSize = 20*1024^2)
options(rsconnect.max.bundle.size=1004572800)
shinyUI(
  navbarPage("Stochastic PQRUT",
             tabPanel(
               "Critical duration",
               sidebarLayout(
                 sidebarPanel(

                  fileInput("QJ",label = h3("observed Q data;csv file with columns with columns date (YYYY-mm-dd) and Q"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                           #  label = h3("path to observed Q data"),"16.193QJ.csv"),
                  fileInput("PJ",label = h3("observed P data;csv file with columns with columns date (YYYY-mm-dd) and Pr"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                  #  label = h3("path to observed Q data"),"16.193QJ.csv"),
                  fileInput("sl",label = h3("soil moisture deficit data;csv file with columns with columns date (YYYY-mm-dd) and sl"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                  fileInput("SWE",label = h3("SWE data;csv file with columns with columns date (YYYY-mm-dd) and SWE"),   accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
                 numericInput("Nsim",
                           label = h3("Number of simulations"),1000),
               numericInput("qT",
                            label = h3("Threshold quantile"),value=0.9,step=0.01),
               actionButton("do1", "Submit")
               ),
                 mainPanel( plotOutput("plot1"), 
                            textOutput("text1")
                            )
               )
             ),
             tabPanel("Fit distribution to Rainfall data",       
                      sidebarLayout(
               sidebarPanel(
      
                 # Make a list of checkboxes
                 radioButtons("radio", label = h3("Statistical Distribution"),
                              choices = list("Exponential" = "Exponential", "Generalised Pareto" = "GP") ),
                 numericInput("qT1",
                              label = h3("Threshold quantile for season 09-11"),value=0.9,step=0.01,),
                 numericInput("qT2",
                              label = h3("Threshold quantile for season 12-02"),value=0.9,step=0.01),
                 numericInput("qT3",
                              label = h3("Threshold quantile for season 03-05"),value=0.9,step=0.01),
                 numericInput("qT4",
                              label = h3("Threshold quantile for season 06-08"),value=0.9,step=0.01),
                 numericInput("durt",
                              label = h3("critical duration"),value=24,step=24),
                 fileInput("Ph",label = h3(" hourly P data,csv file with columns Date YYYY-mm-dd, time=HH:MM:SS and value"),   accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")),
                 fileInput("h1",
                           label = h3("hourly T data,csv file with columns Date YYYY-mm-dd, time=HH:MM:SS and value"), accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                 
                 actionButton("do2", "Submit"),
                 actionButton("do3", "Generate P,T values")
                 
               ),
               mainPanel( plotOutput("plot2"),
                          htmlOutput("text2")
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
                 radioButtons("radio2", h3("Season"),
                              choices = list("Autumn" = "Ft", "Winter" = "Wt",
                                             "Spring" = "Spt","Summer" = "St"),selected = "Ft"),
                 actionButton("do4", "Submit")
               ),
              
               mainPanel( plotOutput("plot3") )
             )
             ),
             tabPanel("Simulate events",
                     fluidPage(
              
                          # Make a list of checkboxes
                          fluidRow(
                            column(3, 
                                   numericInput("K1",
                                                label = h3("K1"),value = 0.1138291,step=0.01)),
                            column(3, 
                                   numericInput("K2",
                                                label = h3("K2"),value = 0.028429,step=0.01)),
                            column(3,
                                   numericInput("T",
                                                label = h3("T"),value = 19.85680,step=1)
                            )
                        ),
                        fluidRow(
                          column(5, 
                                 numericInput("Kd",
                                              label = h3("Kd"),value = 2.85,step=0.01)),
                          column(5, 
                                 numericInput("Tmelt",
                                              label = h3("Tmelt"),value = 0.5,step=0.01)),
                          column(5,
                                 numericInput("PrSn",
                                              label = h3("PrSn"),value = 0.3,step=0.1)
                          ),
                          column(5,
                                 numericInput("Area",
                                              label = h3("Area"),value = 157,step=1)
                          ),
                          column(5,
                          fileInput("Qh",label = h3(" hourly Q data,csv file with columns Date (dd/mm/YYYY HH:MM:SS) and Q"),   accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"))
                          ),
                          actionButton("do5", "Submit")
                        ),
                        mainPanel( plotOutput("plot4") )
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

