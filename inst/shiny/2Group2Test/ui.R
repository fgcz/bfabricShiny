## TESTESTTEST ##

library(bfabricShiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Two Group Comparison"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("bfabric", bfabricInput("bfabric8")),
        tabPanel("generate report", list(tags$hr(),
                                         uiOutput("parameterUI"),
                                         tags$hr(),
                                         
                                         uiOutput("generatereportbutton"),
                                         tags$hr(),
                                         uiOutput("downloadreport"))),
        tabPanel("sessionInfo", verbatimTextOutput("sessionInfo"))
    )),
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("fileInformation")
      )
    
  )
))
