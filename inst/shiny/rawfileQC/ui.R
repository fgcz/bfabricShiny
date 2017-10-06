#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(bfabricShiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("bfabric application ID 225 - Thermo Fisher Raw File QC"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("bfabric", bfabricInput("bfabric8")),
        tabPanel("generate report", list(tags$hr(),
                                         # uiOutput("parameterUI"),
                                         tags$hr(),
                                         uiOutput("generateReportButton"),
                                         tags$hr(),
                                         uiOutput("downloadReport"),
                                         tags$hr(),
                                         uiOutput("wuid"))),
        tabPanel("sessionInfo", verbatimTextOutput("sessionInfo"))
      )),
    # Show a plot of the generated distribution
    mainPanel(
      list(
        tags$hr(),
        uiOutput("fileInformation")
      )
    )
    
  )
))
