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
  titlePanel("ProgenesisQI"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("bfabric", bfabricInput("bfabric8"))),
       selectInput("sep", "csv file separator", list(";", ",")),
      uiOutput("generateReportButton"),
      tags$hr(),
      uiOutput("wuid")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("measurements",
      DT::dataTableOutput("measurements")),
      tabPanel("identifications",
               DT::dataTableOutput("identifications")),
      tabPanel("sessionInfo", verbatimTextOutput("sessionInfo")))
      
      # plotOutput("distPlot",height = 600)
    )
  )
))
