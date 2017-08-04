## TESTESTTEST ##

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Two Group Comparison"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('proteinGroups', 'Choose MQ ProteinGroups File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv','.zip')),
      tags$hr(),
      uiOutput("parameterUI"),
      tags$hr(),
      uiOutput("generatereportbutton"),
      tags$hr(),
      uiOutput("downloadreport")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("fileInformation")
    )
  )
))
