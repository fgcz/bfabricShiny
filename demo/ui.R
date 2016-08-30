#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
qcdata <-read.csv("C:/Users/christian/Documents/Learning_R_scripts/Testdata_Raw_analyzer/Bernd/20160409_05_GM_06.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("ChT test plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # sliderInput("bins",
      #             "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
      # sliderInput("bins2",
      #             "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
      # selectInput("dataset", "Choose a dataset:", 
      #             choices = colnames(state.x77)),
    
    selectInput('xcol', 'X Variable', names(qcdata)),
    
    selectInput('ycol', 'Y Variable', names(qcdata),
                selected=names(qcdata)[[2]]),
    
    numericInput('MS1', 'MS1', 64,
                  min = 32, max = 256, step = 32)
  ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot1")
    )
  )
))
