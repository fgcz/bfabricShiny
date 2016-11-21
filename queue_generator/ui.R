
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyUI(fluidPage(

  fluidPage(
  # Application title
  titlePanel("FGCZ MS Queue Generator"),
  fluidRow(
    column(2, htmlOutput("project")),
    
    column(3, htmlOutput("sample")),
    column(7, htmlOutput("extract"))
    ),
  fluidRow(
    column(3, htmlOutput("area")),
    column(3, htmlOutput("instrument")),
    column(3, htmlOutput("login"))),
 fluidRow(
  
    column(3, htmlOutput("howoften")),
    column(3, htmlOutput("howmany"))
  ),
 
    # Show a plot of the generated distribution
    
      fluidRow(
        DT::dataTableOutput("table")
      ),
 #fluidRow(
 column(4, wellPanel(
 	downloadButton('downloadData', 'Download'),
    	actionButton("bfabricButton", "Add table as dataset to bfabric."),
    	singleton( tags$head(tags$script(src = "message-handler.js")))
    )
  )
) 
)
)
