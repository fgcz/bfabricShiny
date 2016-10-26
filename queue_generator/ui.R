
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
    column(3, htmlOutput("project")),
    
    column(3, htmlOutput("sample")),
    column(3, htmlOutput("extract"))
    ),
 fluidRow(
    column(3, htmlOutput("instrument")),
    column(3, htmlOutput("login")),
    column(3, htmlOutput("howoften")),
    column(3, htmlOutput("howmany"))
  ),
 
    # Show a plot of the generated distribution
    
      fluidRow(
        DT::dataTableOutput("table")
      ),
 fluidRow(
 downloadButton('downloadData', 'Download')
    ))
  )
)
