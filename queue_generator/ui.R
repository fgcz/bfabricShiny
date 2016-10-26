
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
    column(4, htmlOutput("project")),
    column(4, htmlOutput("sample")),
    column(4, htmlOutput("extract"))
    ),
 fluidRow(
    column(4, htmlOutput("instrument")),
    column(4, htmlOutput("howoften")),
    column(4, htmlOutput("howmany"))
  ),

    # Show a plot of the generated distribution
    
      fluidRow(
        DT::dataTableOutput("table")
      )
    )
  )
)
