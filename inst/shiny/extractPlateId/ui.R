#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

stopifnot(require(shiny), require(bfabricShiny))

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel(sprintf("MS Queue Generator - using Plate IDs as input")),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("bfabricUser"),
      uiOutput("orderID"),
      uiOutput("area"),
      uiOutput("instrument"),
      uiOutput("plateID"),
      uiOutput("injvol"),
      uiOutput("extratext"),
      uiOutput('run'),
      uiOutput("downloadReportButton")
   ),
    
    mainPanel(
      list(
        uiOutput("outputKable")
      )
    )
  )
))
