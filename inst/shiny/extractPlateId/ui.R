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
  titlePanel(sprintf("MS Queue Generator - using b-fabric plates")),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("bfabricUser"),
      uiOutput("orderID"),
      uiOutput("area"),
      uiOutput("instrument"),
      uiOutput("system"),
      uiOutput("selectqFUN"),
      uiOutput("instrumentMode"),
      uiOutput("plateID"),
      uiOutput("injvol"),
      uiOutput("randomization"),
      uiOutput("frequency"),
      uiOutput("checkSampleSelection"),
      uiOutput("selectSampleSelection"),
      htmlOutput("download"),
   ),
    mainPanel(
      list(
        DT::dataTableOutput("outputKable")
      )
    )
  )
))
