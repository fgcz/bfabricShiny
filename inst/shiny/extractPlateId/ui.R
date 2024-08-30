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
      hr(),
      uiOutput("orderID"),
      uiOutput("area"),
      uiOutput("instrument"),
      uiOutput("system"),
      uiOutput("selectqFUN"),
      uiOutput("instrumentMode"),
      uiOutput("plateID"),
      uiOutput("checkSampleSelection"),
      uiOutput("selectSampleSelection"),
      uiOutput("injvol"),
      uiOutput("randomization"),
      uiOutput("frequency"),

      htmlOutput("download"),
      hr(),
      a("b-fabric application 319 page", href="https://fgcz-bfabric.uzh.ch/bfabric/application/show.html?id=319"),
      br(),
      a("internal queue generator tiki-wiki page", href="https://fgcz-intranet.uzh.ch/tiki-index.php?page=sw.queueGenerator"),
   ),
    mainPanel(
      list(
        DT::dataTableOutput("outputKable")
      )
    )
  )
))
