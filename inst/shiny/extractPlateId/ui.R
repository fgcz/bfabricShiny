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
  titlePanel(sprintf("FGCZ Metabolomics - plate info extraction")),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("bfabricUser"),
      uiOutput("plateID"),
      uiOutput("instrument")
      #uiOutput("actionGenerateReportButton"),
      #uiOutput("downloadReportButton")
   ),
    
    mainPanel(
      list(
        #uiOutput("title"),
        uiOutput("outputKable")
        #uiOutput("textTemplate")
      )
    )
  )
))
