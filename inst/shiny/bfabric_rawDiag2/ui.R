
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#R

stopifnot(require(rawDiag))
stopifnot(require(bfabricShiny))


# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("rawDiag - Brings Orbitrap Mass Spectrometry Data to Life; Fast and Colorful"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      tagList(
        bfabricShiny::bfabricInput("bfabric13"),
        htmlOutput("bfabricWorkunitId"),
        actionButton('resetBfabricWorkunitId', 'resetWuId'),
        actionButton('generatePDF', 'generatePDF')
      )
    ),
    mainPanel(
      tagList(
        img(src='octopussy.png ', align = "right", width = '50px'),
        br(),
        tagList(
          rawDiag::rawDiagUI("OrbitrapFun02"),
          
        )
      )
    )
  )
)
