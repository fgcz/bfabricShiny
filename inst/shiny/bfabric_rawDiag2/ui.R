#R rawDiag2 shiny app





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
      bslib::navset_card_underline(
        bslib::nav_panel("rawDiag", rawDiag::rawDiagUI("OrbitrapFun02")),
        bslib::nav_panel("Biognosys iRT XICs",  htmlOutput("rawrr")),
        #  )
      )
    )
  )
)
