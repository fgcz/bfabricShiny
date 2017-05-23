#R

library(bfabricShiny)


shinyUI(fluidPage(
  # Application title
  titlePanel("FGCZ bfabric Web Service Password Test"),

  sidebarLayout(
    sidebarPanel(
      bfabricInput("bfabric8"), 
      htmlOutput("load"),
      checkboxInput("ssh", "use SSH", FALSE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      DT::dataTableOutput("proteinGroupsTable")
    )
  )
))
