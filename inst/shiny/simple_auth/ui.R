#R

library(bfabricShiny)


shinyUI(fluidPage(
  # Application title
  titlePanel("bfabricShiny module test"),
  p("browse through your bfabric data."),
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
