
# see also RJSON on 
# http://fgcz-intranet.uzh.ch/tiki-index.php?page=cp


library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Demo bfabric RJSON"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      numericInput("wuId", "workunit ID", 142914, 142913, 15000),
      htmlOutput("bfabric_resources"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
