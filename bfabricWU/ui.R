
# see also RJSON on 
# http://fgcz-intranet.uzh.ch/tiki-index.php?page=cp


library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Demo bfabric RJSON"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      selectInput("wuId", "workunit ID", 142914:142920),
      htmlOutput("bfabric_resources"),
      br(),
      actionButton("loadButton", "load data"),
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
