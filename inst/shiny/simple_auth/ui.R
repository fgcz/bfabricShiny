#R

library(bfabricShiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("FGCZ bfabric Web Service Password Test"),

  # Sidebar with a slider input for number of bins 
  # "$2a$10$We8McOYkCp7iCFzaTCgDoepBe2KkrzkiLKvh0o.v9u8tIQCYmD.D6"
  sidebarLayout(
    sidebarPanel(
      #initStore("store", "shinyStore-ex2", privKey), 
      bfabricInput("bfabric8"), 
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")  
       #DT::dataTableOutput("table")
    )
  )
))
