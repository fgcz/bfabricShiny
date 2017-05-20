#R

library(bfabricShiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dd <- callModule(shinyServerModule, "bfabric8", applicationid=168)
  
  #print (dd)
  #getApplicationID <- 205;

  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
})
