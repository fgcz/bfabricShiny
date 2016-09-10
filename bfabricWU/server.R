# see also RJSON on 
# http://fgcz-intranet.uzh.ch/tiki-index.php?page=cp


library(shiny)
library(jsonlite)
shinyServer(function(input, output, session) {
  
  output$bfabric_resources <- renderUI({
    wuid <- input$wuId
    url <- paste("http://localhost:5000/bfabric/api/workunitid", wuid, sep='/')
    res <- fromJSON(url)
    selectInput('file', 'file', res$resources)
  })
  
  output$distPlot <- renderPlot({
    
    
    
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
