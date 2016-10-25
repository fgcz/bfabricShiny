
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(jsonlite)


shinyServer(function(input, output) {

  output$project <- renderUI({
    res.project <- c(1000, 1959, 2121)
    selectInput('project', 'project', res.project, multiple = FALSE, selected = 1000)
  })
  
  
  output$sample <- renderUI({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/projectid/", input$project, sep='')))
    selectInput('sample', 'sample', paste(res$sample.id, res$sample.name, sep='-'), multiple = TRUE)
  })
  
  output$extract <- renderUI({

    if (length(input$sample) > 0){
      sample.id <- sapply(strsplit(input$sample, split='-'), function(x){x[1]})
      
      
      res <- do.call('rbind', 
                     lapply(sample.id, 
                            function(sampleid){
                              as.data.frame(fromJSON(paste("http://localhost:5000/sampleid/", sampleid, sep='')))
                              }))
      

        
      selectInput('extract', 'extract', res$extract.name, multiple = TRUE)
    }
    
  })
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
