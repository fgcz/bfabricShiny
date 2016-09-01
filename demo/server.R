#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
  
      #select a folder for loading data
      output$INPUT <- renderUI({
        files <- list.files(path = "C:/Users/christian/Documents/Learning_R_scripts/Testdata_Raw_analyzer")
        selectInput('file', 'file', files[grepl(".csv$", files)])
        
        
        # qcdata <- eventReactive(input$goButton, {
        #     input$file
        #  }),
        
        # output$nText <- renderText({
        #   ntext()
        })
      })
  
      
      
      
      # Combine the selected variables into a new data frame
     
      selectedData <- reactive({
      qcdata[, c(input$xcol, input$ycol)]
       })
  
  

    output$plot1 <- renderPlot({
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           pch = 20, cex = 1)
      abline(h = input$MS1, col = "red")
     
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #myCol <- input$dataset
    #x    <- state.x77[, which(colnames(state.x77) == input$dataset)]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white', main = input$dataset)
    
  })
  
})


