#R
# shiny::runApp('/Users/cp/__checkouts/R/bfabricShiny/inst/shiny/simple_auth/', display.mode = "normal", port=8080)

library(bfabricShiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  bfsm <- callModule(shinyServerModule, "bfabric8",  applicationid = c(168, 204))
  
 print("DEBUG BFSM" )
 print(bfsm)
 print("END DEBUG BFSM")
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #print(bfsm)
    # draw the histogram with the specified number of bins
    print(bfsm$resources)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main='')
  })
  #print (dd)
  #getApplicationID <- 205;

  #callModule(chart, "first", bins)
  
  #output$table <- DT::renderDataTable(DT::datatable({
  #   #print (moduleServer)
   # callModule(getInputParameter, "bbb", moduleServer)
    
  #}))
})
