#R
# shiny::runApp('/Users/cp/__checkouts/R/bfabricShiny/inst/shiny/simple_auth/', display.mode = "normal", port=8080)

library(bfabricShiny)

my <- function(input, output, session, bf) {
  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  bf <- callModule(bfabric, "bfabric8",  applicationid = c(168, 204))
  #callModule(my, "My8", input)
  
  output$distPlot <- renderPlot({
    print (bf)
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # print(input$resourceid)
    # draw the histogram with the specified number of bins
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main='jjjj')
  })
  #print (dd)
  #getApplicationID <- 205;

  #callModule(chart, "first", bins)
  
  #output$table <- DT::renderDataTable(DT::datatable({
  #   #print (moduleServer)
   # callModule(getInputParameter, "bbb", moduleServer)
    
  #}))
})
