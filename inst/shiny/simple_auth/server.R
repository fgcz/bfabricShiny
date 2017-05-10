#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(bfabricShiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
  getApplicationID <- 205;
  
  getResources <- reactive({
    rv <- POST('http://localhost:5000/query', 
               body = toJSON(list(login = input$login, 
                                  webservicepassword = input$webservicepassword,
                                  query = 'resource',
                                  projectid = input$project,
                                  applicationid = 205)), 
               encode = 'json')
    rv <- content(rv)
    sort(unlist(rv$workunits), decreasing = TRUE)
  })
  
  
  getProjects <- reactive({
    rv <- POST('http://localhost:5000/query', 
               body=toJSON(list(login=input$login, 
                                webservicepassword=input$webservicepassword,
                                query='project')), 
               encode = 'json')
    
    rv <- content(rv)
    sort(unlist(rv$project), decreasing = TRUE)
  })
  # 
  
  output$resources <- renderUI({
    res <- getResources()
    if (is.null(res)){
      # selectInput('project', 'project:', NULL)
      # textInput('not authorized yet', 'not authorized yet')
    }else{
      
      selectInput('resource', 'resource:', res, multiple = FALSE)
    }
  })
  
  
  output$project <- renderUI({
    res <- getProjects()
    if (is.null(res)){
    }else{
      selectInput('project', 'project:', res, multiple = FALSE)
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
