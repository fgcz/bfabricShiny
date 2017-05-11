#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(bfabricShiny)



columnChooser <- function(input, output, session, login, webservicepassword) {
  output$pprojectl <- renderUI({
    ns <- session$ns
    
    print (paste("DEBUG", login, webservicepassword))
    print (paste("DEBUG", input$login, input$webservicepassword))
    print (paste("DEBUG", ns("login"), ns("webservicepassword")))

    #projects <- getProjects(input$login, input$webservicepassword)
    
    # (projects)
    projects <- c(1000, 2000)
    
    selectInput(ns("pprojectl"), "xxxProjetcs", projects, multiple = FALSE)
  })
  
  return(reactive({
    validate(need(input$pprojectl, FALSE))
    projects
  }))
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dd <- callModule(columnChooser, "bfabric8") 
 #                  login=reactive({input$login}), 
  #                 webservicepassword=reactive({input$webservicepassword}))
  
  print (dd)
  getApplicationID <- 205;

  #getResources <- callModule(getResources, id="bfabric8")
  #etProjects <- callModule(getProjects,  id="bfabric8")
  
  
  output$resources <- renderUI({
    res <- getResources(input$login, input$webservicepassword, input$project)
    if (is.null(res)){
      # selectInput('project', 'project:', NULL)
      # textInput('not authorized yet', 'not authorized yet')
    }else{
      
      selectInput('resource', 'resource:', res, multiple = FALSE)
    }
  })
  
  
  output$project <- renderUI({
    res <- getProjects(input$login, input$webservicepassword)
    
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
