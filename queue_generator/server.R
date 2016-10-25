
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
    selectInput('project', 'Project:', res.project, multiple = FALSE, selected = 1000)
  })
  
  
  output$sample <- renderUI({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/projectid/", input$project, sep='')))
    selectInput('sample', 'Sample:', paste(res$sample.id, res$sample.name, sep='-'), multiple = TRUE)
  })
  
  output$extract <- renderUI({

    if (length(input$sample) > 0){
      sample.id <- sapply(strsplit(input$sample, split='-'), function(x){x[1]})
      
      
      res <- do.call('rbind', 
                     lapply(sample.id, 
                            function(sampleid){
                              as.data.frame(fromJSON(paste("http://localhost:5000/sampleid/", sampleid, sep='')))
                              }))
      

        
      selectInput('extract', 'Extract:', res$extract.name, multiple = TRUE)
    }   else{
     selectInput('extract', 'Extract:', "NA")
    }
  })
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    # call Christian Trachsel's code here!
    data <- iris
    
    data
  }))

})
