
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(jsonlite)


shinyServer(function(input, output, session) {

  
  
  
  output$project <- renderUI({
    res.project <- c(NA, 1000, 1959, 2121)
    selectInput('project', 'Project:', res.project, multiple = FALSE, selected = NA)
  })
  
  
  output$sample <- renderUI({
    if ( input$project %in% 1000:2500){
      res <- as.data.frame(fromJSON(paste("http://localhost:5000/projectid/", input$project, sep='')))
      selectInput('sample', 'Sample:', paste(res$sample.id, res$sample.name, sep='-'), multiple = TRUE)
      
    }else{
      selectInput('sample', 'Sample:', NULL)
    }
  })
  
  
  getExtracts <- reactive({
    sample.id <- sapply(strsplit(input$sample, split='-'), function(x){x[1]})
    res <- do.call('rbind', 
                   lapply(sample.id, 
                          function(sampleid){
                            as.data.frame(fromJSON(paste("http://localhost:5000/sampleid/", sampleid, sep='')))
                          }))
  })
  
  output$extract <- renderUI({

    if (input$sample == "" || length(input$sample) == 0 || is.null(input$sample)){
      selectInput('extract', 'Extract:', NULL)
    }   else{
      
      res <- getExtracts()
      selectInput('extract', 'Extract:', res$extract.name, multiple = TRUE)  
       
    }
  })
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
  
    
    
    # call Christian Trachsel's code here!
 
   
    
    print (input$extract)
    if (input$extract != "" && length(input$extract)>0){
      res <- getExtracts()
      idx <- res$extract.name %in% input$extract
      res[idx, ]
    }else{
      #as.data.frame(list(extract.name=NA, sampleid=NA, extract.id=NA))
      as.data.frame(list(output="no data yet"))
    }
    
    }))

})
