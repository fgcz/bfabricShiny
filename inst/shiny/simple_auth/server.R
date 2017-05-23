#R
# Christian Panse <cp@fgcz.ethz.ch> 2017-05
# shiny::runApp('/Users/cp/__checkouts/R/bfabricShiny/inst/shiny/simple_auth/', display.mode = "normal", port=8080)

library(bfabricShiny)

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(proteinGroups = NULL)
                           
  bf <- callModule(bfabric, "bfabric8",  applicationid = c(168, 204))
  
  output$proteinGroupsTable <- DT::renderDataTable(DT::datatable({
    if (is.null(values$proteinGroups)){
      as.data.frame(list(output="no data - select a zip file."))
    }else{
    as.data.frame(values$proteinGroups)
      #iris
    }
  }))
  
  observeEvent(input$load, {
    if (input$ssh){
    values$proteinGroups <- bfabricShiny:::.ssh_unzip(file = 'proteinGroups.txt',
                                                      zipfile=file.path('/srv/www/htdocs', input$relativepath))
    }else{
      zipfile <- file.path('/srv/www/htdocs', input$relativepath)
      if (file.exists(zipfile)){
      values$proteinGroups <- bfabricShiny:::.unzip(file = 'proteinGroups.txt', zipfile=zipfile)
      }else{message("zip file canot be found.")}
       }
    
    message(dim(values$proteinGroups))
    message("DONE")
 })
  
  output$load <- renderUI({
      actionButton("load", "load data", icon("upload"))
  })
  
})
