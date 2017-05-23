#R

#' Shiny UI module
#'
#' @param id 
#'
#'@seealso \url{http://fgcz-bfabric.uzh.ch}
#'@references \url{https://doi.org/10.1145/1739041.1739135}
#' @return
#' @export bfabricInput
bfabricInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  system.file("keys", package = "bfabricShiny")
  privKey <- PKI.load.key(file=file.path(system.file("keys", package = "bfabricShiny"), "bfabricShiny.key"))
  
  tagList(
    initStore(ns("store"), "shinyStore-ex2", privKey), 
    textInput(ns('login'), 'bfabric Login'),
    passwordInput(ns('webservicepassword'), 'Web Service Password'),
    htmlOutput(ns("applications")),
    actionButton(ns("save"), "Save password", icon("save")),
    htmlOutput(ns("projects")),
    htmlOutput(ns("workunits")),
    htmlOutput(ns("resources")),
   # tags$head(tags$script(src = "message-handler.js")),
    htmlOutput(ns("load"))
  )
}


#' shiny server module for the bfabric
#'
#' @param input 
#' @param output 
#' @param session 
#' @description provides a shiny server module for the bfabric system.
#' It is assumes that the \code{exec/flask_bfabric_sample.py} programm is running.
#' 
#' \code{ssh-keygen -f $PWD/bfabricShiny.key -t rsa} will generate 
#' the key files.
#'@author Christian Panse <cp@fgcz.ethz.ch> 2017
#'@seealso \url{http://fgcz-bfabric.uzh.ch}
#'@references \url{https://doi.org/10.1145/1739041.1739135}
#' @return a list of resources
#' @export bfabric
bfabric <- function(input, output, session, applicationid) {
  ns <- session$ns
  
  pubKey <- PKI.load.key(file=file.path(system.file("keys", 
                                                    package = "bfabricShiny"), "bfabricShiny.key.pub"))
  
  output$projects <- renderUI({
    
    projects <- getProjects(input$login, input$webservicepassword)
    
    if (is.null(projects)){
    }else{
      selectInput(ns("projectid"), "projectid", projects, multiple = FALSE)
    }
  })
  
  output$applications <- renderUI({
    selectInput(ns("applicationid"), "applicationid:", applicationid, multiple = FALSE)
  })
  
  output$workunits <- renderUI({
    if (is.null(input$projectid)){
      return(NULL)
    }
    
    res <- getWorkunits(input$login, input$webservicepassword, 
                        projectid = input$projectid, 
                        applicationid = applicationid)

    if (is.null(res)){
      return (NULL)
    }else if (length(res) == 0){
      return (NULL)
    }else{
        selectInput(ns('workunit'), 'workunit:', res, multiple = FALSE)
    }
  })
  
  resources <- reactive({
    rv <- NULL
    if (length(input$workunit) > 0){
      rv <- getResources(input$login, input$webservicepassword, workunitid = strsplit(input$workunit, " - ")[[1]][1])
    }
    
    rv
  })
  
  output$resources <- renderUI({
    
    if (length(input$workunit) == 0){
      print ("no resources")
    }else{
      workunitid = strsplit(input$workunit, " - ")[[1]][1]
    
      res <- resources()
      
      if (is.null(res)){
      }else{
        selectInput(ns("resourceid"), "resourceid:", res, multiple = FALSE)
      }
      
    }
  })
  
  output$load <- renderUI({
    res <- resources()
    
    if (length(res) > 0){
      actionButton(ns("load"), "load", icon("upload"))
    }else{}
    })
  
  observe({
    if (input$save <= 0){
      # On initialization, set the value of the text editor to the current val.
      updateTextInput(session, "login", value=isolate(input$store)$login)
      updateTextInput(session, "webservicepassword", value=isolate(input$store)$webservicepassword)
      return()
    }
    updateStore(session, "login", isolate(input$login), encrypt=pubKey)
    updateStore(session, "webservicepassword", isolate(input$webservicepassword), encrypt=pubKey)
  })
  
  observeEvent(input$load, {
    
    print ("HELLO!")
    print(input$resourceid)
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })

  eventReactive(input$load, {
    return(list(input$resourceid))
  })
  #return(input)
}
