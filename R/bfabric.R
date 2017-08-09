#R

#' Shiny UI module
#'
#' @param id 
#'
#' @seealso \url{http://fgcz-bfabric.uzh.ch}
#' @references \url{https://doi.org/10.1145/1739041.1739135}
#' @return
#' @export bfabricInput
#' @examples 
#' \dontrun{
#' 
#'     mainPanel(
#'       tabsetPanel(
#'         tabPanel("bfabric", bfabricInput("bfabric8")),
#'         tabPanel("plot", plotOutput("distPlot"))
#'    )
#' 
#' }
bfabricInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  system.file("keys", package = "bfabricShiny")
  privKey <- PKI.load.key(file=file.path(system.file("keys", package = "bfabricShiny"), "bfabricShiny.key"))
  
  tagList(
    initStore(ns("store"), "shinyStore-ex2", privKey), 
    textInput(ns('login'), 'bfabric Login'),
    passwordInput(ns('webservicepassword'), 'Web Service Password', 
                  placeholder = "in bfabric on 'User Details'."),
    htmlOutput(ns("applications")),
    actionButton(ns("save"), "Save password", icon("save")),
    htmlOutput(ns("projects")),
    htmlOutput(ns("workunits")),
    htmlOutput(ns("resources"))
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
#' 
#' @details t.b.d. 
#' \enumerate{
#' \item add \code{bf <- callModule(bfabric, "bfabric8", applicationid = c(155))} 
#' \item follow the instructions \code{\link{bfabricInput}}
#' }
#' @author Christian Panse <cp@fgcz.ethz.ch> 2017
#' @seealso \itemize{
#' \item \url{http://fgcz-bfabric.uzh.ch}
#' \item \url{http://fgcz-svn.uzh.ch/repos/scripts/trunk/linux/bfabric/apps/python}
#' }
#' @references \url{https://doi.org/10.1145/1739041.1739135}
#' @return check the \code{input$resourceid} value.
#' @export bfabric
bfabric <- function(input, output, session, applicationid, resoucepattern = ".*") {
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
                        applicationid = input$applicationid)

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
      print ("no resources yet.")
    }else{
      workunitid = strsplit(input$workunit, " - ")[[1]][1]
      res <- resources()
      if (is.null(res)){
      }else{
        resourceid <- sapply(res, 
                                  function(y){paste(y$`_id`, y$name, sep=" - ")})
        
        relativepath <- sapply(res, 
                             function(y){y$relativepath})
        
        relativepath <- relativepath[grep(resoucepattern, relativepath )]
        
        tagList(
        #selectInput("resourceid", "resourceid:", resourceid, 
        #            multiple = FALSE),
        selectInput("relativepath", "resource relativepath:", relativepath,
                    multiple = FALSE),
        actionButton("load", "load selected RData", icon("upload"))
        )
      }
    }
  })
  
  

  ## shinyStore; for login and password handling
  observe({
    if (input$save <= 0){
      # On initialization, set the value of the text editor to the current val.
      updateTextInput(session, "login", value=isolate(input$store)$login)
      updateTextInput(session, "webservicepassword", value=isolate(input$store)$webservicepassword)
      updateTextInput(session, "project", value=isolate(input$project)$login)
      return()
    }
    updateStore(session, "login", isolate(input$login), encrypt=pubKey)
    updateStore(session, "webservicepassword", isolate(input$webservicepassword), encrypt=pubKey)
    updateStore(session, "project", isolate(input$project), encrypt=pubKey)
  })

  return(list(login = reactive({input$login}), 
              webservicepassword = reactive({input$webservicepassword}),
              workunitid = reactive({input$workunit}),
              projectid = reactive({input$projectid})))
}

