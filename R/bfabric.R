#' Defines the bfabric shiny UI module
#'
#' @param id shiny session id
#' @import shiny
#' @importFrom PKI PKI.load.key PKI.encrypt PKI.decrypt
#' @importFrom shinyStore updateStore initStore
#' @seealso \url{http://fgcz-bfabric.uzh.ch}
#' @references \url{https://doi.org/10.1145/1739041.1739135}
#' @return tagList
#' @export bfabricInput
#' @examples
#' \dontrun{
#'     mainPanel(
#'       tabsetPanel(
#'         tabPanel("bfabric", bfabricInput("bfabric8")),
#'         tabPanel("plot", plotOutput("distPlot"))
#'    )
#' }
bfabricInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  privKey <- PKI::PKI.load.key(file = file.path(system.file("keys",
    package = "bfabricShiny"), "bfabricShiny.key"))


  tagList(
    initStore(ns("store"), "shinyStore-ex2", privKey),
    a(img(src="https://img.shields.io/badge/JIB-10.1515%2Fjib.2022.0031-brightgreen"),
      href='https://www.degruyter.com/document/doi/10.1515/jib-2022-0031/html'),
    br(),
    textInput(ns('login'), 'B-Fabric Login', placeholder = "as you login on https://fgcz-bfabric.uzh.ch"),
    passwordInput(ns('webservicepassword'), 'Web Service Password',
                  placeholder = "in B-Fabric on 'User Details' (upper-right corner)."),
    htmlOutput(ns("applications")),
    actionButton(ns("saveBfabricPassword"), "Encrypt & Save Password", icon("save")),
    br(),
    br(),
    htmlOutput(ns("employee")),
    htmlOutput(ns("containers")),
    htmlOutput(ns("workunits")),
    htmlOutput(ns("resources")),
    htmlOutput(ns("systemInformation"))
  )
}

#' shiny server module for the bfabric
#'
#' @param input default module input
#' @param output default module output
#' @param session module session
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
#' @importFrom utils read.table
#' @import PKI
#' @export bfabric
bfabric <- function(input, output, session,
                    applicationid = NULL,
                    resoucepattern = ".*",
                    resourcemultiple=FALSE) {
  ns <- session$ns

  bfabricValues <- reactiveValues()
  bfabricValues$errorreport <- NULL
  
  # ======Rprofile=====
  Rprofile <- reactive({
    f <- file.path(Sys.getenv("HOME"), ".Rprofile") 
    if (file.exists(f)){ return (f) }
    else{stop(paste0("File not found ", f))}
  })

  posturl <- reactive({
    source(Rprofile(), local=TRUE)
    message(paste0("read bfabricposturl ", bfabricposturl, "."))
    return (bfabricposturl)
  })

  pubKey <- PKI.load.key(file = file.path(system.file("keys",
    package = "bfabricShiny"), "bfabricShiny.key.pub"))

  output$employee <- renderUI({
    message("output$employee <- renderUI({")
    if (nchar(input$login) == 0 || nchar(input$webservicepassword) == 0){return(NULL)}
   
    if (empdegree()){
      HTML("As an employee, you have access to all containers.")
    }
  })


  #=======output$containers======
  output$containers <- renderUI({
     
    if(bfabricConnectionWorking()){
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "querying containers ...")
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL, n = NULL) {
        progress$set(detail = detail)
      }
      if (empdegree()){
        numericInput(ns("containerid"), "Order | project | container id:",
                     value = 3000,
                     min = 1000,
                     max = 35000)
      }else{
        containers <- bfabricShiny:::.getContainers(input$login,
                                                    input$webservicepassword,
                                                    posturl = posturl(),
                                                    updateProgress = updateProgress)
        
        if ('errorreport' %in% names(containers)) {
          HTML(paste0("<b>container:</b> ", containers$errorreport))
        }else{
          selectInput(ns("containerid"), "Order | project | container id:",
                      containers, multiple = FALSE)
        }
      }
    }})

  #=======output$workunits======
  output$workunits <- renderUI({
    if(bfabricConnectionWorking()){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "querying workunits ...")
      on.exit(progress$close())
      
      if (length(input$containerid) == 0){
        print("no workunits yet.")
      }else{
        if (is.null(input$containerid)){
          return(NULL)
        }
        id <- strsplit(input$applicationid, " - ")[[1]][1]
        
        updateProgress <- function(value = NULL, detail = NULL, n = NULL) {
          progress$set(detail = detail)
        }
        
        workunits <- .getWorkunits(input$login,
                                   input$webservicepassword,
                                   containerid = input$containerid,
                                   posturl = posturl(),
                                   applicationid = id, updateProgress)
        
        if ('errorreport' %in% names(workunits)) {
          HTML(paste0("<b>workunit:</b> ", workunits$errorreport))
        }
        else if (is.null(workunits)){
          HTML(paste0("No workunits; choose a project."))
        }else if (length(workunits) == 0){
          return(NULL)
        }else{
          selectInput(ns('workunit'), 'workunit:', workunits, multiple = FALSE)
        }}
    }})
  
  #=======output$resourcess======
  output$resources <- renderUI({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "querying resources ...")
    on.exit(progress$close())
    if (length(input$workunit) == 0){
      print("no resources yet.")
    }else{
      workunitid = strsplit(input$workunit, " - ")[[1]][1]
      res <- resources()
      
      if (is.null(res)){
        HTML("no resources found.")
      }else{
        tagList(
          selectInput(inputId = "relativepath",
                      label = "resource relativepath:",
                      choices = res$relativepath,
                      multiple = resourcemultiple),
          actionButton("load", "load selected data resource", icon("upload")),
          tags$style(    type = 'text/css',
                         ".selectize-input { word-wrap : break-word;}
                          .selectize-input { word-break: break-word;}
                          .selectize-dropdown {word-wrap : break-word;} "
          )
        )
      }
    }
  })
  
  #=======output$applications======
  output$applications <- renderUI({
    if(bfabricConnectionWorking()){
      applications <- application()
      
      if (nrow(applications) > 0) {
        idx <- which(applications$id %in% applicationid) |>
          sort(decreasing = TRUE)
        xxx <- paste(applications[idx, 'id'], applications[idx, 'name'],
                     sep = " - ")
        
        tagList(
          selectInput(ns("applicationid"), "input applicationId:",
                      xxx,
                      multiple = FALSE))
      }else{NULL}
    }})
  
  #=======output$systemInformation======
  output$systemInformation <- renderUI({
    if (input$login %in% c('cpanse', 'mderrico', 'wolski')){
      HTML(paste("<hr>system information",
                 "<ul>",
                 "<li>Rprofile:", Rprofile(), "</li>",
                 "<li>posturl:", posturl(), "</li>",
                 "<li>auth:", bfabricConnectionWorking(), "</li>",
                 "<li>errorreport:", bfabricValues$errorreport, "</li>",
                 "<li>R.version.string:", R.version.string, "</li>",
                 "<li>bfabricShiny:", packageVersion('bfabricShiny'), "</li>",
                 "</ul>",
                 "<hr>"))
    }
  })
  
  #=======bfabricConnectionWorking======
  bfabricConnectionWorking <- eventReactive(
    list(input$login,
         input$webservicepassword),
    {
      rv = bfabricShiny::readPages(input$login,
                                   input$webservicepassword ,
                                   posturl = posturl(),
                                   endpoint = 'user',
                                   query = list(login=login))
      
      if ("errorreport" %in% names(rv)){
        bfabricValues$errorreport <- rv$errorreport
      }
      else if ("status" %in% names(rv)){
        bfabricValues$errorreport <- rv$status
      }
      else{
        bfabricValues$errorreport <- NULL
      }
      
      message(paste0("bfabricConnectionWorking ", (isFALSE("errorreport" %in% names(rv)) && isFALSE("status" %in% names(rv)))))
      (isFALSE("errorreport" %in% names(rv)) && isFALSE("status" %in% names(rv)))
    })
  
  
  empdegree <- reactive({
    if(isFALSE(bfabricConnectionWorking())){return(FALSE)}
    
    user <- bfabricShiny::readPages(input$login,
                                  input$webservicepassword,
                                  posturl  = posturl(),
                                  endpoint = 'user',
                                  query = list(login = input$login))
    
    if('empdegree' %in% names(user[[1]])){
      message("'empdegree' found.")
      return(TRUE)
    }
    return (FALSE)
  })

  application <- reactive({
    if(bfabricConnectionWorking()){
      fn <- system.file("extdata/application.csv", package = "bfabricShiny")
      
      if (file.exists(fn) &&  nchar(fn) > 0) {
        
        applications <- read.table(fn, header = TRUE)
        return(applications)
        
      }else{
        A <- .getApplications(input$login, input$webservicepassword,
                              posturl = posturl())
        bfabricApplication <- data.frame(id = sapply(A, function(x){x$`_id`}), name = sapply(A, function(x){x$name}))
        applications <- bfabricApplication[order(bfabricApplication$id),]
        return(applications)
        
      }
    }
  })

  resources <- reactive({
    if(bfabricConnectionWorking()){
      df <- NULL
      if (length(input$workunit) > 0){
        res <- .getResources(input$login,
                             input$webservicepassword,
                             posturl = posturl(),
                             workunitid = strsplit(input$workunit, " - ")[[1]][1])
        
        resourceid <- sapply(res, function(y){y$`_id`})
        relativepath <- sapply(res, function(y){y$relativepath})
        
        df <- data.frame(resourceid = resourceid, relativepath = relativepath)
        df <- df[grepl(resoucepattern, df$relativepath), ]
      }
      
      return (df)
    }
  })




  ## shinyStore; for login and password handling
  observe({

    #message("OBSERVE bfabricSHINY")

    if ('login' %in% names(input)){
      if (input$saveBfabricPassword <= 0){
        # On initialization, set the value of the text editor to the current val.
        message("saving 'login and webservicepassword' ...")
        updateTextInput(session, "login", value=isolate(input$store)$login)
        updateTextInput(session, "webservicepassword", value=isolate(input$store)$webservicepassword)
        updateTextInput(session, "project", value=isolate(input$project)$login)
        return()
      }

      #stopifnot(require(PKI))
      shinyStore::updateStore(session, "login", isolate(input$login), encrypt=pubKey)
      shinyStore::updateStore(session, "webservicepassword", isolate(input$webservicepassword), encrypt=pubKey)
      shinyStore::updateStore(session, "project", isolate(input$project), encrypt=pubKey)
    }
  }
  )
  # -------bfabric module return--------
  return(list(login = reactive({input$login}),
              webservicepassword = reactive({input$webservicepassword}),
              resources = reactive({resources()}),
              workunitid = reactive({input$workunit}),
              posturl = reactive({posturl()}),
              containerid = reactive({input$containerid})))
}

#' @export bfabricInputLogin
bfabricInputLogin <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  privKey <- PKI::PKI.load.key(file = file.path(system.file("keys",
      package = "bfabricShiny"), "bfabricShiny.key"))
  
  tagList(
    initStore(ns("store"), "shinyStore-ex2", privKey),
    textInput(ns('login'), 'bfabric Login'),
    passwordInput(ns('webservicepassword'), 'Web Service Password',
                  placeholder = "in bfabric on 'User Details'."),
    htmlOutput(ns("applications")),
    actionButton(ns("saveBfabricPassword"), "Encrypt & Save Password", icon("save"))
  )
}

#' @import PKI
#' @export bfabricLogin
bfabricLogin <- function(input, output, session) {
  ns <- session$ns
  
  pubKey <- PKI::PKI.load.key(file = file.path(system.file("keys",
                                                      package = "bfabricShiny"),
                                          "bfabricShiny.key.pub"))
  
  ## shinyStore; for login and password handling
  observe({
    if ('login' %in% names(input)){
      if (input$saveBfabricPassword <= 0){
        # On initialization, set the value of the text editor to the current val.
        message("saving 'login and webservicepassword' ...")
        updateTextInput(session, "login", value=isolate(input$store)$login)
        updateTextInput(session, "webservicepassword",
                        value=isolate(input$store)$webservicepassword)
        return()
      }
      #stopifnot(require(PKI))
      shinyStore::updateStore(session, "login", isolate(input$login),
                              encrypt=pubKey)
      shinyStore::updateStore(session, "webservicepassword",
                              isolate(input$webservicepassword), encrypt=pubKey)
    }
  }
  )
  
  return(list(login = reactive({input$login}),
              webservicepassword = reactive({input$webservicepassword})))
}


