.privateKeyFile <- function(){
  f <- file.path(system.file("keys", package = "bfabricShiny"), "bfabricShiny.key")
  f
}

.publicKeyFile <- function(){
  f <- file.path(system.file("keys", package = "bfabricShiny"), "bfabricShiny.key.pub.pem")
  f
}
#' Defines the bfabric shiny UI module
#'
#' @param id shiny session id
#' 
#' @import shiny
#' @importFrom PKI PKI.load.key PKI.encrypt PKI.decrypt
#' @importFrom shinyStore updateStore initStore
#' @seealso \link{bfabric}
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

  privKey <- PKI::PKI.load.key(file = .privateKeyFile())
  
  tagList(
    shiny::conditionalPanel(
      condition = isFALSE(all(c("login", "webservicepassword") %in% Sys.getenv())),
      initStore(ns("store"), "shinyStore-ex2", privKey),
    ),
    a(img(src="https://img.shields.io/badge/JIB-10.1515%2Fjib.2022.0031-brightgreen"),
      href='https://www.degruyter.com/document/doi/10.1515/jib-2022-0031/html'),
    br(),
    htmlOutput(ns("loginInformation")),
    textInput(ns('login'), 'B-Fabric Login',
              placeholder = "as you login on https://fgcz-bfabric.uzh.ch",
              value = Sys.getenv('login')),
    passwordInput(ns('webservicepassword'),
                  label = 'Web Service Password',
                  placeholder = "in B-Fabric on 'User Details' (upper-right corner).", 
                  value = Sys.getenv('webservicepassword')),
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

#' Shiny server module for the bfabric using bfabricPy
#'
#' @param input default module input
#' @param output default module output
#' @param session module session
#' @description provides a shiny server module for the bfabric system.
#' It is assumes that the \code{bfabric_flask.py} program is running, e.g.,
#' \code{BFABRICPY_CONFIG_ENV=FGCZSTABLE bfabric_flask.py}
#' 
#'
#' @details ensure file staging via ssh if not NFS is available
#' \code{ssh-keygen -f $PWD/bfabricShiny.key -t rsa} will generate
#' the key files.
#' 
#' \enumerate{
#' 
#' \item add \code{bf <- callModule(bfabric, "bfabric8", applicationid = c(155))}
#' 
#' \item follow the instructions \code{\link{bfabricInput}}
#' 
#' }
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch> 2017, 2025
#' @seealso \itemize{
#' \item \url{https://github.com/fgcz/bfabricPy}
#' }
#'
#' @references \url{https://www.degruyterbrill.com/document/doi/10.1515/jib-2022-0031/html}
#' @return check the \code{input$resourceid} value.
#' @importFrom utils read.table
#' @import PKI
#' @export bfabric
bfabric <- function(input, output, session,
                    applicationid = NULL,
                    resoucepattern = ".*",
                    resourcemultiple = FALSE,
                    token = NULL) {
  ns <- session$ns
  
  getToken <- reactive({ 
    getQueryString() -> qs
    if ('token' %in% names(qs)){
      return(.validateToken(qs$token))
    }
    return(NULL)
  })

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
    
    (paste0("read bfabricposturl ", bfabricposturl, ".")) -> msg
    message(msg)
    shiny::showNotification(msg, duration = 4, type = "message")
    
    return (bfabricposturl)
  })

  pubKey <- PKI.load.key(file = .publicKeyFile())

  output$employee <- renderUI({
    message("output$employee <- renderUI({")
    if (nchar(input$login) == 0 || nchar(input$webservicepassword) == 0){return(NULL)}
   
    if (empdegree()){
      HTML("As an employee, you have access to all containers.")
    }
  })

  output$loginInformation <- renderUI({
    if (is.null(getToken())){
      HTML("use regular login;")
    }else{
      HTML("we have token. have a lot of fun!")
    }
  })
  

  #=======output$containers======
  output$containers <- renderUI({
    shiny::req(bfabricConnectionWorking() == TRUE)
    
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
          shiny::showNotification(containers$errorreport, duration = 15, type = "error")
          HTML(paste0("<b>container:</b> ", containers$errorreport))
        }else{
          selectInput(ns("containerid"), "Order | project | container id:",
                      containers, multiple = FALSE)
        }
      }
    }})

  #=======output$workunits======
  output$workunits <- renderUI({
    
    if(shiny::req(bfabricConnectionWorking() == TRUE)){
      
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
                                   applicationid = id,
                                   updateProgress)
        
        if ('error' %in% names(workunits)) {
          shiny::showNotification(workunits$error, duration = 15, type = "error")
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
    shiny::req(bfabricConnectionWorking() == TRUE)
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
    shiny::req(bfabricConnectionWorking() == TRUE)
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
      shiny::req(input$login, input$webservicepassword)
      rv = bfabricShiny::readPages(input$login,
                                   input$webservicepassword ,
                                   posturl = posturl(),
                                   endpoint = 'user',
                                   query = list(login=input$login))
      
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
                                  query = list(login = input$login))$res
    
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
        bfabricApplication <- data.frame(id = sapply(A, function(x){x$id}), name = sapply(A, function(x){x$name}))
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
        
        resourceid <- sapply(res, function(y){y$id})
        relativepath <- sapply(res, function(y){y$relativepath})
        
        df <- data.frame(resourceid = resourceid, relativepath = relativepath)
        df <- df[grepl(resoucepattern, df$relativepath), ]
      }
      
      return (df)
    }
  })

  # -------- LOGIN and WEBSERVICEPASSWORD --------------
  ## login and password handling
  observe({
    getToken() -> token
    print(token)
    # check if token exists and extract login and webservice password from there
    # 
    if (is.null(token)){
      
      #shiny::showNotification(paste0("token is NULL."),
      #                        type = 'error',
      #                        duration = 2)
      
    }else{ 
      shiny::showNotification(paste0("login as ", token$user, "."),
                              type = 'message')
      
      
      updateTextInput(session, "login", value = token$user)
      updateTextInput(session, "webservicepassword", value = token$userWsPassword)
      
      return()
    }
    
    
    if ('login' %in% names(input)){
      if (input$saveBfabricPassword <= 0){
        # On initialization, set the value of the text editor to the current val.
        message("saving 'login and webservicepassword' ...")
        updateTextInput(session, "login", value = isolate(input$store)$login)
        updateTextInput(session, "webservicepassword", value = isolate(input$store)$webservicepassword)
        updateTextInput(session, "project", value = isolate(input$project)$login)
        return()
      }
      
      # no valid token 
      if (isFALSE(is.null(token))){
        # Fetches login and webservicepassword from shinyStore
        shinyStore::updateStore(session, "login", isolate(input$login), encrypt=pubKey)
        shinyStore::updateStore(session, "webservicepassword", isolate(input$webservicepassword), encrypt=pubKey)
        shinyStore::updateStore(session, "project", isolate(input$project), encrypt=pubKey)
      }
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

#' bfabricInputLogin 
#' @export bfabricInputLogin
#'
#' @return a \code{tagList} containing login and webservicepassword
bfabricInputLogin <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  privKey <- PKI::PKI.load.key(file = .privateKeyFile())
  
  tagList(
    initStore(ns("store"), "shinyStore-ex2", privKey),
    textInput(ns('login'), 'bfabric Login'),
    passwordInput(ns('webservicepassword'), 'Web Service Password',
                  placeholder = "in bfabric on 'User Details'."),
    htmlOutput(ns("applications")),
    actionButton(ns("saveBfabricPassword"), "Encrypt & Save Password", icon("save"))
  )
}

#' @inheritParams bfabric
#' @import PKI
#' @export bfabricLogin
bfabricLogin <- function(input, output, session) {
  ns <- session$ns
  
  pubKey <- PKI::PKI.load.key(file = .publicKeyFile())
  
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


