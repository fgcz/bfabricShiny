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
    htmlOutput(ns("projects")),
    htmlOutput(ns("workunits")),
    htmlOutput(ns("resources"))
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
bfabric <- function(input, output, session, applicationid, resoucepattern = ".*", resourcemultiple=FALSE) {
  ns <- session$ns

  pubKey <- PKI.load.key(file = file.path(system.file("keys",
                                                      package = "bfabricShiny"), "bfabricShiny.key.pub"))


  #value <- reactiveValues(resourceid = NA)
  output$projects <- renderUI({

    projects <- getProjects(input$login, input$webservicepassword)

    if (is.null(projects)) {
    }else{
      selectInput(ns("projectid"), "container id", projects, multiple = FALSE)
    }
  })

  application <- reactive({
    fn <- system.file("extdata/application.csv", package = "bfabricShiny")

    if (file.exists(fn) &&  nchar(fn) > 0) {
     
      applications <- read.table(fn, header = TRUE)
      return(applications)

    }else{

      A <- getApplications(input$login, input$webservicepassword)
      bfabricApplication <- data.frame(id = sapply(A, function(x){x$`_id`}), name = sapply(A, function(x){x$name}))
      applications <- bfabricApplication[order(bfabricApplication$id),]
      return(applications)

    }

  })

  output$applications <- renderUI({
    applications <- application()
    # selectInput(ns("applicationid"), "input applicationid:", applicationid, multiple = FALSE)

    if (nrow(applications) > 0) {
      idx <- which(applications$id %in% applicationid) |>
        sort(decreasing = TRUE)
      xxx <- paste(applications[idx, 'id'], applications[idx, 'name'], sep = " - ") 

      tagList(
      selectInput(ns("applicationid"), "input applicationId:",
                  xxx,
                  multiple = FALSE))
    }else{NULL}
  })


  output$workunits <- renderUI({
    if (is.null(input$projectid)){
      return(NULL)
    }

    id <- strsplit(input$applicationid, " - ")[[1]][1]

    res <- getWorkunits(input$login, input$webservicepassword,
                        projectid = input$projectid,
                        applicationid = id)

    if (is.null(res)){
      return(NULL)
    }else if (length(res) == 0){
      return(NULL)
    }else{
      selectInput(ns('workunit'), 'workunit:', res, multiple = FALSE)
    }
  })

  resources <- reactive({
    df <- NULL
    if (length(input$workunit) > 0){
      res <- getResources(input$login, input$webservicepassword, workunitid = strsplit(input$workunit, " - ")[[1]][1])

      resourceid <- sapply(res, function(y){y$`_id`})
      relativepath <- sapply(res, function(y){y$relativepath})

      df <- data.frame(resourceid = resourceid, relativepath = relativepath)
      df <- df[grepl(resoucepattern, df$relativepath), ]
    }

    return (df)
  })


  output$resources <- renderUI({

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

      shinyStore::updateStore(session, "login", isolate(input$login), encrypt=pubKey)
      shinyStore::updateStore(session, "webservicepassword", isolate(input$webservicepassword), encrypt=pubKey)
      shinyStore::updateStore(session, "project", isolate(input$project), encrypt=pubKey)
    }
  }
  )

  return(list(login = reactive({input$login}),
              webservicepassword = reactive({input$webservicepassword}),
              resources = reactive({resources()}),
              workunitid = reactive({input$workunit}),
              projectid = reactive({input$projectid})))
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


