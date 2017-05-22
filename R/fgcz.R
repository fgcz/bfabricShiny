#R
# Christian Panse <cp@fgcz.ethz.ch>
# 2017-05-06
.workunit2resource <- function(workunit_id = 153887){
  
  query_url <- paste("http://localhost:5000/zip_resource_of_workunitid/", workunit_id, sep='')

  res <- as.data.frame(fromJSON(query_url))

  return(res)

}

.unzip <- function(zipfile=NULL, file=NULL){
  cmd <- paste('unzip -p ', zipfile, file)
  content <- read.csv(pipe(cmd), sep='\t', stringsAsFactors = FALSE, header = TRUE)
  content
}

.ssh_unzip <- function(host = 'fgcz-r-021.uzh.ch', user = 'cpanse', zipfile = .workunit2resource(), file = 'proteinGroups.txt'){
  
  cmd <- paste('unzip -p ', zipfile, file)
  ssh_cmd <- paste("ssh ", user, "@", host, " '", cmd, "'", sep="")
  message(ssh_cmd)

  S <- read.csv(pipe(ssh_cmd), sep='\t', stringsAsFactors = FALSE, header = TRUE)

  S
}

.getMaxQuantFilesNames <- function(S){
  gsub("Intensity\\.", "", grep("Intensity\\.",colnames(S),value=T) )
}


.query_example0 <- function(){
  login = ''
  webservicepassword = ""
  url0 <- 'http://localhost:5000/q'
  url <-  'http://localhost:5000/custom'
  rv <- POST(url0, 
             body=toJSON(list(login = login, 
                              webservicepassword = webservicepassword,
                              endpoint = 'workunit', 
                              query=list('applicationid' = 168, 
                                         "projectid" = 1000))), 
             encode = 'json')
  
  rv <- content(rv)
}
  

.query_example1 <- function(){
  login = ''
  webservicepassword = ""
  url0 <- 'http://localhost:5000/q'
  url <-  'http://localhost:5000/custom'
  rv <- GET('http://localhost:5000/custom')
  
  rv <- content(rv)
  
}



#' queries projects of a login
#'
#' @param login 
#' @param webservicepassword 
#'
#' @return a vector of project ids
#' @export getProjects
#'
#' 
getProjects <- function(login, webservicepassword) {

  projetcs <- ({
    rv <- POST('http://localhost:5000/q', 
               body=toJSON(list(login=login, 
                                webservicepassword=webservicepassword,
                                endpoint = 'user',
                                query=list('login'= login))), 
               encode = 'json')
    
    rv <- content(rv)
    rv <- sapply(rv$res[[1]]$project, function(y){y$`_id`})
    sort(unlist(rv), decreasing = TRUE)
  })
  
  
  projetcs
}

getWorkunits <- function(login, webservicepassword, projectid = NULL, applicationid = 168){
  
  workunits <- ({
    rv <- POST('http://localhost:5000/q', 
               body = toJSON(list(login = login, 
                                  webservicepassword = webservicepassword,
                                  endpoint = 'workunit', 
                                  query=list('applicationid' = applicationid, 
                                             'projectid' = projectid)
               ), 
               encode = 'json'))
    
    rv <- content(rv)
    rv <- sapply(rv$res, function(y){paste(y$`_id`, y$name, sep=" - ")})
    
    if (length(rv) > 0){
      rv <- sort(rv, decreasing = TRUE)
    }
    
    rv
  })
  
  return(workunits)
}

#' get all resources of a (login, project) 
#'
#' @param login 
#' @param webservicepassword 
#' @param project 
#'
#' @return a vector of resource ids
#' @export getResources 
getResources <- function(login, webservicepassword, workunitid){
  if (is.null(workunitid)){
    return(NULL)
  }
  resources <- ({
    rv <- POST('http://localhost:5000/q', 
               body = toJSON(list(login = login, 
                                  webservicepassword = webservicepassword,
                                  endpoint = 'resource', 
                                  query=list('workunitid' = workunitid)
                                 ), 
               encode = 'json'))
    
    rv <- content(rv)
    sort(sapply(rv$res, function(y){paste(y$`_id`, y$name, sep=" - ")}), decreasing = TRUE)
  })
  
  return(resources)
}


#' Module UI function
#'
#' @param id 
#'
#' @return
#' @export shinyUIModule
shinyUIModule <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  system.file("keys", package = "bfabricShiny")
  privKey <- PKI.load.key(file=file.path(system.file("keys", package = "bfabricShiny"), "bfabricShiny.key"))
 
  tagList(
    initStore(ns("store"), "shinyStore-ex2", privKey), 
    textInput(ns('login'), 'bfabric Login'),
    passwordInput(ns('webservicepassword'), 'Web Service Password'),
    actionButton(ns("save"), "Save password", icon("save")),
    htmlOutput(ns("projects")),
    htmlOutput(ns("workunits")),
    htmlOutput(ns("resources"))
  )
}


#' shinyServerModule
#'
#' @param input 
#' @param output 
#' @param session 
#' @description \code{ssh-keygen -f $PWD/bfabricShiny.key -t rsa} will generate 
#' the key files
#'
#' @return
#' @export shinyServerModule
shinyServerModule <- function(input, output, session, applicationid) {
  
  ns <- session$ns
  
  #values <- reactiveValues(login = input$login, webservicepassword = input$webservicepassword)
  
  pubKey <- PKI.load.key(file=file.path(system.file("keys", package = "bfabricShiny"), "bfabricShiny.key.pub"))
  print("shinyServer")
  print(pubKey)
  
  output$projects <- renderUI({
    
    projects <- getProjects(input$login, input$webservicepassword)
    
    if (is.null(projects)){
    }else{
      
      selectInput(ns("projectid"), "projectid", projects, multiple = FALSE)
    }
  })
  
  output$workunits <- renderUI({
  
    print("BEGIN DEBUG WORKUNIT")
    print(input$projectid)
    print("END DEBUG")
    
    if (is.null(input$projectid)){
      return(NULL)
    }
      
    res <- getWorkunits(input$login, input$webservicepassword, 
                        projectid = input$projectid, 
                        applicationid = applicationid)
    print (res)
    if (is.null(res)){
      return (NULL)
    }else if (length(res) == 0){
      return (NULL)
    }else{
      tagList(
        selectInput(ns("applicationid"), "applicationid:", applicationid, multiple = FALSE),
        selectInput(ns('workunit'), 'workunit:', res, multiple = FALSE)
      )
    }
  })
  
  login <- reactive({input$login})
  webservicepassword <- reactive({input$webservicepassword})
  
  resources <- reactive({
    getResources(input$login, input$webservicepassword, workunitid = strsplit(input$workunit, " - ")[[1]][1])
  })
 
  output$resources <- renderUI({
    
    if (length(input$workunit) == 0){
      
      print ("no resources")
    }else{
      
      print(input$workunit)
      workunitid = strsplit(input$workunit, " - ")[[1]][1]
      print(workunitid)
      
      res <- resources()
     
      if (is.null(res)){
      }else{
       selectInput(ns("resourceid"), "resourceid:", res, multiple = FALSE)
      }
      
    }
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
  
  
  


  return(list(login = reactive({'test'}), resources = reactive({resources()} )))
 #            ç, 
 #             webservicepassword() = reactive({webservicepassword()})
  #            ))
  #return(reactive({
  #  validate(need(input, FALSE))
  #  1
  #}))
  
  
}

