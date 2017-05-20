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
  
  print (paste("login:", login))
  projetcs <- ({
    rv <- POST('http://localhost:5000/query', 
               body=toJSON(list(login=login, 
                                webservicepassword=webservicepassword,
                                query='project')), 
               encode = 'json')
    
    rv <- content(rv)
    sort(unlist(rv$project), decreasing = TRUE)
  })
  
  
  return(projetcs)
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

getWorkunits <- function(login, webservicepassword, projectid = 1000, applicationid = 168){
  
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
    (sapply(rv$res, function(y){paste(y$`_id`, y$name, sep=" - ")}))
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
  
  resources <- ({
    rv <- POST('http://localhost:5000/q', 
               body = toJSON(list(login = login, 
                                  webservicepassword = webservicepassword,
                                  endpoint = 'resource', 
                                  query=list('workunitid' = workunitid)
                                 ), 
               encode = 'json'))
    
    rv <- content(rv)
    sort(sapply(rv$res, function(y){y$`_id`}), decreasing = TRUE)
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
  privKey <- PKI.load.key(file=file.path(system.file("keys", package = "bfabricShiny"), "test.key"))
  
 
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
#'
#' @return
#' @export shinyServerModule
shinyServerModule <- function(input, output, session, applicationid) {
  
  ns <- session$ns
  pubKey <- PKI.load.key(file=file.path(system.file("keys", package = "bfabricShiny"), "test.key.pub"))
  print("shinyServer")
  print(pubKey)
  
  output$projects <- renderUI({
    projects <- getProjects(input$login, input$webservicepassword)
    
    if (is.null(projects)){
    }else{
      
      selectInput(ns("project"), "Projetcs", projects, multiple = FALSE)
    }
  })
  
  
  output$workunits <- renderUI({
    res <- getWorkunits(input$login, input$webservicepassword, input$projectid, applicationid)
    if (is.null(res)){
      return (NULL)
    }else{
      tagList(
        selectInput("applicationid", "applicationid:", applicationid, multiple = FALSE),
        selectInput(ns('workunit'), 'workunit:', res, multiple = FALSE)
      )
    }
  })
  
  
  output$resources <- renderUI({
    if (is.null(input$workunit)){
      return (NULL)
    }
    workunitid = strsplit(input$workunit, " - ")[[1]][1]
    res <- getResources(input$login, input$webservicepassword, workunitid)
    if (is.null(res)){
    }else{
      selectInput(ns("resourceid"), "resourceid:", res, multiple = FALSE)
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
  
  return(reactive({
    validate(need(input$projects, FALSE))
    1
  }))
  
  
}
