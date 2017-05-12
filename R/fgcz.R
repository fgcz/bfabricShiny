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


.ttt <- function(){
  login = 'cpanse'
  webservicepassword = "$2a$10$We8McOYkCp7iCFzaTCgDoepBe2KkrzkiLKvh0o.v9u8tIQCYmD.D6"
  url0 <- 'http://localhost:5000/q'
  url <-  "localhost:5000/custom"
  rv <- POST(url, 
             body=toJSON(list(login = login, 
                              webservicepassword = webservicepassword,
                              endpoint = 'workunit', 
                              query=list('applicationid' = 168, "projectid"=1000))), 
             encode = 'json')
  
  rv <- content(rv)
}
  

#' get all resources of a (login, project) 
#'
#' @param login 
#' @param webservicepassword 
#' @param project 
#'
#' @return a vector of resource ids
#' @export getResources 
getResources <- function(login, webservicepassword, project) {
  
  resources <- ({
    rv <- POST('http://localhost:5000/query', 
               body = toJSON(list(login = login, 
                                  webservicepassword = webservicepassword,
                                  query = 'resource',
                                  projectid = project,
                                  applicationid = 205)), 
               encode = 'json')
    
    rv <- content(rv)
    sort(unlist(rv$workunits), decreasing = TRUE)
  })
  
  return(resources)
}


#' Module UI function
#'
#' @param id 
#' @param login 
#' @param password 
#'
#' @return
#' @export shinyUIModule
shinyUIModule <- function(id, login, password) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    textInput(ns('login'), 'bfabric Login', login),
    passwordInput(ns('webservicepassword'), 
                  'Web Service Password', 
                  password),
    htmlOutput(ns("projects")),
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
shinyServerModule <- function(input, output, session) {
  
  ns <- session$ns
  
  output$projects <- renderUI({
    projects <- getProjects(input$login, input$webservicepassword)
    
    if (is.null(projects)){
      # textOutput('no project yet')
    }else{
      selectInput(ns("project"), "xxxProjetcs", projects, multiple = FALSE)
    }
  })
  
  output$resources <- renderUI({
    res <- getResources(input$login, input$webservicepassword, input$project)
    if (is.null(res)){
      # 
    }else{
      selectInput(ns('resource'), 'resource:', res, multiple = FALSE)
    }
  })
  
  return(reactive({
    validate(need(input$pprojectl, FALSE))
    1
  }))
}
