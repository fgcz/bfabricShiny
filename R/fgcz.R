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
    # sort(sapply(rv$res, function(y){paste(y$`_id`, y$name, sep=" - ")}), decreasing = TRUE)
    rv$res
     })
  
  return(resources)
}

