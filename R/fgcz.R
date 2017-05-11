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

