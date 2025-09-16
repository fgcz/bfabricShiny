#R
# Christian Panse <cp@fgcz.ethz.ch>
# 2017-05-06
.workunit2resource <- function(workunit_id = 153887){

  query_url <- paste("http://localhost:5000/zip_resource_of_workunitid/", workunit_id, sep='')

  res <- as.data.frame(fromJSON(query_url))

  return(res)
}

#' @importFrom utils read.csv
.unzip <- function(zipfile=NULL, file=NULL){
  cmd <- paste('unzip -p ', zipfile, file)
  content <- read.csv(pipe(cmd), sep='\t', stringsAsFactors = FALSE, header = TRUE)
  content
}

#' @importFrom utils read.csv
.ssh_unzip <- function(host = 'fgcz-r-035.uzh.ch', user = 'cpanse', zipfile = .workunit2resource(), file = 'proteinGroups.txt'){

  cmd <- paste('unzip -p ', zipfile, file)
  ssh_cmd <- paste("ssh ", user, "@", host, " '", cmd, "'", sep="")
  message(ssh_cmd)

  S <- read.csv(pipe(ssh_cmd), sep='\t', stringsAsFactors = FALSE, header = TRUE)

  S
}

.getMaxQuantFilesNames <- function(S){
  gsub("Intensity\\.", "", grep("Intensity\\.",colnames(S),value=T) )
}

##' Convert List to XML
##'
##' Can convert list or other object to an xml object using xmlNode
##' @title List to XML
##' @importFrom XML xmlNode append.xmlNode xmlAttrs xmlAttrs<-
##' @author David LeBauer, Carl Davidson, Rob Kooper
.listToXml <- function(item, tag) {
  # just a textnode, or empty node with attributes
  if(typeof(item) != 'list') {
    if (length(item) > 1) {
      xml <- XML::xmlNode(tag)
      for (name in names(item)) {
        xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(xmlNode(tag, item))
    }
  }

  # create the node
  if (identical(names(item), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- XML::xmlNode(tag, item[['text']])
  } else {
    # node with child nodes
    xml <- XML::xmlNode(tag)
    for(i in 1:length(item)) {
      if (names(item)[i] != ".attrs") {
        xml <- append.xmlNode(xml, .listToXml(item[[i]], names(item)[i]))
      }
    }
  }

  # add attributes to node
  attrs <- item[['.attrs']]
  for (name in names(attrs)) {
    xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
}


.dummy <- function(){

  XML <- "<soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/'
    xmlns:end='http://endpoint.webservice.component.bfabric.org/'>
    <soapenv:Header/>
    <soapenv:Body>
    <end:checkandinsert>
    <parameters>
    <login>?</login>
    <password>?</password>
    <!--Zero or more repetitions:-->
    <workunit>
    <!--Optional:-->
    <applicationid>?</applicationid>
    <!--Optional:-->
    <projectid>?</projectid>
    <!--Optional:-->
    <name>?</name>
    <!--Optional:-->
    <description>?</description>
    <!--Zero or more repetitions:-->
    <inputresource>
    <!--Optional:-->
    <storageid>?</storageid>
    <!--Optional:-->
    <relativepath>?</relativepath>
    </inputresource>
    <!--Zero or more repetitions:-->
    <resource>
    <!--Optional:-->
    <name>?</name>
    <!--Optional:-->
    <storageid>?</storageid>
    <!--Optional:-->
    <relativepath>?</relativepath>
    <!--Optional:-->
    <weburl>?</weburl>
    <!--Optional:-->
    <size>?</size>
    <!--Optional:-->
    <filechecksum>?</filechecksum>
    </resource>
    </workunit>
    </parameters>
    </end:checkandinsert>
    </soapenv:Body>
    </soapenv:Envelope>"
}

.query_example0_SOAP <- function(){
  login = 'cpanse'
  webservicepassword = "$2a$10$2CkvqICN6UTjXMvPtZ.JFOLqhU8IvzmX.vt37jrUsx8gTsVU.G4r6"
  webbase <-  'https://fgcz-bfabric.uzh.ch/bfabric'

  parameters <- .listToXml(list(login = login,
        webservicepassword = webservicepassword,
        query = list(
          applicationid = 168,
          containerid = 1000
          )
       ), 'parameters')

}

.query_example0_REST <- function(){
  login = ''
  webservicepassword = ""
  url0 <- 'http://localhost:5000/q'
  url <-  'http://localhost:5000/custom'
  rv <- POST(url0,
             body=toJSON(list(login = login,
                              webservicepassword = webservicepassword,
                              endpoint = 'workunit',
                              query=list('applicationid' = 168,
                                         "containerid" = 1000))),
             encode = 'json',
             auto_unbox = TRUE)

  rv <- content(rv)
}

.query_example0_SOAP <- function(login=NULL, webservicepassword=NULL, webbase="http://fgcz-bfabric.uzh.ch/bfabric"){

  url0 <- 'http://localhost:5000/q'
  url <-  'http://localhost:5000/custom'
  rv <- POST(url0,
             body=toJSON(list(login = login,
                              webservicepassword = webservicepassword,
                              endpoint = 'workunit',
                              query=list('applicationid' = 168,
                                         "containerid" = 1000))),
             encode = 'json',
             auto_unbox = TRUE)

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



#' query bfabric
#'
#' @inheritParams readPages
#' @param as_data_frame if TRUE it returns a data.frame object.
#' @return a nested list object
#'
#' @importFrom httr POST
#' @export query
#'
#' @examples
#'
#' \dontrun{
#' query(login, webservicepassword, endpoint='sample', query = list(id=206577))
#' }
#'
#' \dontrun{
#'  RES <- query(endpoint = 'resource',
#'     query = list('filechecksum' = '127f0c5b6352a326f9a6c8458d59d921'),
#'     login, webservicepassword)
#'
#'  WU.pending <- query(endpoint='workunit',
#'     query = list('status' = 'pending'),
#'     login, webservicepassword)
#'
#'  APP.analysis <- query(endpoint='application',
#'     query=list('applicationtype' = 'analysis'),
#'     login, webservicepassword)
#'
#'  # a more complex example
#'
#'  ## query metadata
#'  Q <- query(login, webservicepassword,
#'    endpoint = 'resource',
#'    query = list('workunitid' = 163763))
#'
#'  ## stage data
#'  uris <- sapply(Q$res, function(x){x$uris[3]})
#'  (rawfilenames <- sapply(strsplit(unlist(uris), ":"), function(x){x[3]}))
#'  library(rawfileQC)
#'  library(parallel)
#'  RAW <- do.call('rbind',
#'    mclapply(rawfilenames, read.raw, ssh = TRUE, mc.cores = 12))
#'
#'  ## have fun
#'  hex.bin(RAW)
#' }
#'
#' ## Have Fun!
query <- function(login, webservicepassword,
                  endpoint = 'workunit',
                  query,
                  posturl = 'http://localhost:5000/q',
                  as_data_frame = FALSE){
  .Deprecated("bfabricShiny::read")
  
  query_result <- POST(posturl,
               body = toJSON(list(login = login,
                                  webservicepassword = webservicepassword,
                                  endpoint = endpoint,
                                  query = query
               ),
               encode = 'json',
               auto_unbox = TRUE))

  rv <- content(query_result)
  if(as_data_frame){
    rv <- as.data.frame(do.call('rbind', rv$res))
    rv <- t(apply(rv, 1,unlist))
  }
  rv
}


#=======read======
#' read function which supports pages
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword,
#' visible when you check your user details in the bfabric system.
#' @param endpoint the endpoint, e.g., \code{'sample'}
#' @param query e,g, \code{list(containerid = 3000)}
#' @param posturl where the flask server is working
#' @param maxpages max number of supported pages to 
#' @param updateProgress a callback function for writing log output, e.g.,
#' using a \code{\link[shiny]{Progress}} object,
#' see also \url{https://shiny.rstudio.com/articles/progress.html}.
#' @param page define requested page, default is 1
#' @param posturlsuffix defines the method to use, e.g., read. also, save should work
#' @author MdE/CP 2023-03; CP 2024-12-24
#' @export
#' @examples
#' bfabricShiny::read(login,
#'   webservicepassword,
#'   endpoint = 'sample',
#'   query = list('containerid' = 34777),
#'   posturl = bfabricposturl,
#'   maxitems = 1000) -> rv
#' 
read <- function(login = NULL, webservicepassword = NULL,
                  endpoint = 'workunit',
                  offset = 0,
                  maxitems = 100,
                  query = list(),
                  posturl = NULL,
                  posturlsuffix = 'read',
                  idonly = FALSE,
                  updateProgress = NULL){
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)),
            is.numeric(offset),
            is.numeric(maxitems))
  
  # message(paste("DEBUG XXX", login, webservicepassword, posturl))
  posturl <- paste0(posturl, posturlsuffix)
  
  if (interactive()) {message(paste0("using '", posturl, "' as posturl ..."))}
  start_time <- Sys.time()
  query_result <- httr::POST(posturl,
                             body = list(login = login,
                                         webservicepassword = webservicepassword,
                                         endpoint = endpoint,
                                         query = query,
                                         idonly = idonly,
                                         page_offset = offset,
                                         page_max_results = maxitems
                             ),
                             encode = 'json')
  end_time <- Sys.time()

  diff_time_msg <- paste0(round(difftime(end_time, start_time, units = 'secs'), 2), " [s].")
  rv <- httr::content(query_result)
  if (is.null(rv$res)){warning("query failed."); message(rv); return(rv);}
  
  
  ## TODO(@Leo): is that possible?
  if ('errorreport' %in% names(rv$res)){
    stop(paste0("B-Fabric errorreport: ", rv$res$errorreport))
  }
  
  if (interactive()) {
    message(paste0("read query time: ", diff_time_msg))
  }
  rv
}

#=======readPages======
#' read function which supports pages
#'
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword,
#' visible when you check your user details in the bfabric system.
#' @param endpoint the endpoint, e.g., \code{'sample'}
#' @param query e,g, \code{list(containerid = 3000)}
#' @param posturl where the flask server is working
#' @param maxpages max number of supported pages to 
#' @param updateProgress a callback function for writing log output, e.g.,
#' using a \code{\link[shiny]{Progress}} object,
#' see also \url{https://shiny.rstudio.com/articles/progress.html}.
#'
#' @return a list
#' @export
#'
#' @examples
#' # ensure you have login, webservicepassword, and posturl defined
#' Rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
#' source(Rprofile, local = TRUE)
#' 
#' bfabricShiny::readPages(login, webservicepassword , endpoint = 'user',
#' query=list(login='cpanse'), posturl = bfabricposturl)
#' 
#' bfabricShiny::readPages(login,
#'   webservicepassword,
#'   endpoint = 'user',
#'   query=list(),
#'   posturl = bfabricposturl,
#'   updateProgress =  function(...){cat(...)})
#' 
#' bfabricShiny::readPages(login, webservicepassword , endpoint = 'user',
#' query=list(login='cpanse'), posturl = bfabricposturl)
#' 
#' \dontrun{
#'   fraction <- bfabricShiny::readPages(login = login,
#'     webservicepassword = webservicepassword,
#'     endpoint = 'sample',
#'     query = list( attribute = list(name = 'fraction',
#'         value = 'true')))
#'         }
readPages <- read
  

#===========.getSamples======
#' get samples of a container as data frame object
#' 
#' @inheritParams read
#' @param containerid bfabric container id.
#' @return a \code{data.frame}
#' @author CP 2023-03-14, 2025-01-10 
#' @details
#' 2025-01 replace _id by id
#' 
#' @export
#' @examples
#' bfabricShiny:::.getSamples(login, webservicepassword,
#'    posturl = bfabricposturl,
#'    containerid = 30993) -> smp
#' smp
#' 
#' lapply(c(29941,30021,30041,30057), FUN = bfabricShiny:::.getSamples,
#'   login = login,
#'   webservicepassword = webservicepassword,
#'   posturl = bfabricposturl) -> smp
#' smp
.getSamples <- function(login = NULL,
                        webservicepassword = NULL,
                        posturl = NULL,
                        containerid = NULL,
                        updateProgress = NULL) {
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)),
            isFALSE(is.null(containerid)))
  
  rv <- bfabricShiny::read(login,
                           webservicepassword,
                           endpoint = 'sample',
                           posturl = posturl,
                           query = list(containerid = containerid),
			   maxitems = 500,
                           updateProgress = updateProgress)
  if ('error' %in% names(rv)){
    message(rv$error)
    return (rv)
  }
  rv[[1]] -> rv
  # TODO(cp): rename containerid by container.id
  df <- data.frame(
    samples.id = sapply(rv, FUN = function(x){x$id}) |> as.numeric(),
    samples.name = sapply(rv, FUN = function(x){x$name}),
    samples.condition = lapply(rv, FUN = function(x){x$grouping$name}) |>
      sapply(FUN = function(x){if (is.null(x)){"N/A"}else{x}}),
    containerid = sapply(rv, FUN = function(x){x$container$id})) 
  
  return(df[order(df$samples.id), ])
}

#' @inheritParams read
#' @noRd
.getContainers <- function(login, webservicepassword,  posturl = NULL,
                           updateProgress = NULL) {
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)))
  
  
  rv <- bfabricShiny::read(login, webservicepassword,
                           endpoint = 'user',
                           posturl = posturl,
                           query = list(login = login),
                           updateProgress = updateProgress)
  
  
  if ('error' %in% names(rv)){
    message(rv$error)
    return (rv)
  }
  
  rv[[1]] -> rv
  ## extracting container ids
  projetcs <- sapply(rv[[1]]$project, function(y){y$id})
  coachedprojects <- sapply(rv[[1]]$coachedproject, function(y){y$id})
  orders <- sapply(rv[[1]]$order, function(y){y$id})
  
  containers <- c(unlist(projetcs), unlist(coachedprojects), unlist(orders)) |> sort(decreasing = TRUE)
  
  
  # return container ids
  return(containers)
}


#' CP 2024-12-24
#' @inheritParams read
.getWorkunits <- function(login = NULL,
                          webservicepassword =  NULL,
                          posturl = NULL,
                          containerid = 3000,
                          applicationid = 224,
                          updateProgress = NULL,
                          createdbefore = NULL){
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)))
  
  query <- list('applicationid' = applicationid,
                'status' = 'available',
                'containerid' = containerid)
  
  if (!is.null(createdbefore)) {
    query$createdbefore <- createdbefore
  }
  
  
  rv <- bfabricShiny::read(login = login,
                           webservicepassword = webservicepassword,
                           posturl = posturl,
                           endpoint = 'workunit',
                           query = query,
                           updateProgress = updateProgress)
  
  
  if ('error' %in% names(rv)){
    message("DEBUG")
    message(rv$error)
    return (rv)
  }else if (is.null(rv)){
    return(NULL)
  }
  
  ## this is new due to refactoring! 2024-12-24
  rv[[1]] -> rv
  if (length(rv) > 0){
    ## composing a vector of workunit ids and workunit names
    rv <- sapply(rv, function(y){paste(y$id, y$name, sep=" - ")})
    rv <- sort(rv, decreasing = TRUE)
  }
  
  return(rv)
}

#' get all resources of a (login, project)
#'
#' @inheritParams read
#' @param workunitid  a workunit Id
#'
#' @return a vector of resource ids
.getResources <- function(login=NULL, webservicepassword=NULL,
                          posturl=NULL,
                          workunitid = NULL,
                          updateProgress = NULL,
                          createdbefore = NULL){
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)),
            isFALSE(is.null(workunitid)))
  
  query <- list('workunitid' = workunitid)
  if (!is.null(createdbefore)){
    query$createdbefore <- createdbefore
  }

  rv <- bfabricShiny::read(login, webservicepassword,
                                  endpoint = 'resource',
                                  posturl = posturl,
                                  query = query,
                                  updateProgress = updateProgress
                                  )
 

  if ('error' %in% names(rv)){
    message(rv$error)
  }
  return(rv[[1]])
}


#' getApplications
#'
#' @inheritParams read
#' @return list of bfabric applications
#' @examples
#' \dontrun{
#' library(bfabricShiny)
#' A <- .getApplications(login, webservicepassword)
#' bfabricApplication <- data.frame(id = sapply(A, function(x){x$id}),
#'  name = sapply(A, function(x){x$name}))
#' bfabricApplication <- bfabricApplication[order(bfabricApplication$id),]
#' write.table(bfabricApplication, file="./inst/extdata/application.csv",
#'   row.names = FALSE)
#' }
#'
.getApplications <- function(login, webservicepassword, posturl){
  bfabricShiny::read(login, webservicepassword,
                          endpoint = 'application',
                          posturl = posturl,
                          query = list())
  
  if ('error' %in% names(rv)){
    message(rv$error)
    return(rv)
  }
  rv[[1]]
}

#' Saves an object in bfabric
#' 
#' @inheritParams readPages
#' 
#' @return bfabric json object.
#' @author Christian Panse <cp@fgcz.ethz.ch> 2016-2023, MdE 2023-03-17
#' @export
#' @examples 
#' \dontrun{
#' bfabricShiny::save(login, webservicepassword , 'workunit',
#'   posturl = bfabricposturl,
#'   list(applicationid = 212, description = 'test2', containerid = 3000))
#' }
save <- function(login = NULL,
                 webservicepassword = NULL,
                 endpoint = 'workunit',
                 posturl = 'http://localhost:5000/',
                 query = NULL){
  
  return(read(login, webservicepassword, endpoint = endpoint, query = query,
               posturl = posturl, posturlsuffix = 'save'))
}

#' Create a workunit
#' @aliases bfabricUploadFile
#' @importFrom httr POST content
#' @importFrom jsonlite toJSON
#' @author Christian Panse <cp@fgcz.ethz.ch> 2021
#' 
#' @examples 
#' \dontrun{
#' bfabricShiny:::.createWorkunit(login, webservicepassword, posturl=posturl,
#' containerid=3000, applicationid=212, inputresourceid=list(1753925, 1753924))
#' }
#' 
.createWorkunit <-
  function(login = NULL,
           webservicepassword = NULL,
           posturl = NULL,
           containerid=NULL,
           applicationid=NULL,
           inputresourceid = NULL,
           status = 'PENDING',
           description = '',
           name='') {
    
    stopifnot(isFALSE(is.null(login)),
              isFALSE(is.null(webservicepassword)),
              isFALSE(is.null(containerid)),
              isFALSE(is.null(posturl)),
              isFALSE(is.null(applicationid))
              )
    
    queryObject <- list(
      'containerid' = containerid,
      'applicationid' = applicationid,
      'name' = name,
      'status' = status,
      'description' = description
    )
    
    if (isFALSE(is.null(inputresourceid))){
      queryObject$inputresourceid  <- inputresourceid
    }
    
    rv <- bfabricShiny::save(login, webservicepassword,
                             posturl = posturl,
                             endpoint = 'workunit',
                             query = queryObject)
    
    #rv <- httr::POST(paste0(posturl, '/s'),
    #           body = jsonlite::toJSON(
    #             list(
    #               login = login,
    #               webservicepassword = webservicepassword,
    #               endpoint = 'workunit',
    #               query = queryObject
    #             ),
    #             encode = 'json'
    #           ))
    
   
    #rv <- content(rv)
    return(rv)
  }


.saveResource <- function(login,
                         webservicepassword,
                         posturl = NULL,
                         workunitid,
                         content,
                         name){

  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)))
            
  
  rv <- POST(paste0(posturl, '/save'),
             body = toJSON(
               list(
                 login = login,
                 webservicepassword = webservicepassword,
                 endpoint = 'resource',
                 query = list(
                   'name' = sprintf("WU%s-%s-%s", workunitid,
                                    format(Sys.time(), format="%Y%m%d-%H%M"), name),
                   'workunitid' = workunitid,
                   'base64' = content
                 )
               ),
               encode = 'json',
               auto_unbox = TRUE,
             ))
  rv <- content(rv)

  return(rv$res)  
}

#' Generate a workunit and upload a resource (file) to an internal bfabric
#' storage.
#'
#' @inheritParams readPages
#' @param projectid a containerid (project id or order id)
#' @param applicationid a application id
#' @param status in \code{c('AVAILABLE', 'FAILED', 'PENDING')}
#' default is 'pending'.
#' @param inputresourceid an integer or a list of integer inputresourceIds. 
#' Of note, this works only for  succeeding/preceding applications.
#' Default is set to \code{NULL}.
#' @param workunitname the workunit name
#' @param resourcename the reosurce name
#' @param file_content a BLOB containing the content
#' @importFrom base64enc base64encode
#'
#' @return the workunit id
#' @export
bfabric_upload_file <- function(login = NULL,
                        webservicepassword = NULL,
                        projectid = 3000,
                        applicationid = 217,
                        status = 'pending',
                        description = '',
                        inputresourceid = NULL,
                        workunitname = 'MaxQuant result',
                        resourcename = 'MaxQuant report',
                        file_content = NULL) {
  .Deprecated('use bfabricShiny::uploadResource')
  stopifnot(isFALSE(is.null(login)), isFALSE(is.null(webservicepassword)))
  
    description <- 
      sprintf("%s\nGenerated by Rpkg https://github.com/fgcz/bfabricShiny/ version %s.\n System information: %s",
              description,
              packageVersion('bfabricShiny'),
              paste(Sys.info(), collapse = ', '))
  
  
  message(workunitname)
  message(resourcename)
  
  wu <-
    .createWorkunit(
      login = login,
      webservicepassword = webservicepassword,
      containerid = projectid,
      inputresourceid = inputresourceid,
      applicationid = applicationid,
      name = workunitname,
      status = status,
      description = description
    )


  r <- .saveResource(login, webservicepassword,
                     workunitid = wu[[1]]$id,
                    content = file_content,
                    name = resourcename)

  wu[[1]]$id
}

#=====uploadResource=======
#' Generate a workunit and upload a resource (file)  to a internal bfabric
#' storage
#'
#' @inheritParams readPages
#' @param containerid a containerid (project id or order id)
#' @param applicationid a application id
#' @param status in \code{c('AVAILABLE', 'FAILED', 'PENDING')}
#' default is 'PENDING'.
#' @param description free text, default is is empty.
#' @param inputresourceid an integer or a list of integer of inputresourceIds. 
#' Of note, this works only for  succeeding/preceding applications.
#' Default is set to \code{NULL}.
#' @param workunitname the workunit name
#' @param resourcename the reosurce name
#' @param file a filename for a file to be uploaded.
#'
#' @return returns a nested list containing the workunit and the resource
#' object returned by the save method.
#' @importFrom base64enc base64encode
#' @importFrom tools file_ext
#' @author Christian Panse <cp@fgcz.ethz.ch> 2016-2023, MdE 2023-03-17
#' @export
#' @examples
#' fRp <- file.path(Sys.getenv("HOME"), ".Rprofile")
#' source(fRp, local=TRUE)
#' tf <- tempfile()
#' write.csv(iris, file=tf)
#' rv <- bfabricShiny::uploadResource(login, webservicepassword, bfabricposturl,
#'    containerid = 3000,
#'    status = 'PENDING',
#'    description = "generated by a test run",
#'    applicationid = 212,
#'    workunitname = "bfabricShiny example",
#'    resourcename = "R's iris data",
#'    file = tf)
#'    
#' print(rv)
uploadResource <- function(login = NULL,
                                webservicepassword = NULL,
                                posturl = NULL,
                                containerid = 3000,
                                applicationid = 212,
                                status = 'PENDING',
                                description = '',
                                inputresourceid = NULL,
                                workunitname = 'bfabricShiny result',
                                resourcename = 'bfabricShiny report',
                                file = NULL) {
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(file)),
            isFALSE(is.null(containerid)),
            isFALSE(is.null(applicationid)),
            isFALSE(is.null(posturl)),
            file.exists(file)
            )
  
  stopifnot(status %in% c('AVAILABLE', 'FAILED', 'PENDING'))
  
  fileContent <- readBin(file, "raw", file.info(file)[1, "size"]) |>
    base64enc::base64encode(tools::file_ext(file))
  
  description <- 
    sprintf("%s\n\n
Generated by Rpkg https://github.com/fgcz/bfabricShiny/ version %s.
System information: %s\n
To help us funding further development, please cite:
(bfabricSiny) PMID: 36073980 DOI: 10.1515/jib-2022-0031",
            description,
            packageVersion('bfabricShiny'),
            paste(Sys.info(), collapse = ', '))
  
  wu <-
    .createWorkunit(
      login = login,
      webservicepassword = webservicepassword,
      posturl = posturl,
      containerid = containerid,
      inputresourceid = inputresourceid,
      applicationid = applicationid,
      name = workunitname,
      status = status,
      description = description
    )
  
  #res <- 
  #  .saveResource(login, webservicepassword,
  #                   posturl = posturl,
  #                   workunitid = wu[[1]]$id,
  #                   content = fileContent,
  #                   name = resourcename
  #
  workunitid <- wu$res[[1]]$id
  res <- save(login, webservicepassword,
              posturl = posturl,
              endpoint = 'resource',
              query = list(
                'name' = sprintf("WU%s-%s-%s",workunitid,
                                 format(Sys.time(), format="%Y%m%d-%H%M"), resourcename),
                'workunitid' = workunitid,
                'base64' = fileContent
              )
              
  )
  
  list(workunit = wu, resource = res)
}


# sanity check for uploading file to bfabric
.sanityCheckUploadResource <- function(){
  inputfile1 <- file.path(system.file(package = 'aaa'),
                          'extdata', '20210311_Qda_order_24259_H_Elhawi_extract.txt')
  
  inputfile2 <- file.path(system.file(package = 'aaa'),
                          'extdata', 'amino-acid-analysis-report-template.html')
  
  stopifnot(file.exists(inputfile1), file.exists(inputfile1))
  
  wu1 <- bfabricShiny::uploadResource(login, webservicepassword, 
                                         containerid = 3000,
                                         applicationid = 499,
                                         status = 'PENDING',
                                         description = 'AAA sanity check input',
                                         workunitname = "AAA report",
                                         resourcename = 'csv file',
                                         file = inputfile1)
  
  wu2 <- bfabricShiny::uploadResource(login, webservicepassword, 
                                         containerid = 3000,
                                         applicationid = 500,
                                         status = NULL,
                                         description = 'AAA sanity check output',
                                         workunitname = "AAA report",
                                         resourcename = 'html report',
                                         inputresourceid = wu1$resource[[1]]$id,
                                         file = inputfile2)
}

#' reads Custom Attributes of a workunit
#'
#' @inheritParams readPages
#' @param workunitid a valid bfabric workunit id
#'
#' @return a \code{data.frame} containing columns sampleId , resourceId, and
#' iff exisiting a merged table of the Custom Attributes.
#'
#' @export
.readCustomAttributes <- function(login, webservicepassword, workunitid = 202570){

  inputResourcesIds <- unlist(read(login, webservicepassword,
    endpoint = 'workunit',
      query = list(id=workunitid))[[1]][[1]]$inputresource)


  inputSampleIds <-  vapply(inputResourcesIds, function(x){
    read(login, webservicepassword, endpoint = 'resource',
         query = list(id=x))[[1]][[1]]$sample$id
    }, FUN.VALUE = 202691)


  samples <- lapply(inputSampleIds, function(x){
    read(login, webservicepassword, endpoint = 'sample',
         query = list(id=x))[[1]][[1]]
  })

  do.call('rbind', lapply(samples, as.data.frame))

  data.frame(resourceId=inputResourcesIds, sampleId=inputSampleIds)
}

.Rprofile <- function(){ 
  f <- file.path(Sys.getenv("HOME"), ".Rprofile") 
  if (file.exists(f)){ return (f) }
  stop("no '.Rprofile'")
}

.login <- function(){
  source(.Rprofile(), local = TRUE)
  message(paste0("read login ", login, "."))
  stopifnot('login' %in% ls())
  return (login)
}

.posturl <- function(){
  source(.Rprofile(), local = TRUE)
  message(paste0("read bfabricposturl ", bfabricposturl, "."))
  stopifnot('bfabricposturl' %in% ls())
  return (bfabricposturl)
}

.webservicepassword <- function(){
  source(.Rprofile(), local = TRUE)
  message(paste0("read webservicepassword for login ", login, "."))
  stopifnot('webservicepassword' %in% ls())
  return(webservicepassword)
}


#' Validate Token
#'
#' @param token a given token
#' @param posturl the bfabric REST proxy URL
#'
#' @returns login webservicepassword
#' @author LS,CP 2025-07-03 devDay
#' @importFrom httr POST content
.validateToken <- function(token, posturl = bfabricposturl) {
  paste0(posturl, "/validate_token") -> posturl 
  print("DEBUG running .validateToken ...")
  httr::POST(posturl,  
             body = list(token = token),
             encode = 'json') |>
    httr::content() -> qr

  if ("error" %in% names(qr)){
    return (NULL)
  }
  return(qr) 
}
