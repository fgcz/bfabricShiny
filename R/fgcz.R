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
             encode = 'json')

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
  .Deprecated("bfabricShiny::readPages")
  
  query_result <- POST(posturl,
               body = toJSON(list(login = login,
                                  webservicepassword = webservicepassword,
                                  endpoint = endpoint,
                                  query = query
               ),
               encode = 'json'))

  rv <- content(query_result)
  if(as_data_frame){
    rv <- as.data.frame(do.call('rbind', rv$res))
    rv <- t(apply(rv, 1,unlist))
  }
  rv
}


## TODO(cp): this function should replace the read function
#' .read
#' @inheritParams readPages
#' @param page define requested page, default is 1
#' @param posturlsuffix defines the method to use, e.g., read. also, save should work
#' @author MdE/CP 2023-03
.read <- function(login = NULL, webservicepassword = NULL,
                  endpoint = 'workunit',
                  page = 1,
                  query = list(),
                  posturl = NULL,
                  posturlsuffix = 'read',
                  idonly = FALSE,
                  updateProgress = NULL){
  
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)),
            is.numeric(page))
  
  # message(paste("DEBUG XXX", login, webservicepassword, posturl))
  posturl <- paste0(posturl, posturlsuffix)
  
  if (interactive()) {message(paste0("using '", posturl, "' as posturl ..."))}
  start_time <- Sys.time()
  query_result <- httr::POST(posturl,
                             body = jsonlite::toJSON(list(login = login,
                                                          webservicepassword = webservicepassword,
                                                          endpoint = endpoint,
                                                          query = query,
                                                          idonly = idonly,
                                                          page = page
                             ),
                             encode = 'json'))
  end_time <- Sys.time()

  diff_time_msg <- paste0(round(difftime(end_time, start_time, units = 'secs'), 2), " [s].")
  rv <- httr::content(query_result)
  if (is.null(rv$res)){warning("query failed."); return(rv);}
  
  if ('errorreport' %in% names(rv$res)){
    stop(paste0("B-Fabric errorreport: ", rv$res$errorreport))
  }
  
  if (interactive()) {
    
    msg <- paste0("idonly: ", idonly, "\n",
                  "endpoint: ", endpoint, "\n",
                  "entitiesonpage: ", rv$res$entitiesonpage, "\n",
                  "numberofpages: ", rv$res$numberofpages, "\n",
                  "page: ", rv$res$page)
    message(msg)
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      msg <- sprintf("read (idonly=%s) %d/%d %s page(s) (%d items) in %s",
                     idonly,
                     rv$res$page, rv$res$numberofpages, endpoint, rv$res$entitiesonpage,
                     diff_time_msg)
      
      updateProgress(value = rv$res$page, detail = msg, n = rv$res$numberofpages)
    }
    
    message(paste0("query time: ", diff_time_msg))
  }
  rv$res
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
readPages <- function(login = NULL,
                      webservicepassword = NULL,
                      endpoint = 'workunit',
                      query = list(),
                      posturl = 'http://localhost:5000/',
                      maxpages = 10,
                      updateProgress = NULL){
  
  rv <- .read(login = login,
              webservicepassword = webservicepassword,
              endpoint = endpoint,
              query = query,
              idonly = TRUE,
              posturl = posturl,
              updateProgress = updateProgress,
              posturlsuffix = 'read')
  
  
  # TODO(CP): too lazy to program; so start with page 1
  if ('errorreport' %in% names(rv)){
    return(rv)
  }
  
  if ('status' %in% names(rv)){
    return(rv)
  }
  
  if ('entitiesonpage' %in% names(rv)){
    if (rv$entitiesonpage == 0) return (NULL)
  }
  
  if(rv$numberofpages > 0){
    rv <- lapply(seq(1, min(rv$numberofpages, maxpages)),
                 FUN = .read,
                 login = login,
                 webservicepassword = webservicepassword,
                 endpoint = endpoint,
                 query = query,
                 posturl = posturl,
                 updateProgress = updateProgress,
                 posturlsuffix = 'read') |> 
      lapply(FUN = function(x){get(endpoint, x)}) |> 
      unlist(recursive = FALSE)
    return (rv)
  }else{
    return(get(endpoint, rv))
  }
  
  return(rv)
}
  
#' read method to access bfabric REST
#'
#' @inheritParams readPages
#' @param posturl POST url, default is \code{'http://localhost:5000/q'}.
#' @param as_data_frame if TRUE it returns a data.frame object.
#' @return a nested list object
#'
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @export read
#'
#' @examples
#' # ensure you have login and password
#' Rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
#' source(Rprofile, local = TRUE)
#' 
#' res.sample <- bfabricShiny::read(login = login,
#'    webservicepassword = webservicepassword,
#'    endpoint = 'sample',
#'    query = list(containerid = 27053),
#'    as_data_frame = TRUE)
#'    
#' res.sample |> subset(select=c("_id", "name", "groupingvar.name"))
#'    
#' res.file <- bfabricShiny::read(login, webservicepassword,
#'   endpoint = 'resource',
#'   query = list('filechecksum' = '127f0c5b6352a326f9a6c8458d59d921'),
#'   )
#'   
#' res.workunit <- bfabricShiny::read(login, webservicepassword, 
#'     endpoint='workunit',
#'     query = list('status' = 'pending'),
#'     as_data_frame = TRUE)
#'       
#'
#'  ## query metadata
#'  Q <- bfabricShiny::read(login, webservicepassword,
#'    endpoint = 'resource',
#'    query = list('workunitid' = 163763))
#'    
read <- function(login = NULL, webservicepassword = NULL,
                  endpoint = 'workunit',
                  query,
                  posturl = 'http://localhost:5000/',
                  as_data_frame = FALSE){
  

  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)))
  
  if (interactive()) {
    .Deprecated("bfabricShiny::readPages")
    message(paste0("using '", posturl, "' as posturl ..."))
    }
  
  query_result <- httr::POST(paste0(posturl, "/q"),
                       body = jsonlite::toJSON(list(login = login,
                                          webservicepassword = webservicepassword,
                                          endpoint = endpoint,
                                          query = query
                       ),
                       encode = 'json'))

  rv <- httr::content(query_result)
  if (is.null(rv$res)){warning("query failed."); return(rv);}
  if(as_data_frame){
    if (interactive()) {message("reshaping list to data.frame object ...")}
    rv <- Reduce(rbind, rv$res) |>
      as.data.frame() |>
      apply(1, unlist) |>
      t()
  }
  rv
}

#===========.getSamples======
#' get samples of a container as data frame object
#' @inheritParams readPages
#' @param containerid bfabric container id.
#' @return a \code{data.frame}
#' @author CP 2023-03-14
#' @export
#' @examples
#' smp <- bfabricShiny:::.getSamples(login, webservicepassword,
#'    posturl = bfabricposturl,
#'    containerid = 30993)
.getSamples <- function(login = NULL,
                        webservicepassword = NULL,
                        posturl = NULL,
                        containerid = NULL, updateProgress=NULL) {
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)),
            isFALSE(is.null(containerid)))
  
  rv <- bfabricShiny::readPages(login,
                                webservicepassword,
                                endpoint = 'sample',
                                posturl = posturl,
                                query = list(containerid = containerid),
                                updateProgress = updateProgress)
  
  df <- data.frame(
    samples._id = sapply(rv, FUN = function(x){x$`_id`}) |> as.numeric(),
    samples.name = sapply(rv, FUN = function(x){x$name}),
    samples.condition = lapply(rv, FUN = function(x){x$grouping$name}) |>
      sapply(FUN = function(x){if (is.null(x)){"N/A"}else{x}}),
    containerid = sapply(rv, FUN = function(x){x$container$`_id`})) 
  
  return(df[order(df$samples._id), ])
}

#' @noRd
#' @return a vector of project ids
.getContainers <- function(login, webservicepassword,  posturl = NULL,
                           updateProgress = NULL) {
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)))
  
  #containers <- ({
    rv <- bfabricShiny::readPages(login, webservicepassword,
                                  endpoint = 'user',
                                  posturl = posturl,
                                  query = list(login = login),
                                  updateProgress = updateProgress)
    
    if ('errorreport' %in% names(rv)){
      return (rv)
    }
    
    projetcs <- sapply(rv[[1]]$project, function(y){y$`_id`})
    coachedprojects <- sapply(rv[[1]]$coachedproject, function(y){y$`_id`})
    orders <- sapply(rv[[1]]$order, function(y){y$`_id`})
    
    containers <- c(unlist(projetcs), unlist(coachedprojects), unlist(orders)) |> sort(decreasing = TRUE)
  #})
  
 # containers
    return(containers)
}


#' @noRd
.getWorkunits <- function(login = NULL, webservicepassword =  NULL,
                          posturl = NULL,
                          containerid = 3000,
                          applicationid = 224,
                          updateProgress = NULL){
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)))
  
   workunits <- ({
    rv <- bfabricShiny::readPages(login = login,
                                  webservicepassword = webservicepassword,
                                  posturl = posturl,
                                  endpoint = 'workunit',
                                  query=list('applicationid' = applicationid,
                                             'status' = 'available',
                                             'containerid' = containerid),
                                  updateProgress = updateProgress)
    
    if ('errorreport' %in% names(rv)){
      return (rv)
    }
    
    if (is.null(rv)) return(NULL)
    rv <- sapply(rv, function(y){paste(y$`_id`, y$name, sep=" - ")})

    if (length(rv) > 0){
      rv <- sort(rv, decreasing = TRUE)
    }
    rv
  })
  return(workunits)
}

#' get all resources of a (login, project)
#'
#' @inheritParams readPages
#' @param workunitid  a workunit Id
#'
#' @return a vector of resource ids
.getResources <- function(login=NULL, webservicepassword=NULL,
                          posturl=NULL,
                         workunitid = NULL,
                         updateProgress = NULL){
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(posturl)),
            isFALSE(is.null(workunitid)))
  
  
  resources <- bfabricShiny::readPages(login, webservicepassword,
                                  endpoint = 'resource',
                                  posturl = posturl,
                                  query = list('workunitid' = workunitid),
                                  updateProgress = updateProgress
                                  )
 

  return(resources)
}


#' getApplications
#'
#' @inheritParams readPages
#' @return list of bfabric applications
#' @examples
#' \dontrun{
#' library(bfabricShiny)
#' A <- .getApplications(login, webservicepassword)
#' bfabricApplication <- data.frame(id = sapply(A, function(x){x$`_id`}),
#'  name = sapply(A, function(x){x$name}))
#' bfabricApplication <- bfabricApplication[order(bfabricApplication$id),]
#' write.table(bfabricApplication, file="./inst/extdata/application.csv",
#'   row.names = FALSE)
#' }
#'
.getApplications <- function(login, webservicepassword, posturl){
  bfabricShiny::readPages(login, webservicepassword,
                          endpoint = 'application',
                          posturl = posturl,
                          query = list())
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
  
  return(.read(login, webservicepassword, endpoint = endpoint, query = query,
               posturl = posturl, posturlsuffix = 's'))
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
            
  rv <- POST(paste0(posturl, '/s'),
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
               encode = 'json'
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
                     workunitid = wu[[1]]$`_id`,
                    content = file_content,
                    name = resourcename)

  wu[[1]]$`_id`
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
  
  res <- 
    .saveResource(login, webservicepassword,
                     posturl = posturl,
                     workunitid = wu[[1]]$`_id`,
                     content = fileContent,
                     name = resourcename
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
                                         inputresourceid = wu1$resource[[1]]$`_id`,
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
         query = list(id=x))[[1]][[1]]$sample$`_id`
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


