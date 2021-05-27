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


#' queries projects of a login
#'
#' @param login bfabric login 
#' @param webservicepassword bfabric webservicepassword
#'
#' @importFrom httr POST
#' @importFrom httr content
#' @return a vector of project ids
#' @export getProjects
getProjects <- function(login, webservicepassword) {
  stopifnot(isFALSE(is.null(login)), isFALSE(is.null(webservicepassword)))
  projetcs <- ({
    rv <- httr::POST('http://localhost:5000/q',
               body = toJSON(list(login = login,
                                webservicepassword = webservicepassword,
                                endpoint = 'user',
                                query = list('login' = login))),
               encode = 'json')

    rv <- httr::content(rv)
    rv_p <- sapply(rv$res[[1]]$project, function(y){y$`_id`})
    rv_cp <- sapply(rv$res[[1]]$coachedproject, function(y){y$`_id`})
    rv_o <- sapply(rv$res[[1]]$order, function(y){y$`_id`})

    sort(c(unlist(rv_p), unlist(rv_cp), unlist(rv_o)), decreasing = TRUE)
  })


  projetcs
}


#' query bfabric
#'
#' @param endpoint the endpoint, e.g., workunit, resource, application, project.
#' @param query the query object list
#' @param login bfabric login
#' @param webservicepassword bfabric password, check user details.
#' @param posturl POST url, default is \code{'http://localhost:5000/q'}.
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
               encode = 'json'))

  rv <- content(query_result)
  if(as_data_frame){
    rv <- as.data.frame(do.call('rbind', rv$res))
    rv <- t(apply(rv, 1,unlist))
  }
  rv
}


#' read method to access bfabric REST
#'
#' @param endpoint the endpoint, e.g., workunit, resource, application, project.
#' @param query the query object list
#' @param login bfabric login
#' @param webservicepassword bfabric password, check user details.
#' @param posturl POST url, default is \code{'http://localhost:5000/q'}.
#' @param as_data_frame if TRUE it returns a data.frame object.
#' @return a nested list object
#'
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @export read
#'
#' @examples
#'
#' \dontrun{
#' bfabricShiny::query(login, webservicepassword, endpoint='sample', query = list(id=206577))
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
#'  library(rawDiag)
#'  library(parallel)
#'  RAW <- do.call('rbind',
#'    mclapply(rawfilenames, read.raw, ssh = TRUE, mc.cores = 12))
#'
#'  ## have fun
#'  hex.bin(RAW)
#' }
#'
read <- function(login = NULL, webservicepassword = NULL,
                  endpoint = 'workunit',
                  query,
                  posturl = 'http://localhost:5000/q',
                  as_data_frame = FALSE){

  stopifnot(isFALSE(is.null(login)), isFALSE(is.null(webservicepassword)))
  
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


# getWorkunits(login, webservicepassword)
getWorkunits <- function(login=NULL, webservicepassword=NULL, projectid = 3000, applicationid = 224){
  stopifnot(isFALSE(is.null(login)), isFALSE(is.null(webservicepassword)))
  
   workunits <- ({
    rv <- POST('http://localhost:5000/q',
               body = toJSON(list(login = login,
                                  webservicepassword = webservicepassword,
                                  endpoint = 'workunit',
                                  query=list('applicationid' = applicationid,
                                             'status' = 'available',
                                             'containerid' = projectid)
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
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword
#' @param project a project or order Id
#'
#' @export
#' @return a vector of resource ids
getResources <- function(login=NULL, webservicepassword=NULL, workunitid=NULL){
  
  stopifnot(isFALSE(is.null(login)),
            isFALSE(is.null(webservicepassword)),
            isFALSE(is.null(workunitid)))
  

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


#' getApplications
#'
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword
#'
#' @return
#' @export
#' @examples
#' \dontrun{
#' library(bfabricShiny)
#' A <- getApplications(login, webservicepassword)
#' bfabricApplication <- data.frame(id = sapply(A, function(x){x$`_id`}),
#'  name = sapply(A, function(x){x$name}))
#' bfabricApplication <- bfabricApplication[order(bfabricApplication$id),]
#' write.table(bfabricApplication, file="./inst/extdata/application.csv",
#'   row.names = FALSE)
#' }
#'
getApplications <- function(login, webservicepassword){
  applications <- ({
    rv <- POST('http://localhost:5000/q',
               body = toJSON(list(login = login,
                                  webservicepassword = webservicepassword,
                                  endpoint = 'application',
                                  query = list()
               ),
               encode = 'json'))

    rv <- content(rv)
    rv$res
  })
  return(applications)
}


createWorkunit <-
  function(login,
           webservicepassword,
           containerid,
           applicationid,
           inputresource,
           status = 'available',
           description = '',
           name ) {
    workunitid <- NA
    rv <- POST('http://localhost:5000/s',
               body = toJSON(
                 list(
                   login = login,
                   webservicepassword = webservicepassword,
                   endpoint = 'workunit',
                   query = list(
                     'containerid' = containerid,
                     'applicationid' = applicationid,
                     'name' = name,
                     'status' = status,
                     'description' = description
                   )
                 ),
                 encode = 'json'
               ))

    #'inputresource' = list(inputresource),

    rv <- content(rv)
    print(rv)
    return(rv$res)
  }


#' Saves an object in bfabric
#'
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword
#' @param endpoint bfabric endpoint
#' @param query list object to be saved in bfabric.
#'
#' @return
#' @export
save <- function(login, webservicepassword, endpoint = 'workunit', query){
  stopifnot(isFALSE(is.null(login)), isFALSE(is.null(webservicepassword)))
  rv <- POST('http://localhost:5000/s',
             body = toJSON(
               list(
                 login = login,
                 webservicepassword = webservicepassword,
                 endpoint = endpoint,
                 query = query
               ),
               encode = 'json'
             ))


  rv <- content(rv)

  return(rv$res)
}

saveResource <- function(login,
                         webservicepassword,
                         workunitid,
                         content,
                         name){

  rv <- POST('http://localhost:5000/s',
             body = toJSON(
               list(
                 login = login,
                 webservicepassword = webservicepassword,
                 endpoint = 'resource',
                 query = list(
                   'name' = name,
                   'workunitid' = workunitid,
                   'base64' = content
                 )
               ),
               encode = 'json'
             ))

  rv <- content(rv)

}

#' upload resouce to bfabric
#'
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword
#' @param projectid a project or order id
#' @param applicationid the application id
#' @param status 'pending', 'failed', or 'available'
#' @param workunitname the workunit name
#' @param resourcename the reosurce name
#' @param file_content a BLOB containing the content
#'
#' @return
#' @export bfabric_upload_file
bfabric_upload_file <- function(login,
                        webservicepassword,
                        projectid = 1000,
                        applicationid = 217,
                        status,
                        description = '',
                        inputresource = NULL,
                        workunitname = 'MaxQuant result',
                        resourcename = 'MaxQuant report',
                        file_content = NULL) {

  message(workunitname)
  message(resourcename)
  wu <-
    createWorkunit(
      login = login,
      webservicepassword = webservicepassword,
      containerid = projectid,
      inputresource = inputresource,
      applicationid = applicationid,
      name = workunitname,
      status = 'pending',
      description = description
    )


  r <- saveResource(login, webservicepassword, workunitid = wu[[1]]$`_id`,
                    content = file_content, name = resourcename)

  wu[[1]]$`_id`
}



#' reads Custom Attributes of a workunit
#'
#' @param login login
#' @param webservicepassword webservicepassword
#' @param workunitid a valid bfabric workunit id
#'
#' @return a \code{data.frame} containing columns sampleId , resourceId, and
#' iff exisiting a merged table of the Custom Attributes.
#'
#' @export readCustomAttributes
readCustomAttributes <- function(login, webservicepassword, workunitid = 202570){

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
