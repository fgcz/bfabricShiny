#R
## using fastAPI
## Christian Panse and Leonardo Schwarz
## 20251215
## 20251216

.callRestProxy  <- function(auth, params, posturl, posturlsuffix){
  
  paste0(posturl, posturlsuffix) -> posturl 
  message(posturl)
  message(posturlsuffix)
  if (interactive()) {
    paste0(".callRestusing '", posturl, "' as posturl ...") |> message()
    }
  
  Sys.time() -> start_time
  
  httr::POST(posturl, body = list(auth = auth,
                                  params = params),
             encode = 'json') -> query_result
  Sys.time() -> end_time
  
  paste0(round(difftime(end_time, start_time, units = 'secs'), 2), " [s].") -> diff_time_msg
  httr::content(query_result) -> rv
  
  if ('error' %in% names(rv)){
    stop(paste0("B-Fabric errorreport: ", rv$error))
  }
  
  if (interactive()) {
    message(paste0("read query time: ", diff_time_msg))
  }
  
  rv
}
#=======read======
#' read function which supports pages
#' 
#' @param login bfabric login
#' @param webservicepassword bfabric webservicepassword,
#' visible when you check your user details in the bfabric system.
#' @param endpoint the endpoint, e.g., \code{'sample'}
#' @param query e,g, \code{list(containerid = 3000)}
#' @param posturl where the flask server is working
#' @param maxitems max number of supported pages to 
#' @param updateProgress a callback function for writing log output, e.g.,
#' using a \code{\link[shiny]{Progress}} object,
#' see also \url{https://shiny.rstudio.com/articles/progress.html}.
#' @param page define requested page, default is 1
#' @param posturlsuffix defines the method to use, e.g.,
#' read. also, save should work
#' @author MdE/CP 2023-03; CP 2024-12-24; LS 2025-12-15
#' @export
#' @examples
#' 
#' \dontrun{
#' bfabricShiny::read(login,
#'   webservicepassword,
#'   endpoint = 'sample',
#'   query = list('containerid' = 34777),
#'   posturl = bfabricposturl,
#'   maxitems = 1000) -> rv
#'   }
read <- function(login = NULL,
                 webservicepassword = NULL,
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
  
  
  .callRestProxy(auth = list(login = login,
                             webservicepassword = webservicepassword),
                 params = list(
                   endpoint = endpoint,
                   query = query,
                   idonly = idonly,
                   page_offset = offset,
                   page_max_results = maxitems), 
                 posturl = posturl,
                 posturlsuffix = posturlsuffix) -> rv
  
  
  
  list(res = rv)
}



#R
## using fastAPI
## Christian Panse and Leonardo Schwarz
## 20251215



# =====createWorkunit=======
#' Generate a workunit and upload a list of resource(s) (file)
#' to the internal bfabric storage
#'
#' @inheritParams read
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
#' @return returns a nested list containing the workunit
#'
#' object returned by the save method.
#' @importFrom base64enc base64encode
#' @importFrom tools file_ext
#'
#' @author Christian Panse <cp@fgcz.ethz.ch> 2016-2023, MdE 2023-03-17, LS, 2025-12-15
#'
#' @export
#'
#' @examples
#'
#' ## create a csv file
#' tf <- tempfile(fileext = ".csv", pattern = "R-iris-data-")
#' write.csv(iris, file = tf)
#'
#'
#' createWorkunit(
#'   login = login,
#'   webservicepassword = webservicepassword,
#'   posturl = bfabricposturl,
#'   containerid = 3000,
#'   applicationid = 212,
#'   workunitname = "TEST Xerces",
#'   files = list(tf),
#'   description = "ignore",
#'   posturlsuffix = "/create/workunit/v1"
#' ) -> rv
#'
#' print(rv)
createWorkunit <- function(
    login = NULL,
    webservicepassword = NULL,
    posturl = "http://127.0.0.1:5000",
    containerid = 3000,
    applicationid = 212,
    workunitname = "bfabricShiny result",
    parameters = structure(list(), names = character(0)),
    files = structure(list(), names = character(0)),
    inputresourceid = list(),
    links = structure(list(), names = character(0)),
    description = "",
    posturlsuffix = "create/workunit/v1") {
  stopifnot(
    isFALSE(is.null(login)),
    isFALSE(is.null(webservicepassword))
    # isFALSE(is.null(files)),
    # isFALSE(is.null(posturl)),
    # file.exists(file)
  )

  # stopifnot(status %in% c('AVAILABLE', 'FAILED', 'PENDING'))

  lapply(files, FUN = function(fn) {
    readBin(fn, "raw", file.info(fn)[1, "size"]) |>
      base64enc::base64encode(tools::file_ext(fn))
  }) -> fileContentList

  names(fileContentList) <- sapply(files, basename)
  # browser()
  ## update description
  sprintf(
    "%s\n\n
Generated by Rpkg https://github.com/fgcz/bfabricShiny/ version %s.
System information: %s\n
To help us funding further development, please cite:
(bfabricSiny) PMID: 36073980 DOI: 10.1515/jib-2022-0031",
    description,
    packageVersion("bfabricShiny"),
    paste(Sys.info(), collapse = ", ")
  ) -> description

  .callRestProxy(
    auth = list(
      login = login,
      webservicepassword = webservicepassword
    ),
    params = list(
      container_id = containerid,
      application_id = applicationid,
      workunit_name = workunitname,
      parameters = parameters,
      resources = fileContentList,
      links = links,
      description = description,
      input_resource_ids = inputresourceid
    ),
    posturl = posturl,
    posturlsuffix = posturlsuffix
  ) -> rv

  list(workunit = rv)
}
