#R

# READ ==========================
#' Read sample
#' @param samplelist list of sample ids
#' @value data.frame containing: sample name, filename, sample ID
#' @export
readSample <- function(sampleIds, login, webservicepassword, posturl){
  res <- bfabricShiny::read(login, webservicepassword, posturl = posturl,
                            endpoint = "sample",
                            query = list('id' = list(sampleIds)))[[1]]
  
  data.frame("SampleName" = sapply(res, function(x)x$name),
             "SampleID" = sapply(res, function(x)x$id),
             "TubeID" = sapply(res, function(x)x$tubeid),
             stringsAsFactors = FALSE) -> df
  
 
  df
}


#' Read plate
#' @param plateid plate id
#' @description
#' extracts plate and sample information from the plate id
#' @value data.frame containing: sample ID, sample name, tube ID, position
#' @examples readPlate(plateid = 2616, login, webservicepassword, bfabricposturl)
#' @export
readPlate <- function(plateid, login, webservicepassword, posturl) {
  message(sprintf("Reading plate %s ...", plateid))
  
  res <- bfabricShiny::read(login, webservicepassword, posturl = posturl,
                            endpoint = "plate",
                            query = list('id' = plateid))[[1]]
  
  
  res[[1]]$sample |>
    lapply(FUN = function(s){
      data.frame(
        "SampleID" = s$id,
        Position = s$`_gridposition`
      )
    }) |> Reduce(f = rbind) -> dfPlate
  
  

  readSample(dfPlate$"SampleID", login, webservicepassword, posturl) -> dfSample
  # browser()
  stopifnot(nrow(dfPlate) == nrow(dfSample))
  merge(dfSample, dfPlate, by = 'SampleID') -> df
  colnames(df) <- c("Sample ID" ,  "Sample Name", "Tube ID", "Position")
  df
}

