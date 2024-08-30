#R
## 2024-07-04 Clauda Fortes / Christian Panse



.insertSample <- function(x, where = NA, howOften = round(nrow(x)/2),
                          sampleFUN = NA, path=NA, ...){
  output <- data.frame(matrix(ncol = ncol(x), nrow = 0))
  colnames(output) <- colnames(x)
  
  if (is.na(where)){
    for (i in 1:nrow(x)){
      if (i %% howOften == 0){
        plateId <- output$Position[nrow(output)] |> substr(1,1)
        rbind(output, sampleFUN(x, plateId=plateId, ...)) -> output
      }
      rbind(output, x[i, ]) -> output
    }
  }else if (where == 0){
    plateId <- x$Position[1] |> substr(1,1)
    rbind(sampleFUN(x, plateId = plateId, ...), x) ->  output
  }else if (where > nrow(x)){
    plateId <- x$Position[nrow(x)] |> substr(1,1)
    rbind(x, sampleFUN(x, plateId = plateId, ...)) ->  output
  }else{stop("Invalid arguments")}
  
  output$Path <- path
  output
}


## TODO(cp): check replacemet for QHF2!!
.toHystar <- function(x, file='file.xml'){
	data.frame(
	    Position = x$Position |>
	      stringr::str_replace("^", "S") |>
	      stringr::str_replace(":", "_") |>
	      stringr::str_replace(",", ""),
	    SampleID = sprintf("%s", x$"File Name"),
	    SampleComment = "",
	    Volume = 1,
	    DataPath = x$Path |> stringr::str_replace("QEXACTIVEHF_2", "TIMSTOFFLEX_1"),
	    SuperMethod = "",
	    ResultDatafile = sprintf("%s\\%s.d", x$Path, x$"File Name"),
	    ACQEND_EXECUTE = "D:\\FGCZ\\Biobeamer\\biobeamer.bat"
	    ) -> df


    xml <- XML::xmlTree()
    xml$addTag("SampleTable")
    dump <- lapply(1:nrow(df), FUN = function(i) xml$addTag("Sample", close=TRUE, attrs=df[i, ]))
      
    message(paste0("Saving XML to file '", file, "' ..."))
    rvSave <- XML::saveXML(xml$value(), file = file, encoding = "utf-8")
    print(rvSave)

}

.replaceRunIds <- function(x){
	for (i in 1:nrow(x)){
		rn <- sprintf("_%03d_", i)
		x$`File Name`[i] |>
		  stringr::str_replace("_@@@_", rn) -> x$`File Name`[i]
		
		x$`File Name`[i] |>
		  stringr::str_replace("#", "_") -> x$`File Name`[i]

	}

	x
}



  
#' @examples
#' .readSampleOfContainer(35464, login, webservicepassword, bfabricposturl)
.readSampleOfContainer <- function(containerID, login, webservicepassword, posturl){
  res <- bfabricShiny::read(login, webservicepassword, posturl = posturl,
                            endpoint = "sample",
                            maxitems = 2000,
                            query = list('containerid' = containerID))$res
  
  data.frame('Sample Name' = sapply(res, function(x)x$name),
             `Sample ID`= sapply(res, function(x)x$id),
             "Tube ID" = sapply(res, function(x)x$tubeid),
             stringsAsFactors = FALSE) -> df
  
  colnames(df) <- c("Sample Name", "Sample ID", "Tube ID")
  
  df[order(df$`Sample ID`), ]
}

.deriveVialPositionVanquish <- function(n = 10){
  X <- c("A", "B", "C", "D", "E")
  nY <- 9
  P <- c("Y", "R", "B", "G")
  counterPlate <- 1
  counterX <- 0
  counterY <- 0
  
  pos <- rep("", n)
  for (i in 1:n){
    pos[i] <- sprintf("%s:%s%d", counterPlate, X[counterX + 1], (counterY+1))
    
    counterY <- counterY + 1
    
    if (i %% nY == 0){
      counterX <- counterX + 1
      counterY <- 0
    }
    if (i %% (length(X) * nY) == 0){
      counterPlate <- counterPlate + 1 
      counterX <- 0
      counterY <- 0
    }
     
  }
  pos
} 


.extractSampleIdfromTubeID <- function(containerid, tid){
  sapply(tid, FUN = function(x){
    pattern = sprintf("%s/[0-9]+", containerid)
    if(grepl(pattern, x)){
      x |> stringr::str_replace("/", "-")
    }else{
      containerid
    }
  })
}

.composePlateSampleTable <- function(p,
                                     orderID = 34843,
                                     area = "Metabolomics",
                                     mode = "",
                                     instrument = 'ASTRAL_1',
                                     user = 'cpanse',
                                     injVol = 3.5, 
                                     plateCounter = 0,
                                     randomization = 'plate'){
  format(Sys.time(), "%Y%m%d") -> currentdate
  
  p$"File Name" <- sprintf("%s_@@@_C%s_S%d%s_%s",
                           currentdate,
                           .extractSampleIdfromTubeID(orderID, p$`Tube ID`),
                           p$"Sample ID",
                           mode,
                           p$"Sample Name")
  
  p$"Path" <- paste0("D:\\Data2San\\p", orderID, "\\", area,
                     "\\", instrument, "\\",
                     user, "_", currentdate)
  p$"Sample Name" <- paste0(p$"Sample Name", mode)
  
  p$Position <- sprintf("%d:%s", plateCounter, p$Position)
  
  p$"Inj Vol" <- injVol
  
  p$"L3 Laboratory" <- "FGCZ"
  
  p$"Instrument Method" <- sprintf("%s\\methods\\", p$Path)
  
  if (randomization == "plate"){
    set.seed(872436)
    p[sample(nrow(p)), ] -> p
  }
  
  plateCounter <<- plateCounter + 1
  p[, columnOrder]
}


#' @examples
#' .readSampleOfContainer(34843, login, webservicepassword, bfabricposturl) |> .composeSampleTable(orderID = 34843, randomization = TRUE) -> x
#' x|> qconfigMetabolomics()|> .replaceRunIds() -> xx
.composeSampleTable <- function(x, orderID = 34843,
                                area = "Metabolomics",
                                mode = "",
                                instrument = 'ASTRAL_1',
                                user = 'cpanse',
                                injVol = 3.5, 
                                randomization = TRUE){
  
  format(Sys.time(), "%Y%m%d") -> currentdate
  p <- x
  p$"File Name" <- sprintf("%s_@@@_C%s_S%d%s_%s",
                           currentdate,
                           orderID,
                           p$"Sample ID",
                           mode,
                           p$"Sample Name")
  p$"Path" <- paste0("D:\\Data2San\\p", orderID, "\\", area,
                     "\\", instrument, "\\",
                     user, "_", currentdate)
  p$"Sample Name" <- paste0(p$"Sample Name", mode)
  p$Position <- .deriveVialPositionVanquish(n = nrow(p))
  p$"Inj Vol" <- injVol
  p$"L3 Laboratory" <- "FGCZ"
  p$"Instrument Method" <- sprintf("%s\\methods\\", p$Path)
  
  if (randomization){
    split(1:nrow(p), substr(p$Position, 1, 1)) |>
      lapply(function(idx){p[idx[sample(length(idx))], ]}) |>
      Reduce(f = rbind) -> p
  }
  p$Position |> sapply(FUN = .parsePlateNumber) -> p$Position
   p
}
