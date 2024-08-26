#R
## 2024-07-04 Clauda Fortes / Christian Panse
## 2024-07-11 Martina

.toHystar <- function(x, file='file.xml'){
	data.frame(
	    Position = x$Position |> stringr::str_replace("^", "S") |> stringr::str_replace(":", "-") |> stringr::str_replace(",", ""),
	    SampleID = sprintf("%s.d", x$"File Name"),
	    SampleComment = "",
	    Volume = 1,
	    DataPath = x$Path |> stringr::str_replace("QEXACTIVEHF_2", "TIMSTOFFLEX_1"),
	    SuperMethod = "",
	    ResultDatafile = sprintf("%s\\%s.d", x$Path, x$"File Name"),
	    ACQEND_EXECUTE = "D:\\FGCZ\\BioBeamer\\biobeamer.bat"
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




# Metabolomics ========================================

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

.pooledQC <- function(x, plateId = "Y", QCrow = "H", mode = ""){
  #plateId <- x$Position[nrow(x)] |> substr(1,1)
  data.frame(matrix(NA, ncol = ncol(x), nrow = 3)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_poolQC%s", currentdate, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 8)
  pool$`Sample Name`[1] <- sprintf("poolQC%s", mode)
  
  pool[2, "File Name"] <- sprintf("%s_@@@_150mix%s", currentdate, mode)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[2] <- sprintf("150mix%s", mode)
  
  pool[3, "File Name"] <- sprintf("%s_@@@_blank%s", currentdate, mode)
  pool$Position[3] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[3] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

.clean <- function(x, plateId = "Y", QCrow = "H", mode = ""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_blank%s", currentdate, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId,QCrow, 1)
  pool$`Sample Name`[1] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}


.pooledQCDil <- function(x, plateId = "Y", QCrow = "H", mode = ""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 9)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  for (i in 1:7){
    pool[i, "File Name"] <- sprintf("%s_@@@_pooledQCDil%d%s", currentdate, i, mode)
    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }
  
  pool[8, "File Name"] <- sprintf("%s_@@@_150mix%s", currentdate,mode)
  pool$Position[8] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[8] <- sprintf("150mix%s", mode)
  
  pool[9, "File Name"] <- sprintf("%s_@@@_blank%s", currentdate,mode)
  pool$Position[9] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[9] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

#' Vanquish plate number parser
#' 
#' @param x character position string
#' 
#' @value position string with a valid Vanquish plate numbering 
#' 
#' @example .parsePlateNumber("1:A4")
.validatePlateNumber <- function(x){ 
  L <- c("Y", "R", "B", "G")
  pn <- substr(x, 1, 1) 
  if (pn %in% (1:4 |> as.character())) return(L[as.integer(pn)])
  else if (pn %in% L) return(pn)
  else stop("Invalid plate number", x)
}

.parsePlateNumber <- function(x){
  .validatePlateNumber(x)
  
  sprintf("%s%s", .validatePlateNumber(x), substr(x, 2, 5)) -> rv
  rv
}

#' qconfigMetabolomics
#'
#' @param df 
#' 
#' @describeIn
#' as defined my MZ
#' @return data.frame
#' 
#' 
## TODOs(cp):
## 1. take clean dil qcs only from plateId H?
## 2. insert tube ID.
## 3. dir for instrument method
qconfigMetabolomicsPlateXCalibur <- function(x, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
                  "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore H row
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  message(x$Path[1])
  im <- paste0(x$Path[1], "\\methods\\")

  # in between
  x |> .insertSample(howOften = 22, sampleFUN = .pooledQC, path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x

  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x

  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  #x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}

qconfigMetabolomicsVialXCalibur <- function(x, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")

  x |> .insertSample(howOften = 22, sampleFUN = .pooledQC, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x

  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  x |> .insertSample(where = (nrow(x) + 1),
                     sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x

  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
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
  
  p$"File Name" <- sprintf("%s_C%s_@@@_S%d%s_%s",
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
  p$"File Name" <- sprintf("%s_C%s_@@@_S%d%s_%s",
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
