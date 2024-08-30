

# Metabolomics ========================================
## 2024-07-11 Martina
.pooledQC <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  #plateId <- x$Position[nrow(x)] |> substr(1,1)
  data.frame(matrix(NA, ncol = ncol(x), nrow = 3)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_poolQC%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 8)
  pool$`Sample Name`[1] <- sprintf("poolQC%s", mode)
  
  pool[2, "File Name"] <- sprintf("%s_@@@_C%s_150mix%s", currentdate, containerid, mode)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[2] <- sprintf("150mix%s", mode)
  
  pool[3, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[3] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[3] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

.clean <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId,QCrow, 1)
  pool$`Sample Name`[1] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}


.pooledQCDil <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 9)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  for (i in 1:7){
    pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQCDil%d%s", currentdate, containerid, i, mode)
    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }
  
  pool[8, "File Name"] <- sprintf("%s_@@@_C%s_150mix%s", currentdate, containerid, mode)
  pool$Position[8] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[8] <- sprintf("150mix%s", mode)
  
  pool[9, "File Name"] <- sprintf("%s_@@@_C%s+blank%s", currentdate, containerid, mode)
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
qconfigMetabolomicsPlateXCalibur <- function(x, howOften = 22, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore H row
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  message(x$Path[1])
  im <- paste0(x$Path[1], "\\methods\\")
  
  # in between
  x |> .insertSample(howOften = howOften, sampleFUN = .pooledQC, path = x$Path[1], ...) -> x
  
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

qconfigMetabolomicsVialXCalibur <- function(x, howOften = 22, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften, sampleFUN = .pooledQC, path = x$Path[1], ...) -> x
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
