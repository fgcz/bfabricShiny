#R
## 2024-07-04 Clauda Fortes / Christian Panse

.toHystar <- function(x, xmlfile='file.xml'){
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
      
    message(paste0("Saving XML to file '", xmlfile, "' ..."))
    rvSave <- XML::saveXML(xml$value(), file = xmlfile, encoding = "utf-8")
    print(rvSave)

}

.replaceRunIds <- function(x){
	for (i in 1:nrow(x)){
		rn <- sprintf("_%03d_", i)
		x$`File Name`[i] <- stringr::str_replace(x$`File Name`[i], "_@@@_", rn)

	}

	x
}


# Proteomics ========================================

qconfigEVOSEP6x12x8Hystar <- function(df){

	Y <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
	
	currentdate <- format(Sys.time(), "%Y%m%d")
	output <- data.frame(matrix(ncol = 8, nrow = 0))
	colnames(output) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")

	clean <- data.frame(matrix(ncol = 8, nrow = 0))
	cleanAutoQC03 <- data.frame(matrix(ncol = 8, nrow = 0))

	colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")

	colnames(clean) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
	clean <- c("Clean", df$Path[1], "5:X:X", 1, "FGCZ", "clean", "clean", "clean")
	cleancount <- 1
	cleancountx <- 1
	cleancounty <- 1

	colnames(cleanAutoQC03) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
	autoQC03count <- 1
	autoQC03countx <- 1
	autoQC03county <- 1

        ## initial clean and autoQC03dia
	#clean <- c(sprintf("%s_%03d_clean", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
	#cleancountx <- cleancountx + 1
	#cleancount <- cleancount + 1
	#output <- rbind(output, clean)

	#autoQC03 <- c(sprintf("%s_%03d_autoQC03dia", currentdate, autoQC03countx), df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx), 1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
	#autoQC03countx <- autoQC03countx + 1
	#autoQC03count <- autoQC03count + 1
	#output <- rbind(output, autoQC03)
	for (i in 1:nrow(df)){
	  output <- rbind(output, df[i, ])
	  
	  if(i %% 12 == 0 && i %% 48 == 0) {
	    clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
	    cleancountx <- cleancountx + 1
	    cleancount <- cleancount + 1
	    output <- rbind(output, clean)
	    
	    autoQC03 <- c(sprintf("%s_@@@_autoQC03dia_%02d", currentdate, autoQC03countx), df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx), 1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
	    autoQC03countx <- autoQC03countx + 1
	    autoQC03count <- autoQC03count + 1
	    output <- rbind(output, autoQC03)
	  } else if (i %% 12 == 0) {
	    clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
	    cleancountx <- cleancountx + 1
	    cleancount <- cleancount + 1
	    output <- rbind(output, clean)
	  }
	  
	  if (cleancountx > 12){
	    cleancountx <- 1
	    cleancounty <- cleancounty + 1
	  }
	  
	  if (autoQC03countx > 12){
	    autoQC03countx <- 1
	    autoQC03county <- autoQC03county + 1
	  }
	  
	}
	output 
}


# Metabolomics ========================================

.insertSample <- function(x, where = NA, howOften = round(nrow(x)/2), sample = NA, path=NA){
  
 # if (is.na(sample)){
#    stop("No sample name provided")
 # }
  
  output <- data.frame(matrix(ncol = ncol(x), nrow = 0))
  colnames(output) <- colnames(x)
  
  if (is.na(where)){
    for (i in 1:nrow(x)){
      if (i %% howOften == 0){
        rbind(output, sample) -> output
      }
      rbind(output, x[i, ]) -> output
    }
  }else if (where == 0){
    rbind(sample, x) ->  output
  }else if (where > nrow(x)){
    rbind(x, sample) ->  output
  }else{stop("Invalid arguments")}
  
  output$Path <- path
  output
}

.pooledQC <- function(x, plateId = "Y"){
  plateId <- x$Position[nrow(x)] |> substr(1,1)
  data.frame(matrix(NA, ncol = ncol(x), nrow = 3)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, 1] <- sprintf("%s_@@@_poolQC", currentdate)
  pool$Position[1] <- sprintf("%s:H%d", plateId, 3)
  
  pool[2, 1] <- sprintf("%s_@@@_150mix", currentdate)
  pool$Position[2] <- sprintf("%s:H%d", plateId, 2)
  
  pool[3, 1] <- sprintf("%s_@@@_clean", currentdate)
  pool$Position[3] <- sprintf("%s:H%d", plateId, 1)
  pool$`Sample Name`[3] <- sprintf("clean")
  
  pool$`Inj Vol` <- 3.5
  pool
}

.clean <- function(x, plateId = "Y"){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, 1] <- sprintf("%s_@@@_clean", currentdate)
  pool$Position[1] <- sprintf("%s:H%d", plateId, 1)
  pool$`Sample Name`[1] <- sprintf("clean")
  
  pool$`Inj Vol` <- 3.5
  pool
}


.pooledQCDil <- function(x, plateId = "Y"){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 8)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  for (i in 1:7){
    pool[i, 1] <- sprintf("%s_@@@_pooledQCDil%d", currentdate, i)
    pool$Position[i] <- sprintf("%s:H%d", plateId, i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d", i)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }

  pool[8, 1] <- sprintf("%s_@@@_clean", currentdate)
  pool$Position[8] <- sprintf("%s:H%d", plateId, 1)
  pool$`Sample Name`[8] <- "clean"
  
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
qconfigMetabolomics <- function(x){
  colnames(x) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
                   "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  
  # ignore H row
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  message(x$Path[1])
  im <- paste0(x$Path[1], "\\methods\\")
  
  
  x |> .insertSample(howOften = 24, sample = .pooledQC(x), path = x$Path[1]) -> x
  
  x |> .insertSample(where = 0, sample = .pooledQCDil(x), path = x$Path[1]) -> x
  x |> .insertSample(where = 0, sample = .clean(x), path = x$Path[1]) -> x
  x |> .insertSample(where = 0, sample = .clean(x), path = x$Path[1]) -> x
  
  x |> .insertSample(where = (nrow(x) + 1), sample = .pooledQCDil(x), path = x$Path[1]) -> x
  
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  
  x
}

ttt <- function(){
  load("/tmp/mx.RData")
  x |> qconfigMetabolomics() |> .replaceRunIds()
}
  
