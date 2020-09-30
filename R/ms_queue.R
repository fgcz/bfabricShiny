#R
# Autors: Christian Trachsel / Christian Panse / Witold Wolski
#
#generate test data set ----

.test_data_single <- function(){
  extract.name <- "Sample_1"
  extract.id <- 1
  extract.Condition <- "Control"
  data.frame(extract.name, extract.id, extract.Condition, containerid = 7320)
}

.test_data_medium <- function(){
  extract.name <- c(paste("Sample", 1:20, sep = "_"))
  extract.id <-  1:20
  extract.Condition <- c(rep("Control", 4), rep("Ampicillin", 4), rep("Kanamycin", 4), rep("Less", 3), rep("More", 5))
  data.frame(extract.name, extract.id, extract.Condition, containerid = rep(7320,20))
}


.test_data_medium_random <- function(){
  extract.name <- c(paste("Sample", 1:20, sep = "_"))
  extract.id <- 1:20
  condition <- c(rep("Control", 4), rep("Ampicillin", 4), rep("Kanamycin", 4), rep("Less", 3), rep("More", 5))
  extract.Condition <- condition[sample(1:20)]
  data.frame(extract.name, extract.id, extract.Condition,containerid = rep(7320,20))
}

.test_data_large <- function(){
  extract.name <- c(paste("Sample", 1:80, sep = "_"))
  extract.id <- 1:80
  extract.Condition <- c(rep("Control", 16), rep("Ampicillin", 16), rep("Kanamycin", 16), rep("Less", 12), rep("More", 20))
  data.frame(extract.name, extract.id, extract.Condition,containerid = rep(7320,20))
}

#'
#'@examples
#'.test_data_order()
.test_data_order <- function(){
  res <- data.frame(
    extract.name =  c(paste("Sample", 1:10, sep = "_"),paste("XYZ", 1:10, sep = "_") ),
    extract.id = c(1:10,41:50),
    extract.Condition = c(rep("Control", 16), rep("Ampicillin", 16), rep("Kanamycin", 16), rep("Less", 12), rep("More", 20)),
    containerid = c(rep(7239,10), rep(3111,10)))
  return(res)

}

.getInstrument <- function(){
  list(
    QEXACTIVE_2 = 'Xcalibur',
    QEXACTIVEHF_2 = 'Xcalibur',
    QEXACTIVEHF_4 = 'Xcalibur',
    QEXACTIVEHFX_1 = 'Xcalibur',
    FUSION_1 = 'Xcalibur',
    FUSION_2 =  'Xcalibur',
    EXPLORIS_1 = 'Xcalibur',
    LUMOS_1 = 'Xcalibur',
    LUMOS_2 = 'Xcalibur'
  )}

# file Extensions -----
.getInstrumentSuffix <- function(){
  list(
    VELOS_1 = 'RAW',
    VELOS_2 = 'RAW',
    G2HD_1 = 'wiff',
    QTRAP_1 = 'wiff',
    TSQ_1 = 'RAW',
    TSQ_2 = 'RAW',
    QEXACTIVE_1 = 'raw',
    QEXACTIVE_2 = 'raw',
    QEXACTIVE_3 = 'raw',
    FUSION_1 = 'raw',
    FUSION_2 = 'raw',
    QEXACTIVEHF_1 = 'raw',
    QEXACTIVEHF_2 = 'raw',
    QEXACTIVEHF_4 = 'raw',
    QEXACTIVEHFX_1 = 'raw',
    LUMOS_1 = 'raw',
    LUMOS_2 = 'raw',
    EXPLORIS_1 = 'raw',
    IMSTOF_1 = 'h5'
  )
}


# Tray creation ----
#'
#' @examples
#' get_tray()
get_tray_2_48_plates <- function(start.row = 1
                     , start.col = 'A'
                     , start.plate = 1){
  makeid = paste(start.plate, start.col, start.row, sep = "~" )
  plate <- data.frame(
    cols = rep(LETTERS[1:6], each = 8),
    rows = rep(c(1:8), times = 6))
  plate1 <- plate
  plate2 <- plate
  plate1$plate <- 1
  plate2$plate <- 2
  tray <- rbind(plate1[1:45,], plate2[1:45,])

  tray <- tidyr::unite(tray, id, c("plate", "cols", "rows"), sep="~" , remove=FALSE)
  start <- which(makeid == tray$id)
  tray <- tray[start:nrow(tray),]
  return(tray)
}

#'
#' @examples
#' bfabricShiny:::get_tray_2_48_plates_nextpos(12)
#'
get_tray_2_48_plates_nextpos <- function(n, startpos = list(row = 1, col = "A", plate = 1)){
  positions <- get_tray_2_48_plates(start.row = 1, start.col = "A", start.plate = 1)
  end.pos <- positions[n+1,]
  return(end.pos)
}


#'
#' @examples
#'
#' all.equal(get_tray_waters(),bfabricShiny:::.waters())
#' bfabricShiny:::get_tray_waters(3, 'A', 1)
get_tray_waters <- function(start.row = 1
                            , start.col = 'A'
                            , start.plate = 1){
  tray <- get_tray_2_48_plates(
    start.row = start.row
    , start.col = start.col
    , start.plate = start.plate)
  tray <- dplyr::mutate(tray,pos = sprintf("\"%01d:%s,%01d\"",
                                           plate,
                                           cols,
                                           rows))
  res <- c('waters', list(tray$pos),  '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"' )
  return(res)
}

#' method eksigent
#' @examples
#' r <- bfabricShiny:::.waters()
#' r
#' all.equal(r[[2]][1:5] , c("\"1:A,1\"", "\"1:A,2\"", "\"1:A,3\"", "\"1:A,4\"", "\"1:A,5\""))
#'
.waters <- function(){
  tray1 <- rep(1, times = 46) %>%
    paste(rep(LETTERS[1:6], each = 8), sep = ":") %>%
    paste(rep(1:8, times = 6), sep = ",") %>%
    paste0('"', ., '"')
  tray2 <- rep(2, times = 46) %>%
    paste(rep(LETTERS[1:6], each = 8), sep = ":") %>%
    paste(rep(1:8, times = 6), sep = ",") %>%
    paste0('"', ., '"')
  pos <- c(tray1[1:45], tray2[1:45])
  res <- c('waters', list(pos),  '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"' )
  return(res)
}


#'
#' @examples
#' bfabricShiny:::.eksigent()
.eksigent <- function(){
  tray1 <- rep(2, times = 46) %>%
    paste(rep(LETTERS[1:6], each = 8), sep = "") %>%
    paste(rep(sprintf("%02d", c(1:8)), times = 6), sep = "")
  pos <- tray1[1:45]
  res <- c('eksigent', list(pos), '2F08', '2F07', '2F07','2F06')
  return(res)
}


#' method eksigent
#' @examples
#' bfabricShiny:::.easylc()
.easylc <- function(){
  tray1 <- paste(rep(LETTERS[1:6], each = 8), rep(1:8, times = 6), sep = "")
  pos <- tray1[1:45]
  res <- c('easylc', list(pos), 'F8', 'F7', 'F7','F6' )
  return(res)
}



#' Define HPLC, autoQC01, autoQC02, autoQC4L, clean ----
#' list elements are: 1) HPLC, 2) sample positions, 3) autoQC01 position, 4) autoQC02 position, 5) autoQC4L position, 6) clean position
#' @examples
#'
#' bfabricShiny:::getHPLCparameter()[["LUMOS_2"]]
#' bfabricShiny:::getHPLCparameter(3,"D",2)[["LUMOS_2"]]
getHPLCparameter <- function(row = 1, col ="A", plate = 1){
  list(VELOS_1 = .eksigent(),
       VELOS_2 = .eksigent(),
       G2HD_1 = .waters(),
       QTRAP_1 = .eksigent(),
       TSQ_1 = .eksigent(),
       TSQ_2 = .eksigent(),
       QEXACTIVE_1 = get_tray_waters(row, col, plate),
       QEXACTIVE_2 = get_tray_waters(row, col, plate),
       QEXACTIVE_3 = .easylc(),
       FUSION_1 = .easylc(),
       FUSION_2 = get_tray_waters(),
       QEXACTIVEHF_1 = get_tray_waters(row, col, plate),
       QEXACTIVEHF_2 = get_tray_waters(row, col, plate),
       QEXACTIVEHF_4 = get_tray_waters(row, col, plate),
       QEXACTIVEHFX_1 = get_tray_waters(row, col, plate),
       LUMOS_1 = get_tray_waters(row, col, plate),
       LUMOS_2 = get_tray_waters(row, col, plate),
       EXPLORIS_1 = get_tray_waters(row, col, plate),
       IMSTOF_1 = .eksigent())
}

#'
#'@examples
#' bfabricShiny:::getQCsample()
#'
getQCsample <- function(){
  list(extract.name = c('autoQC01', 'autoQC02', 'autoQC4L', 'clean'),
       extract.Condition = c(as.integer(NA), as.integer(NA), as.integer(NA), as.integer(NA)),
       extract.Condition = c('autoQC01', 'autoQC02', 'autoQC4L', 'clean'),
       position = c(3, 4, 5, 6))}

#other helper functions ----

.emptydf <- function(){
  res <- data.frame(extract.name = character(),
                    extract.id = integer(),
                    extract.Condition = character(),
                    position = character(),
                    idx = numeric(),
                    stringsAsFactors = FALSE)

  return(res)
}

#' Title
#'
#' @param x
#' @param instrument
#'
#' @return
#' @export
#'
#' @examples
#' bfabricShiny::.tray_position(bfabricShiny:::.test_data_medium(), instrument="LUMOS_2")
#' bfabricShiny::.tray_position(bfabricShiny:::.test_data_medium(),
#' startpos = list(row = 4, col = "A", plate = 1),
#'  instrument="LUMOS_2")
.tray_position <- function(queue,
                           startpos = list(row = 1, col = "A", plate = 1),
                           instrument = ""){
  pos.idx <- getHPLCparameter(startpos$row, startpos$col, startpos$plate)[[instrument]][[2]]
  n <- nrow(queue)
  positions.needed <- ceiling(n/46)
  pos <- rep(pos.idx, times = positions.needed)
  pos <- pos[1:n]
  queue$position <- pos
  res <- queue

  return(res)
}


.equal.groups <- function(x){
  empty.line <- data.frame(extract.name = "NA", extract.id = as.integer(NA))
  y <- x %>%
    dplyr::group_by(extract.Condition) %>%
    dplyr::summarise(n()) %>%
    dplyr::pull("n()")
  condition.names <- data.frame(extract.Condition = unique(x$extract.Condition))
  lines.needed <-  max(y) - y
  df <- empty.line[rep(row.names(empty.line), dplyr::n_distinct(x$extract.Condition)), ] %>%
    dplyr::mutate(extract.Condition = unique(x$extract.Condition)) %>%
    dplyr::arrange(extract.Condition) %>%
    dplyr::mutate(IDX = 1:length(y) + 0.1) %>%
    dplyr::mutate(z = lines.needed)
  df <- df[rep(seq_len(nrow(df)), df$z), 1:4]
  res <- x %>%
    dplyr::mutate(randomidx = rnorm(nrow(x))) %>%
    dplyr::arrange(extract.Condition, randomidx) %>%
    dplyr::mutate(IDX = rep(1:length(y), y)) %>%
    dplyr::bind_rows(df) %>%
    dplyr::arrange(IDX)
  res <- res %>% dplyr::select("extract.name", "extract.id", "extract.Condition")

  return(res)
}


#'
#' @examples
#' autoQC01 = "TRUE"
#' QC01o = 4
#' QC01m = 1
#' bfabricShiny:::.autoQC01(20, "FUSION_1", QC01o, QC01m, autoQC01)
#' bfabricShiny:::.autoQC(20)
#' bfabricShiny:::.autoQC(20, autoQCName = "autoQC02", idxoffset = 0.8)
.autoQC <- function(nrow_x, instrument = "FUSION_1",
                    howoften = 4,
                    howmany = 1,
                    autoQCName = "autoQC01",
                    idxoffset = 0.6,
                    position = 3){
  if (howoften == 0) {
    res <- .emptydf()
  } else {
    check <- floor(nrow_x / howoften)
    if (check < 1) {
      qc.inserts <- 1
      qc.idx <- nrow_x
    }else {
      qc.inserts = floor(nrow_x/howoften)
      qc.idx <- howoften*(1:qc.inserts)
    }
    repetitions <- qc.inserts * howmany
    df <- data.frame(extract.name = autoQCName,
                     extract.id = as.integer(NA),
                     extract.Condition = autoQCName,
                     position = unlist(getHPLCparameter()[[instrument]][position]),
                     idx = as.numeric(NA),
                     stringsAsFactors = FALSE)
    res <- df[rep(1, times = repetitions), ]
    res$idx  <-  rep(qc.idx + idxoffset, each = howmany)
  }
  return(res)
}

#QC inserting functions ----
#'
#' @examples
#' autoQC01 = "TRUE"
#' QC01o = 3
#' QC01m = 1
#' bfabricShiny:::.autoQC01(20, "FUSION_1", QC01o, QC01m, autoQC01)
#'
#'
.autoQC01 <- function(nrow_x, instrument, QC01o, QC01m, autoQC01){
  if (autoQC01 == "FALSE") {
    return(.emptydf())
  }else{
    res <- .autoQC(nrow_x,
                   instrument = instrument,
                   howoften = QC01o,
                   howmany = QC01m,
                   autoQCName = "autoQC01",
                   idxoffset = 0.6,
                   position = 3)
    return(res)
  }
}
#'
#' @examples
#' autoQC02 = "TRUE"
#' QC02o = 3
#' QC02m = 1
#' x <- bfabricShiny:::.autoQC02e(20, "FUSION_1", QC02o, QC02m, autoQC02)
#' all.equal(x, xe)
.autoQC02 <- function(nrow_x, instrument, QC02o, QC02m, autoQC02){
  if (autoQC02 == "FALSE") {
    return(.emptydf())
  }else{
    res <- .autoQC(nrow_x,
                   instrument = instrument,
                   howoften = QC02o,
                   howmany = QC02m,
                   autoQCName = "autoQC02",
                   idxoffset = 0.8,
                   position = 4)
    return(res)
  }
  return(res)
}
#'
#' @examples
#' autoQC02 = "TRUE"
#' QC02o = 3
#' QC02m = 1
#' x <- bfabricShiny:::.autoQC4L(20, "FUSION_1", QC02o, QC02m, autoQC02)
#'
.autoQC4L <- function(nrow_x, instrument, QC4Lo, QC4Lm, autoQC4L){
  if (autoQC4L == "FALSE") {
    return(.emptydf())
  }else{
    res <- .autoQC(nrow_x,
                   instrument = instrument,
                   howoften = QC4Lo,
                   howmany = QC4Lm,
                   autoQCName = "autoQC4L",
                   idxoffset = 0.9,
                   position = 4)
    return(res)
  }
}

#'
#' @examples
#' autoQC02 = "TRUE"
#' QC02o = 3
#' QC02m = 1
#' x <- bfabricShiny:::.clean(20,  "FUSION_1", QC02o, QC02m, autoQC02)
#' xe <- bfabricShiny:::.cleane(20,  "FUSION_1", QC02o, QC02m, autoQC02)
#' all.equal(x, xe)
.clean <- function(nrow_x, instrument, cleano, cleanm, clean){
  if (clean == "FALSE") {
    return(.emptydf())
  }else{
    res <- .autoQC(nrow_x,
                   instrument = instrument,
                   howoften = cleano,
                   howmany = cleanm,
                   autoQCName = "clean",
                   idxoffset = 0.4,
                   position = 6)
    return(res)
  }
}

.insert_qc_samples <- function(x, y, z, u, v){
  x <- x %>%
    dplyr::mutate(idx = seq_along(x$extract.name))

  res <- dplyr::bind_rows(x,y,z,u,v) %>%
    dplyr::arrange(idx)

  res[sapply(res, is.factor)] <- lapply(res[sapply(res, is.factor)], as.character)
  cleanup.idx <- max(which(res$extract.name == x$extract.name[nrow(x)]))
  res <- res[1:cleanup.idx,]
  return(res)
}

#'
#' @examples
#' bfabricShiny:::getStartorEndLine()
getStartorEndLine <- function(instrument = "LUMOS_1", method = 1){
  if (is.na(method)) {
    res <-  .emptydf()
    res <- res[, 1:4]
  } else {
    res <- data.frame(extract.name = getQCsample()[[1]][method],
                      extract.id = getQCsample()[[2]][method],
                      extract.Condition = getQCsample()[[3]][method],
                      position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][method]]]),
                      stringsAsFactors = FALSE)
  }
  return(res)
}

#queue start and end ----
#'
#' @examples
#' bfabricShiny:::.gen.start("LUMOS_1", start1 = 1, start2 = NA, start3 = NA)
#'
.gen.start.end <- function(instrument, pos1, pos2, pos3){ #argument names!
  S1 <- getStartorEndLine(instrument, pos1)
  S2 <- getStartorEndLine(instrument, pos2)
  S3 <- getStartorEndLine(instrument, pos3)
  res <- dplyr::bind_rows(S1, S2, S3)
  return(res)
}

#'
#'
#' @examples
#' .gen.start("LUMOS_1", start1, start2 , start3)
#' bfabricShiny:::.gen.end("LUMOS_1", end1, end2 , end3)
#'
.clean_queue <- function(x, instrument, start1, start2, start3, end1, end2, end3){
  start <- .gen.start.end(instrument, start1, start2 , start3)
  end <- .gen.start.end(instrument, end1, end2 , end3)
  res <- dplyr::bind_rows(start, x, end)

  return(res)
}

#basic queue formating function ----

#' .generate_template_base
#'
#' @param x: the sample information (data.frame)
#'
#' @return the original dataframe (data.frame)
#' @export
#'
#' @examples
#'
.generate_template_base <- function(x){
  return(x)
}

#random queue formating function ----

.generate_template_random <- function(x){
  res <- x[sample(nrow(x)), ]
  return(res)
}

#blockrandom queue formating function ----
#'
#'@examples
#'
#' set.seed(3)
#' bfabricShiny:::.generate_template_random_block(bfabricShiny:::.test_data_medium())
#' set.seed(3)
#' bfabricShiny:::.generate_template_random_block(bfabricShiny:::.test_data_medium_random())
.generate_template_random_block <- function(x){
  #TODO:
  #check if more than one condition is present. If not display a warning
  #check if all extract.Conditions are equal sized if not display a warning
  # tab <- as.vector(table(x$extract.Condition))
  #length(unique(tab)) == 1
  #max(table(x$extract.Condition)) - min(table(x$extract.Condition))
  #TODO END
  blocks <- length(unique(x$extract.Condition))
  elements <- max(table(x$extract.Condition))
  res <- x %>%
    bfabricShiny:::.equal.groups()
  res <- dplyr::mutate(res, blockidx = as.vector(replicate(blocks, sprintf("%02d", c(1:elements)))))
  res <- dplyr::arrange(res, blockidx)
  res <-   dplyr::mutate(res, randomidx = as.vector(replicate(elements, sample(1:blocks))))
  res <-   dplyr::arrange(res, blockidx, randomidx)
  res <- dplyr::filter(res, !is.na(extract.id))
  res <- dplyr::select(res, "extract.name", "extract.id", "extract.Condition")

  return(res)
}

#method evaluation queue formating function ----
#'
#' @examples
#'
#' sampleData <- bfabricShiny:::.test_data_medium()
#' que <- bfabricShiny:::.generate_template_method_testing(sampleData)
#' dim(que)
#' stopifnot(all(dim(que) == c(120,5)))
#' que %>% arrange(position)
.generate_template_method_testing <- function(x,
                                              nr.methods = 2,
                                              nr.replicates = 3,
                                              startpos = list(row = 1, col = "A", plate = 1),
                                              instrument = "QEXACTIVEHF_2")
{
  res <- .tray_position(queue = x, startpos = startpos, instrument = instrument)
  res <- res[rep(seq_len(nrow(res)), each = nr.methods), ]

  nMethod <- rep("Method", times = nr.methods)
  nMethodNr <- paste(nMethod, 1:nr.methods, sep = "_")
  res$extract.Condition <- paste(res$extract.Condition,
                                 nMethodNr, sep = "_")


  res <- res[rep(seq_len(nrow(res)), each = nr.replicates ), ]
  res <- res[order(sample(nrow(res))), ]

  return(res)
}

#PRM queue formating function ----
#'
#' @examples
#'
#' sampleData <- bfabricShiny::.test_data_large()
#' que <- bfabricShiny::.generate_template_PRM(sampleData)
#' stopifnot(all(dim(que) == c(160,4)))
#'
#'
.generate_template_PRM <- function(x,
                                   lists = 2,
                                   startpos = list(row = 1, col = "A", plate = 1),
                                   instrument = "LUMOS_2"){
  res <- x[sample(nrow(x)), ]
  res <- .tray_position(res, startpos = startpos, instrument = instrument)

  res <- res[rep(seq_len(nrow(res)), each = lists), ]
  res$extract.Condition <- res$extract.Condition %>%  #attache to extract.Condition
    paste(rep("Targets", times = nrow(res)), sep = "_") %>%
    paste(rep(1:lists, times = nrow(res)/lists), sep = "")

  return(res)
}

#generate queue ----

#' Title
#'
#' @param x
#' @param foldername
#' @param area
#' @param instrument
#' @param username
#' @param pathprefixsep
#'
#' @return
#' @export
#'
#' @examples
#'
.generate_folder_name <- function(
  x,
  foldername,
  area = "Proteomics",
  instrument = "FUSION_1",
  username = "bfabricusername",
  pathprefixsep='/'
){
  n <- nrow(x)
  rundate <- format(Sys.Date(), format = "%Y%m%d")
  out <- paste(username, rundate, sep = "_")
  if (foldername != '') {
    out <- paste(out, gsub('([[:punct:]])|\\s+', '_', foldername), sep = "_")
  }
  out <- paste(area, instrument, out, sep = pathprefixsep)
  out <- rep(out, times = n)
  return(out)
}

#' .generate_name
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
.generate_name <- function(x, startposition = 1) {
  n <- nrow(x)
  rundate <- format(Sys.Date(), format = "%Y%m%d") #produce the date in YYYYMMDD format
  injection.index <- sprintf("%03d", (seq_len(n) - 1) + startposition) #use start queue with input value instead of 1
  injection.name <- paste(rundate, injection.index, sep = "_")
  injection.name <- paste(injection.name, paste("S", x$extract.id,sep = ''), sep = "_")

  injection.name <- gsub("_SNA", "", injection.name)

  injection.name <- paste(injection.name, x$extract.name, sep = "_") %>%
    paste(x$extract.Condition, sep = "_")

  return(injection.name)
}

#' .generate_name
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' bfabricShiny:::.generate_name_order(bfabricShiny:::.test_data_medium())
#'
.generate_name_order <- function(x, startposition = 1){
  n <- nrow(x)
  rundate <- format(Sys.Date(), format = "%Y%m%d") #produce the date in YYYYMMDD format
  injection.index <- sprintf("%03d", (seq_len(n) - 1) + startposition) #use start queue with input value instead of 1
  injection.name <- paste(rundate, paste("C", x$containerid, sep = ''), sep = "_")
  injection.name <- paste(injection.name, injection.index, sep = "_")
  injection.name <- paste(injection.name, paste("S", x$extract.id, sep = ''), sep = "_")
  injection.name <- gsub("_SNA", "", injection.name)
  injection.name <- paste(injection.name, x$extract.name, sep = "_")
  injection.name <- paste(injection.name, x$extract.Condition, sep = "_")

  return(injection.name)
}

#'
#'@export
#'@examples
#'
#' generate_queue_order(bfabricShiny:::.test_data_medium())
#' generate_queue_order(bfabricShiny:::.test_data_medium())
#' generate_queue_order(bfabricShiny:::.test_data_order())
generate_queue_order <- function(x,
                                 foldername = '',
                                 projectid = 1000,
                                 area = 'Proteomics',
                                 instrument = 'LUMOS_2',
                                 username = 'cpanse',
                                 autoQC01 = "TRUE",
                                 QC01o = 3,
                                 QC01m = 1,
                                 autoQC02 = "FALSE",
                                 QC02o = 0,
                                 QC02m = 1,
                                 autoQC4L = "FALSE",
                                 QC4Lo = 4,
                                 QC4Lm = 1,
                                 clean = "TRUE",
                                 cleano = 4,
                                 cleanm = 1,
                                 start1 = 1,
                                 start2 = NA,
                                 start3 = NA,
                                 end1 = 1,
                                 end2 = NA,
                                 end3 = NA,
                                 lists = 1,
                                 startposition = 1,
                                 nr.methods = 2,
                                 nr.replicates = 3,
                                 qc.type = 1,
                                 method = 'default',
                                 pathprefix = "D:\\Data2San",
                                 pathprefixsep = "\\",
                                 DEBUG = FALSE,
                                 startpos = list(row = 1, col = "A", plate = 1)){

  samples_list <- split(x, x$containerid)
  res_queues <- list()

  order <- if (length(samples_list) > 1 || is.null(projectid)) {TRUE}else{FALSE}


  for (i in 1:length(samples_list)) {
    res <-
      generate_queue(samples_list[[i]],
                     foldername = foldername,
                     projectid = projectid,
                     area = area,
                     instrument = instrument,
                     username = username,
                     autoQC01 = autoQC01,
                     QC01o = QC01o,
                     QC01m = QC01m,
                     autoQC02 = autoQC02,
                     QC02o = QC02o,
                     QC02m = QC02m,
                     autoQC4L = autoQC4L,
                     QC4Lo = QC4Lo,
                     QC4Lm = QC4Lm,
                     clean = clean,
                     cleano = cleano,
                     cleanm = cleanm,
                     start1 = start1,
                     start2 = start2,
                     start3 = start3,
                     end1 = end1,
                     end2 = end2,
                     end3 = end3,
                     lists = lists,
                     startposition = startposition,
                     nr.methods = nr.methods,
                     nr.replicates = nr.replicates,
                     qc.type = qc.type,
                     method = method,
                     pathprefix = pathprefix,
                     pathprefixsep = pathprefixsep,
                     order = order,
                     startpos = startpos)
    startpos <- list(row = res$nextpos$rows, col = res$nextpos$cols, plate = res$nextpos$plate)
    res_queues[[names(samples_list)[i]]] <- res$rv
  }
  res <- dplyr::bind_rows(res_queues)
  return(res)
}

#' FGCZ mass spec queue generator
#'
#' @param x
#' @param foldername
#' @param projectid
#' @param area
#' @param instrument
#' @param username
#' @param how.often
#' @param how.many
#' @param nr.methods
#' @param nr.replicates
#' @param showcondition
#' @param qc.type
#' @param hplc
#' @param method
#' @param pathprefix
#' @param pathprefixsep
#'
#' @return a instrument configuration as \code{data.frame}.
#' @export generate_queue
#'
#' @examples
#' generate_queue(x <- bfabricShiny:::.test_data_medium(),start2 = NA,start3 = NA)
#' generate_queue(x <- bfabricShiny:::.test_data_medium(),
#'    projectid = 3000,
#'    area = "Proteomics",
#'    instrument = "QEXACTIVE_2",
#'    username = "roschi",
#'    autoQC01 = "TRUE",
#'    QC01o = 4,
#'    QC01m = 1,
#'    autoQC02 = "FALSE",
#'    QC02o = 4,
#'    QC02m = 1,
#'    autoQC4L = "FALSE",
#'    QC4Lo = 4,
#'    QC4Lm = 1,
#'    clean = "FALSE",
#'    cleano = 4,
#'    cleanm = 1,
#'    start1 = 1,
#'    start2 = NA,
#'    start3 = NA,
#'    end1 = 4,
#'    end2 = 1,
#'    end3 = 3,
#'    lists = 1,
#'    startposition = 1,
#'    nr.methods = 1,
#'    nr.replicates = 1,
#'    qc.type = 1,
#'    method = "default",
#'    pathprefix = "D:Data2San")
#' generate_queue(bfabricShiny:::.test_data_single() , order=TRUE, startpos = list(row=3, col="A", plate=2))
#' generate_queue(bfabricShiny:::.test_data_medium() , order=FALSE)
#' generate_queue(bfabricShiny:::.test_data_single())
#' generate_queue(bfabricShiny:::.test_data_large())
#' generate_queue(bfabricShiny:::.test_data_medium_random())
#'
#' generate_queue(bfabricShiny:::.test_data_medium_random(), method = "default")
#' generate_queue(bfabricShiny:::.test_data_medium_random(), method = "random")
#' generate_queue(bfabricShiny:::.test_data_medium(), method = "blockrandom")
#' bfabricShiny:::.test_data_medium()
#' generate_queue(bfabricShiny:::.test_data_medium())
#'
generate_queue <- function(x,
                           foldername = '',
                           projectid = 1000,
                           area = 'Proteomics',
                           instrument = 'LUMOS_2',
                           username = 'cpanse',
                           autoQC01 = "TRUE",
                           QC01o = 3,
                           QC01m = 1,
                           autoQC02 = "FALSE",
                           QC02o = 0,
                           QC02m = 1,
                           autoQC4L = "FALSE",
                           QC4Lo = 4,
                           QC4Lm = 1,
                           clean = "TRUE",
                           cleano = 4,
                           cleanm = 1,
                           start1 = 1,
                           start2 = NA,
                           start3 = NA,
                           end1 = 1,
                           end2 = NA,
                           end3 = NA,
                           lists = 1,
                           startposition = 1,
                           nr.methods = 2,
                           nr.replicates = 3,
                           qc.type = 1,
                           method = 'default',
                           pathprefix = "D:\\Data2San",
                           pathprefixsep = "\\",
                           DEBUG = FALSE,
                           order = TRUE,
                           startpos = list(row = 1, col = "A", plate = 1)
                           ){
  # generate the queue template
  if (method == 'default'){
    res.template <- .generate_template_base(x = x)
  }
  else if (method == 'random') {
    res.template <- .generate_template_random(x = x)
  } else if (method == 'blockrandom') {
    res.template <- .generate_template_random_block(x = x)
  } else if (method == 'PRM') {
    res.template <- .generate_template_PRM(x = x, lists = lists, instrument = instrument)
  } else if (method == 'testing') {
    res.template <- .generate_template_method_testing(x = x,
                                                      nr.methods = nr.methods,
                                                      nr.replicates = nr.replicates,
                                                      instrument = instrument)
  }else{
    stop("wrong method selected :", method)
  }

  if (!method %in% c('PRM', 'testing')) {
    res.position <- .tray_position(queue = res.template, startpos = startpos, instrument = instrument )
    nextpos <- get_tray_2_48_plates_nextpos(nrow(res.template))
  }


  # insert qc samples

  res.autoQC01 <- .autoQC01(nrow(res.position),
                            instrument = instrument,
                            QC01o = QC01o,
                            QC01m = QC01m,
                            autoQC01 = autoQC01)

  res.autoQC02 <- .autoQC02(nrow(res.position),
                            instrument = instrument,
                            QC02o = QC02o,
                            QC02m = QC02m,
                            autoQC02 = autoQC02)

  res.autoQC4L <- .autoQC4L(nrow(res.position),
                            instrument = instrument,
                            QC4Lo = QC4Lo,
                            QC4Lm = QC4Lm,
                            autoQC4L = autoQC4L)

  res.clean <- .clean(nrow(res.position),
                      instrument = instrument,
                      cleano = cleano,
                      cleanm = cleanm,
                      clean = clean)

  res.qc <- .insert_qc_samples(x = res.position,
                               y = res.autoQC01,
                               z = res.autoQC02,
                               u = res.autoQC4L,
                               v = res.clean)

  # clean up the sample queue
  res.queue <- .clean_queue(x = res.qc,
                            instrument = instrument,
                            start1 = start1,
                            start2 = start2,
                            start3 = start3,
                            end1 = end1,
                            end2 = end2,
                            end3 = end3)
  res.queue$containerid <- unique( na.omit( res.queue$containerid ) )
  # generate folder name acc. FGCZ naming convention
  res.folder <- .generate_folder_name(x = res.queue,
                                      foldername = foldername,
                                      area = area,
                                      instrument = instrument,
                                      username = username,
                                      pathprefixsep = pathprefixsep)

  # generate file name
  if (order) {
    res.filename <- .generate_name_order(x = res.queue, startposition = startposition)
  }else{
    res.filename <- .generate_name(x = res.queue, startposition = startposition)
  }

  orderstring <- if (order) {'orders'}else{paste('p', projectid, sep = '')}

  rv <- cbind('File Name' = res.filename,
              'Path' = paste(pathprefix, orderstring , res.folder, sep = pathprefixsep),
              'Position' = as.character(res.queue$position),
              'Inj Vol' = rep(2, times = length(res.filename)),
              'L3 Laboratory' = rep("FGCZ", times = length(res.filename)),
              'Sample ID' = res.queue$extract.id,
              'Sample Name' = res.queue$extract.name,
              'L1 Study' = rep(projectid, times = length(res.filename))
  )

  # some naming cosmetics
  rv[, 'File Name' ] <- gsub("[^-a-zA-Z0-9_]", "_", rv[, 'File Name' ])
  rv[, 'File Name' ] <- gsub("_autoQC01_autoQC01", "_autoQC01", rv[, 'File Name' ])
  rv[, 'File Name' ] <- gsub("_autoQC02_autoQC02", "_autoQC02", rv[, 'File Name' ])
  rv[, 'File Name' ] <- gsub("_autoQC4L_autoQC4L", "_autoQC4L", rv[, 'File Name' ])
  rv[, 'File Name' ] <- gsub("_clean_clean", "_clean", rv[, 'File Name' ])
  rv[, 'File Name' ] <- gsub("_N_A$", "", rv[, 'File Name' ])

  rv <- as.data.frame(rv)
  rv$"Instrument Method" <- ""
  rv$"Instrument Method"[grep("_autoQC01", rv$"File Name")] <- "C:\\Xcalibur\\methods\\__autoQC\\trap\\autoQC01"
  rv$"Instrument Method"[grep("_autoQC02", rv$"File Name")] <- "C:\\Xcalibur\\methods\\__autoQC\\trap\\autoQC02"
  rv$"Instrument Method"[grep("_autoQC4L", rv$"File Name")] <- "C:\\Xcalibur\\methods\\__autoQC\\trap\\autoQC4L"

  if (DEBUG) {
    rv <- merge(rv, x, by.x = "Sample ID", by.y = "extract.id", all = TRUE)
  }
  rv <- rv[order(rv$`File Name`),]
  return(list(rv = rv, nextpos = nextpos))
}



#' Run shiny queue generator application
#'
#' @return
#'
#' @export runQueue
#' @examples
#' #bfabricShiny::runQueue()
runQueue <- function(){
  qgs <- system.file("shiny", "queue_generator10", package = "bfabricShiny")
  shiny::runApp(qgs,
                #host = getOption("shiny.host", "127.0.0.1"),
                host = "130.60.81.134",
                port = 1234,
                display.mode = "normal",
                quiet = TRUE,
                launch.browser = FALSE)
}
