#R
# Autors: Christian Trachsel / Christian Panse / Witold Wolski
#
#generate test data set ----

test_data_single <- function(){
  extract.name <- "Sample_1"
  extract.id <- "SID1"
  extract.Condition <- "Control"
  data.frame(extract.name, extract.id, extract.Condition)
}

test_data_medium <- function(){
  extract.name <- c(paste("Sample", 1:20, sep = "_"))
  extract.id <- c(paste("SID", 1:20, sep = ""))
  extract.Condition <- c(rep("Control", 4), rep("Ampicillin", 4), rep("Kanamycin", 4), rep("Less", 3), rep("More", 5))
  data.frame(extract.name, extract.id, extract.Condition)
}

test_data_medium_random <- function(){
  extract.name <- c(paste("Sample", 1:20, sep = "_"))
  extract.id <- c(paste("SID", 1:20, sep = ""))
  condition <- c(rep("Control", 4), rep("Ampicillin", 4), rep("Kanamycin", 4), rep("Less", 3), rep("More", 5))
  extract.Condition <- condition[sample(1:20)]
  data.frame(extract.name, extract.id, extract.Condition)
}

test_data_large <- function(){
  extract.name <- c(paste("Sample", 1:80, sep = "_"))
  extract.id <- c(paste("SID", 1:80, sep = ""))
  extract.Condition <- c(rep("Control", 16), rep("Ampicillin", 16), rep("Kanamycin", 16), rep("Less", 12), rep("More", 20))
  data.frame(extract.name, extract.id, extract.Condition)
}

#HPLC position helper functions ----

.eksigent <- function(){
  tray1 <- rep(2, times = 46) %>% 
    paste(rep(LETTERS[1:6], each = 8), sep = "") %>% 
    paste(rep(sprintf("%02d", c(1:8)), times = 6), sep = "") 
  pos <- tray1[1:45]
  return(pos)
}

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
  return(pos)
}

.easylc <- function(){
  tray1 <- paste(rep(LETTERS[1:6], each = 8), rep(1:8, times = 6), sep = "")
  pos <- tray1[1:45]
  return(pos)
}

#Define HPLC, autoQC01, autoQC02, autoQC4L, clean ----
#list elements are: 1) HPLC, 2) sample positions, 3) autoQC01 position, 4) autoQC02 position, 5) autoQC4L position, 6) clean position
getHPLCparameter <- function(){list(VELOS_1 = c('eksigent', list(.eksigent()), '2F08', '2F07', '2F07','2F06'),
                                    VELOS_2 = c('eksigent', list(.eksigent()),'2F08', '2F07', '2F07','2F06'),
                                    G2HD_1 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                    QTRAP_1 = c('eksigent', list(.eksigent()), '2F08', '2F07', '2F07','2F06'),
                                    TSQ_1 = c('eksigent', list(.eksigent()),'2F08', '2F07', '2F07','2F06'),
                                    TSQ_2 = c('eksigent', list(.eksigent()), '2F08', '2F07', '2F07','2F06'),
                                    QEXACTIVE_2 = c('easylc', list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                    QEXACTIVE_3 = c('easylc', list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                    FUSION_1 = c('easylc',list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                    FUSION_2 = c('easylc', list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                    QEXACTIVEHF_1 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                    QEXACTIVEHF_2 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                    QEXACTIVEHFX_1 = c('waters',list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                    IMSTOF_1 = c('eksigent', list(.eksigent()), '2F08', '2F07', '2F07','2F06'))}

getQCsample <- function(){list(extract.name = c('autoQC01', 'autoQC02', 'autoQC4L', 'clean'),
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
.tray_position <- function(x, instrument = ""){
  
  pos.idx <- unlist(getHPLCparameter()[[instrument]][2])
  n <- nrow(x) 
  positions.needed <- ceiling(n/46)
  pos <- rep(pos.idx, times = positions.needed)
  pos <- pos[1:n]
  x$position <- pos
  res <- x
  
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
    dplyr::mutate(IDX = 1:length(y)+ 0.1) %>% 
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

#QC inserting functions ----

.autoQC01 <- function(x, instrument, QC01o, QC01m, autoQC01){ 
  if (QC01o == 0|autoQC01 == "FALSE"){
    res <- .emptydf()
  } else {
    check <- floor(nrow(x)/QC01o)
    if(check < 1){
      qc.inserts <- 1
      qc.idx <- nrow(x)
    }else {
      qc.inserts = floor(nrow(x)/QC01o)
      qc.idx <- QC01o*(1:qc.inserts)
    }
    repetitions <- qc.inserts*QC01m
    df <- data.frame(extract.name = "autoQC01",
                     extract.id = as.integer(NA), 
                     extract.Condition = "autoQC01", 
                     position = unlist(getHPLCparameter()[[instrument]][3]),
                     idx = as.numeric(NA),
                     stringsAsFactors = FALSE)
    res <- df[rep(1, times = repetitions), ]
    res$idx  <-  rep(qc.idx + 0.6, each = QC01m)
  }
  
  return(res)
}

.autoQC02 <- function(x, instrument, QC02o, QC02m, autoQC02){
  if (QC02o == 0 |autoQC02 == "FALSE"){
    res <- .emptydf()
  } else {
    check <- floor(nrow(x)/QC02o)
    if(check < 1){
      qc.inserts <- 1
      qc.idx <- nrow(x)
    }else {
      qc.inserts = floor(nrow(x)/QC02o)
      qc.idx <- QC02o*(1:qc.inserts)
    }
    repetitions <- qc.inserts*QC02m
    df <- data.frame(extract.name = "autoQC02",
                     extract.id = as.integer(NA), 
                     extract.Condition = "autoQC02", 
                     position = unlist(getHPLCparameter()[[instrument]][4]),
                     stringsAsFactors = FALSE)
    res <- df[rep(1, times = repetitions), ]
    res$idx  <-  rep(qc.idx + 0.8, each = QC02m)
  }
  
  return(res)
}

.autoQC4L <- function(x, instrument, QC4Lo, QC4Lm, autoQC4L){
  if (QC4Lo == 0|autoQC4L == "FALSE"){
    res <- .emptydf()
  } else {
    check <- floor(nrow(x)/QC4Lo)
    if(check < 1){
      qc.inserts <- 1
      qc.idx <- nrow(x)
    }else {
      qc.inserts = floor(nrow(x)/QC4Lo)
      qc.idx <- QC4Lo*(1:qc.inserts)
    }
    repetitions <- qc.inserts*QC4Lm
    df <- data.frame(extract.name = "autoQC4L",
                     extract.id = as.integer(NA), 
                     extract.Condition = "autoQC4L", 
                     position = unlist(getHPLCparameter()[[instrument]][4]),
                     stringsAsFactors = FALSE)
    res <- df[rep(1, times = repetitions), ]
    res$idx  <-  rep(qc.idx + 0.9, each = QC4Lm)
  }
  
  return(res)
}

.clean <- function(x, instrument, cleano, cleanm, clean){
  if (cleano == 0|clean == "FALSE"){
    res <- .emptydf()
  } else {
    check <- floor(nrow(x)/cleano)
    if(check < 1){
      qc.inserts <- 1
      qc.idx <- nrow(x)
    }else {
      qc.inserts = floor(nrow(x)/cleano)
      qc.idx <- cleano*(1:qc.inserts)
    }
    repetitions <- qc.inserts*cleanm
    df <- data.frame(extract.name = "clean",
                     extract.id = as.integer(NA), 
                     extract.Condition = "clean", 
                     position = unlist(getHPLCparameter()[[instrument]][6]),
                     stringsAsFactors = FALSE)
    res <- df[rep(1, times = repetitions), ]
    res$idx  <-  rep(qc.idx + 0.4, each = cleanm)
  }
  
  return(res)
}

.insert_qc_samples <- function(x, y, z, u, v){
  x <- x %>% 
    mutate(idx = seq_along(x$extract.name))
  
  res <- dplyr::bind_rows(x,y,z,u,v) %>% 
    dplyr::arrange(idx)
  
  res[sapply(res, is.factor)] <- lapply(res[sapply(res, is.factor)], as.character)
  cleanup.idx <- max(which(res$extract.name == x[which(x$extract.name == x[nrow(x), 1]), 1]))
  res <- res[1:cleanup.idx, 1:4]
  
  return(res)
}

#queue start and end ----
.gen.start <- function(instrument, start1, start2, start3){ #argument names!
  if (is.na(start1)){
    S1 <-  .emptydf()
    S1 <- S1[, 1:4]
  } else {
    S1 <- data.frame(extract.name = getQCsample()[[1]][start1],
                     extract.id = getQCsample()[[2]][start1], 
                     extract.Condition = getQCsample()[[3]][start1], 
                     position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][start1]]]),
                     stringsAsFactors = FALSE)
  }
  if (is.na(start2)){
    S2 <-  .emptydf()
    S2 <- S2[, 1:4]
  } else {
    S2 <- data.frame(extract.name = getQCsample()[[1]][start2],
                     extract.id = getQCsample()[[2]][start2], 
                     extract.Condition = getQCsample()[[3]][start2], 
                     position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][start2]]]),
                     stringsAsFactors = FALSE)
  }
  if (is.na(start3)){
    S3 <-  .emptydf()
    S3 <- S3[, 1:4]
  } else {
    S3 <- data.frame(extract.name = getQCsample()[[1]][start3],
                     extract.id = getQCsample()[[2]][start3], 
                     extract.Condition = getQCsample()[[3]][start3], 
                     position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][start3]]]),
                     stringsAsFactors = FALSE)
  }
  res <- dplyr::bind_rows(S1, S2, S3)
  
  return(res)
}



.gen.end <- function(instrument, end1, end2, end3){ #argument names!
  if (is.na(end1)){
    E1 <-  .emptydf()
    
  } else {
    E1 <- data.frame(extract.name = getQCsample()[[1]][end1],
                     extract.id = getQCsample()[[2]][end1], 
                     extract.Condition = getQCsample()[[3]][end1], 
                     position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][end1]]]),
                     stringsAsFactors = FALSE)
  }
  
  if (is.na(end2)){
    E2 <-  .emptydf()
    
  } else {
    E2 <- data.frame(extract.name = getQCsample()[[1]][end2],
                     extract.id = getQCsample()[[2]][end2], 
                     extract.Condition = getQCsample()[[3]][end2], 
                     position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][end2]]]),
                     stringsAsFactors = FALSE)
  }
  
  if (is.na(end3)){
    E3 <-  .emptydf()
    
  } else {
    E3 <- data.frame(extract.name = getQCsample()[[1]][end3],
                     extract.id = getQCsample()[[2]][end3], 
                     extract.Condition = getQCsample()[[3]][end3], 
                     position = unlist(getHPLCparameter()[[instrument]][[getQCsample()[[4]][end3]]]),
                     stringsAsFactors = FALSE)
  }
  
  df <- dplyr::bind_rows(E1, E2, E3)
  
  return(df)
}

.clean_queue <- function(x, instrument, start1, start2, start3, end1, end2, end3){
  Instrument = instrument
  start1 = start1
  start2 = start2
  start3 = start3
  
  start <- .gen.start(instrument, start1, start2 , start3)
  
  end <- .gen.end(instrument, end1, end2 , end3)
  
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
  res <- x
  #  res$extract.name = as.factor(res$extract.name)
  #  res$extract.id = as.integer(res$extract.id)
  #  res$extract.Condition = as.factor(res$extract.Condition)
  return(res)
}

#random queue formating function ----

.generate_template_random <- function(x){
  res <- x[order(sample(nrow(x))), ]
  return(res)
}

#blockrandom queue formating function ----

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
    .equal.groups
  res <- res %>% 
    dplyr::mutate(blockidx = as.vector(replicate(blocks, sprintf("%02d", c(1:elements))))) %>% 
    dplyr::arrange(blockidx) %>% 
    dplyr::mutate(randomidx = as.vector(replicate(elements, sample(1:blocks)))) %>% 
    dplyr::arrange(blockidx, randomidx) %>% 
    dplyr::select("extract.name", "extract.id", "extract.Condition") %>% 
    dplyr::filter(!is.na(extract.id))
  
  return(res)
}

#method evaluation queue formating function ----

.generate_template_method_testing <- function(x, nr.methods = 2, nr.replicates = 3, instrument = "QEXACTIVEHF_2"){
  res <- .tray_position(x = x, instrument = instrument)
  res <- res[rep(seq_len(nrow(res)), each = nr.methods), ]
  res$extract.Condition <- paste(res$extract.Condition, paste(rep("Method", times = nr.methods), 1:nr.methods, sep = "_"), sep = "_")
  res <- res[rep(seq_len(nrow(res)), each = nr.replicates ), ]
  res <- res[order(sample(nrow(res))), ]
  
  return(res)
}

#PRM queue formating function ----

.generate_template_PRM <- function(x, lists = 2, instrument = ""){
  res <- x[order(sample(nrow(x))), ]
  res <- .tray_position(res, instrument = instrument)
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
.generate_folder_name <- function(x, foldername, area = "Proteomics", instrument = "FUSION_1", username = "bfabricusername", pathprefixsep='/'){
  n <- nrow(x)
  rundate <- format(Sys.Date(), format = "%Y%m%d") 
  out <- paste(username, rundate, sep = "_")
  if (foldername != ''){
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
.generate_name <- function(x, startposition = 1){
  n <- nrow(x)
  rundate <- format(Sys.Date(), format = "%Y%m%d") #produce the date in YYYYMMDD format
  injection.index <- sprintf("%03d", (seq_len(n)-1)+ startposition) #use start queue with input value instead of 1
  injection.name <- paste(rundate, injection.index, sep = "_")
  injection.name <- paste(injection.name, paste("S", x$extract.id,sep=''), sep = "_")
  
  injection.name <- gsub("_SNA", "", injection.name)
  
  injection.name <- paste(injection.name, x$extract.name, sep = "_") %>% 
    paste(x$extract.Condition, sep = "_")
  
  return(injection.name)
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
generate_queue <- function(x, 
                           foldername = '', 
                           projectid = 1000, 
                           area = 'Proteomics', 
                           instrument = 'FUSION_1', 
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
                           start2 = "",
                           start3 = "",
                           end1 = 1,
                           end2 = "",
                           end3 = "",
                           lists = 1,
                           startposition = 1,
                           nr.methods = 2, 
                           nr.replicates = 3,
                           qc.type = 1,
                           method = 'default',
                           pathprefix = "D:\\Data2San", 
                           pathprefixsep = "\\"){
  
  # generate the queue template
  if(method == 'random'){
    res.template <- .generate_template_random(x = x)
  }else if (method == 'blockrandom'){
    res.template <- .generate_template_random_block(x = x)
  }else if (method == 'PRM'){
    res.template <- .generate_template_PRM(x = x, lists = lists, instrument = instrument)  
  }else if (method == 'testing'){
    res.template <- .generate_template_method_testing(x = x,
                                                      nr.methods = nr.methods,
                                                      nr.replicates = nr.replicates,
                                                      instrument = instrument)
  }else {
    res.template <- .generate_template_base(x = x)
  }  
  
  # attache HPLC plate position
  
  if(method == 'PRM'){
    res.position <- res.template
  } else if (method == 'testing'){
    res.position <- res.template 
  } else {
    res.position <- .tray_position(x = res.template, instrument = instrument )  
  }
  
  # insert qc samples
  
  res.autoQC01 <- .autoQC01(x = res.position, 
                            instrument = instrument, 
                            QC01o = QC01o, 
                            QC01m = QC01m,
                            autoQC01 = autoQC01)
  
  res.autoQC02 <- .autoQC02(x = res.position, 
                            instrument = instrument, 
                            QC02o = QC02o, 
                            QC02m = QC02m,
                            autoQC02 = autoQC02)
  
  res.autoQC4L <- .autoQC4L(x = res.position, 
                            instrument = instrument, 
                            QC4Lo = QC4Lo, 
                            QC4Lm = QC4Lm,
                            autoQC4L = autoQC4L)
  
  
  res.clean <- .clean(x = res.position, 
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
  
  # generate folder name acc. FGCZ naming convention
  res.folder <- .generate_folder_name(x = res.queue,
                                      foldername = foldername, 
                                      area = area, 
                                      instrument = instrument, 
                                      username = username,
                                      pathprefixsep = pathprefixsep)
  
  # generate file name  
  res.filename <- .generate_name(x = res.queue, startposition = startposition)
  
  cbind('File Name' = res.filename,
        'Path' = paste(pathprefix, paste('p', projectid, sep = ''), res.folder, sep = pathprefixsep), 
        'Position' = as.character(res.queue$position),
        'Inj Vol' = rep(2, times = length(res.filename)),
        'L3 Laboratory' = rep("FGCZ", times = length(res.filename)),
        'Sample ID' = res.queue$extract.id,
        'Sample Name' = res.queue$extract.name,
        'L1 Study' = rep(projectid, times = length(res.filename))
  )
  
}


#' Run shiny queue generator application
#'
#' @return
#'
#' @export runQueue
runQueue <- function(){
  qgs <- system.file("shiny", "queue_generator", package = "bfabricShiny")
  shiny::runApp(qgs, display.mode = "normal")
}