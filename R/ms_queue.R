#R
# Autors: Christian Trachsel / Christian Panse / Witold Wolski
#


#TODOS:
# think of good generic layout for shiny user interface for selecting the appropriate QC standard(s)
# select from:
#Proteomics -> Fetuin, clean, HeLc?, BSA?,
# -> select QC sample -> drop down with selections
# insert Cleans -> with every standard/ every second standard/none
# refactor code to match UI



#generate testdata sets
test_data_single <- function(){
  extract.name <- "Sample_1"
  extract.id <- 1
  extract.Condition <- "Control"
  data.frame(extract.name, extract.id, extract.Condition)
}

test_data_medium <- function(){
  extract.name <- c(paste("Sample", 1:20, sep = "_"))
  extract.id <- c(1:20)
  extract.Condition <- c(rep("Control", 4), rep("Ampicillin", 4), rep("Kanamycin", 4), rep("Less", 3), rep("More", 5))
  data.frame(extract.name, extract.id, extract.Condition)
}

test_data_large <- function(){
  extract.name <- c(paste("Sample", 1:80, sep = "_"))
  extract.id <- c(1:80)
  extract.Condition <- c(rep("Control", 16), rep("Ampicillin", 16), rep("Kanamycin", 16), rep("Less", 12), rep("More", 20))
  data.frame(extract.name, extract.id, extract.Condition)
}

#' .equilize_groups
#'
#' @param df: a dataframe (data.frame)
#' @param group: a column index indicating along which the grouping (split) is performed (integer)
#' @param rowsneeded: a value indicating how many rows each group will contain after the function call (numeric)
#'
#' @return a new dataframe in which each group is containing the same number of rows as indicated by rowsneeded. New inserted rows will
#' contain <NA> as value
#' @export
#'
#' @examples
#' 
#' data <- data.frame(extract.name = paste("Sample", 1:4, sep = "_"), extract.id = 1:4, extract.Condition = c(rep("A",2), rep("B",2)))
#' grouping <- which(colnames(data) == "extract.Condition")
#' 
#' equilize_groups(data, grouping, 4)
#' 
#' equilize_groups(test_data_medium(), 3, 2)
#' 
.equilize_groups <- function(df, group, rowsneeded){
  do.call(rbind, lapply(split(df, df[[group]]), function(x){
    x <- data.frame(lapply(x, `length<-`, rowsneeded))
    x[group] <- x[[group]][1]
    return(x)
  }))
}

#' .order_extract.Condition_blocks
#'
#' a function for randomization of a queue of samples in a block randomized manner.
#'
#' @param x: the sample information containing the extract.Condition information in one column (data.frame)
#'
#' @return returns the initial dataframe but sample order within the extract.Conditions is randomly shuffled (data.frame)
#' @export
#'
#' @examples
#' 
#' data <- data.frame(extract.name = paste("Sample", 1:10, sep = "_"), extract.id = 1:10, extract.Condition = c(rep("A",5), rep("B",5)))
#' 
#' block_randomizer(data)
#' 
#' block_randomizer(test_data_medium())
#' 
.order_condition_blocks <- function(x){
  unique.extract.Conditions <- unique(x$extract.Condition) 
  number.of.extract.Conditions <-length(unique.extract.Conditions) 
  df <- vector(number.of.extract.Conditions, mode = "list")
  for(i in 1:number.of.extract.Conditions){ 
    sub <- x[x$extract.Condition == unique.extract.Conditions[i], ] 
    sub <- sub[sample(1:nrow(sub)), ] 
    df[[i]] <-  sub 
  }
  df <- do.call("rbind", df)  
  return(df)
}

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

#' .generate_template_random
#'
#' @param x:the sample information (data.frame)
#'
#' @return the original dataframe randomly reordered (data.frame)
#' @export
#'
#' @examples
#' 
#' data <- data.frame(extract.name = LETTERS[1:10], extract.id = 1:10, extract.Condition = letters[1:10])
#' 
#' .generate_template_random(data)
#' 
.generate_template_random <- function(x){
  random.index <- sample(1:nrow(x))
  res <- x[order(random.index),]
  return(res)
}

#'.generate_template_random_block
#'
#'this function is composing blocks of samples (each block contains one randomly picked sample from each extract.Condition). The functions
#'does not required all extract.Conditions to have the same lenght (which is not strictly correct with the "random block" design).
#'
#' @param x: the sample information. A column with the information of the samples extract.Conditions is madatory (data.frame)
#'
#' @return the initial data ordered in random blocks (each block contains one random sample from each extract.Condition) (data.frame)
#' @export
#'
#' @examples
#' 
#' data <- data.frame(extract.name = paste("Sample", 1:10, sep = "_"), extract.id = 1:10, extract.Condition = c(rep("A",5), rep("B",5)))
#' 
#' .generate_template_random_block(data)
#' 
#' .generate_template_random_block(test_data_medium())
#' 
.generate_template_random_block <- function(x){
  #TODO:
  #check if more than one condition is present. If not display a warning
  #check if all extract.Conditions are equal sized if not display a warning
  # tab <- as.vector(table(x$extract.Condition))
  #length(unique(tab)) == 1
  #TODO END
  repeats <- length(unique(x$extract.Condition))
  cond <- max(table(x$extract.Condition))
  res <- .order_condition_blocks(x)
  extract.Condition.vector <- as.vector(replicate(repeats, sprintf("%02d", c(1:cond))))
  blockgroupindex <- which(colnames(res) == "extract.Condition")
  res <- .equilize_groups(res, blockgroupindex, cond)
  res$blockrandom <- extract.Condition.vector
  res <- res[order(res$blockrandom),]
  extract.Condition.vector.2 <- as.vector(replicate(repeats, sample(1:repeats)))
  resort <- paste(res$blockrandom, extract.Condition.vector.2, sep =".")
  res$blockrandom2 <- resort
  res <- res[order(res$blockrandom2), ]
  res$blockrandom <- NULL
  res$blockrandom2 <- NULL
  res <- res[!is.na(res$extract.name), ]  
  return(res)
}




.generate_template_method_testing <- function(x, nr.methods = 2, nr.replicates = 3){
  x <- x[rep(seq_len(nrow(x)), each = nr.methods), ]
  x$extract.Condition <- paste(rep("Method", times = nr.methods), 1:nr.methods, sep = "_")
  res <- x[rep(seq_len(nrow(x)), each = nr.replicates ), ]
  res$dev <- 
  #res <- .order_condition_blocks(res)
  return(res)
}

#' .generate_template_method_testing
#'
#' @param x: the sample information, optimally only one single sample (data.frame)
#' @param nr.methods: the number of methods which will be tested (numeric)
#' @param nr.replicates: how many injections for each method should be performed (numeric) 
#' @param hplc 
#'
#' @return a dataframe which can be used as input for the queue formating functions
#' @export
#'
#' @examples
#' 
#' data <- data.frame(extract.name = paste("Sample", 1:2, sep = "_"), extract.id = 1:2, extract.Condition = c("A","B"))
#' 
#' .method_testing(data, 1, 4)
#' 
#' data <- data.frame(extract.name = "Sample_1", extract.id = 1, extract.Condition = "A")
#' 
#' .method_testing(data, 3, 2)
#' 
.generate_template_method_testing <- function(x, nr.methods = 2, nr.replicates = 3, hplc = "easylc"){
  res <- .hplc_position_test(x = x, hplc = hplc)
  res <- res[rep(seq_len(nrow(res)), each = nr.methods), ]
  res$extract.Condition <- paste(res$extract.Condition, paste(rep("Method", times = nr.methods), 1:nr.methods, sep = "_"), sep = "_")
  res <- res[rep(seq_len(nrow(res)), each = nr.replicates ), ]
  res <- res[order(sample(nrow(res))), ]
  res <- res[order(sample(nrow(res))), ]
  return(res)
}


.eksigent <- function(){
  pos <- (paste(rep(2, times = 46), paste(rep(LETTERS[1:6], each = 8), rep(sprintf("%02d", c(1:8)), times = 6), sep = ""), sep = "")) 
  pos[1:45]
  return(pos)
}

.waters <- function(){
  pos.p1 <- paste(rep(1, times = 46), paste(rep(LETTERS[1:6], each = 8), rep(1:8, times = 6), sep = ","), sep = ":") 
  pos.p1 <- paste0('"', pos.p1, '"')
  pos.p1 <- pos.p1[1:45]
  pos.p2 <- paste(rep(2, times = 46), paste(rep(LETTERS[1:6], each = 8), rep(1:8, times = 6), sep = ","), sep = ":") 
  pos.p2 <- paste0('"', pos.p2, '"')
  pos.p2 <- pos.p2[1:45]
  pos <- c(pos.p1, pos.p2)
  return(pos)
}

.easylc <- function(){
  pos <- paste(rep(LETTERS[1:6], each = 8), rep(1:8, times = 6), sep = "")
  pos[1:45]
  return(pos)
}

.hplc_position_test <- function(x, hplc = ""){
  n <- nrow(x)
  if (hplc == "eksigent") {
    pos <- .eksigent()
  } else if (hplc == "waters") {
    pos <- .waters()
  } else if (hplc == "easylc"){
    pos <- .easylc()
  } else {
    pos <- .easylc()
  }
  positions.needed <- ceiling(n/46)
  pos <- rep(pos, times = positions.needed)
  pos <- pos[1:n]
  x$position <- pos
  res <- x
  return(res)
}

#' Title
#'
#' @param x 
#' @param hplc 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
.hplc_position <- function(x, hplc = "", method = ""){
#  if (method == 'testing'){
#    res <- x
#  } else{
   n <- nrow(x)
  if (hplc == "eksigent") {
    pos <- .eksigent()
  } else if (hplc == "waters") {
    pos <- .waters()
  } else if (hplc == "easylc"){
    pos <- .easylc()
  } else {
    pos <- .easylc()
  }
  positions.needed <- ceiling(n/46)
  pos <- rep(pos, times = positions.needed)
  pos <- pos[1:n]
  x$position <- pos
  res <- x
#  }
  return(res)
}

#calculate QC inserts

#for fetuin only
.qc.type.one <- function(x, qc.position, how.often, how.many){
  n <- nrow(x)
  qc.inserts <- floor(n/how.often)
  qc.idx <- how.often*(1:qc.inserts)
  repetitions <- qc.inserts*how.many
  fet <- data.frame(extract.name = "Fetuin_400amol", extract.id = as.integer(NA), extract.Condition = "Fetuin", position = qc.position)
  fet$position <- as.character(fet$position)
  fet <- fet[rep(row.names(fet), repetitions), ]
  res <- rbind(x, fet)
  res$idx  <- c(seq_along(x$extract.name), rep(qc.idx+0.75, each = how.many))
  res <- res[order(res$idx),]
  res$idx <-NULL
  return(res)
}

#for even fetuin,clean
.qc.type.two <- function(x, qc.position, clean.position, how.often, how.many){
  n <- nrow(x)
  qc.inserts <- floor(n/how.often)
  qc.idx <- how.often*(1:qc.inserts)
  repetitions <- qc.inserts*how.many
  fet <- data.frame(extract.name = "Fetuin_400amol", extract.id = as.integer(NA), extract.Condition = "Fetuin", position = qc.position)
  fet <- fet[rep(row.names(fet), repetitions), ]
  clean <- data.frame(extract.name = "Clean", extract.id = as.integer(NA), extract.Condition = "Clean", position = clean.position)
  clean <- clean[rep(row.names(clean), repetitions), ]
  res <- rbind(x, fet, clean)
  res$idx  <- c(seq_along(x$extract.name), rep(qc.idx + 0.75, each = how.many) , rep(qc.idx + 0.25, each = how.many))
  res <- res[order(res$idx),]
  res$idx <-NULL
  return(res)
}
#for odd fetuin,clean
.qc.type.three <- function(x, qc.position, clean.position, how.often, how.many){
  n <- nrow(x)
  qc.inserts <- floor(n/how.often)
  qc.idx <- how.often*(1:qc.inserts)
  repetitions <- qc.inserts*how.many
  fet <- data.frame(extract.name = "Fetuin_400amol", extract.id = as.integer(NA), extract.Condition = "Fetuin", position = qc.position)
  fet <- fet[rep(row.names(fet), repetitions), ]
  clean <- data.frame(extract.name = "Clean", extract.id = as.integer(NA), extract.Condition = "Clean", position = clean.position)
  clean <- clean[rep(row.names(clean), ceiling(repetitions/2)), ]
  res <- rbind(x, fet, clean)
  odd <- c(TRUE, FALSE)
  res$idx  <- c(seq_along(x$extract.name), rep(qc.idx + 0.75, each = how.many) , rep(qc.idx[odd] + 0.25, each = 1))
  res <- res[order(res$idx),]
  res$idx <-NULL
  return(res)
}

#current version
.insert_qc_samples <- function(x, how.often, how.many, hplc = "", qc.type = 1){
  #TODO: assign these variables generic via the instrument hash table -> value predefined
  how.many <- how.many
  how.often <- how.often
  if (hplc == "easylc"){
    qc.position <- as.character("F8")
    clean.position <- as.character("F6")
  } else if ( hplc == "waters"){
    qc.position <- '"1:F,8"'
    clean.position <- '"1:F,6"'
  } else {
    qc.position <- "1F08"
    clean.position <- "1F06"
  }
  if (qc.type == 1){
    res <- .qc.type.one(x = x, qc.position = qc.position, how.often = how.often, how.many = how.many)
  } else if (qc.type == 2){
    res <- .qc.type.two(x, qc.position = qc.position, clean.position = clean.position, how.often = how.often, how.many = how.many)
  } else{
    res <- .qc.type.three(x, qc.position = qc.position, clean.position = clean.position, how.often = how.often, how.many = how.many)
  }
  return(res)
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
.clean_up_queue <- function(x, hplc = ""){
  x[sapply(x, is.factor)] <- lapply(x[sapply(x, is.factor)], as.character)
  cleanup.idx <- max(which(x$extract.name == x[nrow(x),1]))
  x <- x[1:cleanup.idx,]
  if (hplc == "easylc"){
    qc.position <- as.character("F8")
  } else if ( hplc == "waters"){
    qc.position <- '"1:F,8"'
  } else {
    qc.position <- "1F08"
  }
  start <- data.frame(extract.name = rep("Fetuin_400amol",2), extract.id = rep(as.integer(NA),2), extract.Condition = rep("Fetuin",2), position = rep(qc.position,2))
  end <- data.frame(extract.name = rep("Fetuin_400amol",1), extract.id = rep(as.integer(NA),1), extract.Condition = rep("Fetuin",1), position = rep(qc.position,1))
  res <- rbind(start, x, end)
  return(res)
}

#Generate the actual queue



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
.generate_name <- function(x, showcondition = TRUE){
  n <- nrow(x)
  rundate <- format(Sys.Date(), format = "%Y%m%d") #produce the date in YYYYMMDD format
  injection.index <- sprintf("%02d", c(1:n))
  injection.name <- paste(rundate, injection.index, sep = "_")
  injection.name <- paste(injection.name, x$extract.id, sep = "_")
  injection.name <- paste(injection.name, x$extract.name, sep = "_")
  if (showcondition == TRUE){
    injection.name <- paste(injection.name, x$extract.Condition, sep = "_")
  } else {
    
  }
  return(injection.name)
}

generate_queue <- function(x, 
                           foldername='', 
                           projectid=1000, 
                           area='Proteomics', 
                           instrument='FUSION_1', 
                           username='cpanse', 
                           how.often=2, 
                           how.many=1, 
                           nr.methods = 2, 
                           nr.replicates = 3,
                           showcondition = FALSE,
                           qc.type = 1,
                           hplc = '', #list(name='easylc', clean='F6', standard='F8'),
                           method='default',
                           pathprefix="D:\\Data2San", 
                           pathprefixsep="\\"){
  
  if (!'extract.Condition' %in% names(x)){
    x$extract.Condition <- "A"
  }
  
  # generate the queue template
  if(method == 'random'){
    res.template <- .generate_template_random(x)
  }else if (method == 'blockrandom'){
    res.template <- .generate_template_random_block(x)
  }else if (method == 'testing'){
    res.template <- .generate_template_method_testing(x)
  }else {
    res.template <- .generate_template_base(x)
  }  
  # attache HPLC plate position
  res.position <- .hplc_position(x = res.template, method = method, hplc = hplc)#$name) 

  # insert qc samples
  res.qc <- .insert_qc_samples(x = res.position,
                               how.often = how.often,
                               how.many = how.many,
                               hplc = hplc,#$name,
                               qc.type = qc.type)
  # clean up the sample queue
  res.queue <- .clean_up_queue(x = res.qc, hplc = hplc)
  # generate folder name acc. FGCZ naming convention
  res.folder <- .generate_folder_name(x = res.queue,
                                      foldername = foldername, 
                                      area = area, 
                                      instrument = instrument, 
                                      username = username,
                                      pathprefixsep = pathprefixsep)
  # generate file name  
  res.filename <- .generate_name(x = res.queue, showcondition = showcondition)
                                 
  
  cbind('File Name' = res.filename,
        'Path' = paste(pathprefix, paste('p', projectid, sep=''), res.folder, sep=pathprefixsep), 
        'Position' = res.queue$position,
        'Inj Vol' = 2
  )
}