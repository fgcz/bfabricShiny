#test data
test_data <- function(){
  extract.name <- c(paste("Sample", 1:20, sep = "_"))
  extract.id <- c(1:20)
  Condition <- c(rep("Control", 4), rep("Ampicillin", 4), rep("Kanamycin", 4), rep("Less", 3), rep("More", 5))
  data.frame(extract.name, extract.id, Condition)
}


#Format data for multiple injections and HPLC type
.format_input_data <- function(x, multiple = 2, hplc = ""){
  n <- nrow(x)
  res <- x
  if (hplc == "eksigent") {
    pos <- (paste(rep(2, times = n), paste(rep(LETTERS[1:6], each = 8)[1:n], rep(sprintf("%02d", c(1:8)), times = 8)[1:n], sep = ""), sep = "")) #generates plate position in eksigent format for a 48 position plate (A1-F8)
  } else if (hplc == "waters") {
    pos <- paste(rep(1, times = n), paste(rep(LETTERS[1:6], each = 8)[1:n], rep(1:8, times = 8)[1:n], sep = ","), sep = ":") #generates plate position in waters format for a 48 position plate (A1-F8)
    pos <- paste0('"', pos, '"')
  } else if (hplc == "easylc"){
    pos <- paste(rep(LETTERS[1:6], each = 8)[1:n], rep(1:8, times = 8)[1:n], sep = "") #generates plate position in thermo easyLC format for a 48 position plate (A1-F8)
  } else {
    pos <- paste(rep(LETTERS[1:6], each = 8)[1:n], rep(1:8, times = 8)[1:n], sep = "") #generates plate position in thermo easyLC format for a 48 position plate (A1-F8)
  }
  res["Position"] <- pos #attach corresponding plate position to queue table 
  m <- ncol(res)
  res <- res[rep(seq_len(nrow(res)), each = multiple),1:m] #multiplies the queue table entries in case multiple injections per samples are requested
  
  #TODO: IF AND condition check for method checking -> rep = multiple and times methods
  if (multiple > 1) {
    res["extract.name"] <- paste(res$extract.name, rep(LETTERS[1:multiple], times = n), sep = "_") #attaches a letter suffix to the extract name to make them unique again in case multiple injections are requested
  } else {
    
  }
  res
} 


#generate the queue according to the method selected

equilize_groups <- function(df, group, rowsneeded)
  #extendes a dataframe by any number of rows
  #the position and the number of rows can be defined
  #
  # Args: df -> a data.frame on which the function is applied
  #       group -> a column of the dataframe indicating a grouping of the data
  #       rowsneeded -> a number, indicated by how many rows a group is extended
  #
  # Returns: the original dataframe with additional rows inserterd (all groups will have the same amount of rows). inserted rows will contain <NA>
{
  do.call(rbind, lapply(split(df, df[[group]]), function(x){
    x <- data.frame(lapply(x, `length<-`, rowsneeded))
    x[group] <- x[[group]][1]
    x
  }))
}

block_randomizer <- function(x){
  df <- NULL
  v <- unique(x$Condition)
  n <-length(v)
  for(i in 1:n){
    sub <- x[which(x$Condition == v[i]),]
    bind <- sub[order(rank(sample(1:nrow(x[which(x$Condition == v[i]),])))),]
    df <- rbind(df, bind)  
  }
  df  
}

.generate_template_random_block <- function(x){
  n <- nrow(x)
  repeats <- length(unique(x$Condition))
  cond <- max(table(x$Condition))
  res <- block_randomizer(x)
  condition.vector <- as.vector(replicate(repeats, sprintf("%02d", c(1:cond))))
  blockgroupindex <- which(colnames(res) == "Condition")[1]
  res <- equilize_groups(res, blockgroupindex, cond)
  res["blockrandom"] <- condition.vector
  res <- res[order(rank(res$blockrandom)),]
  condition.vector.2 <- as.vector(replicate(repeats, sample(1:repeats)))
  resort <- paste(res$blockrandom, condition.vector.2, sep =".")
  res["blockrandom2"] <- resort
  #res[order(res$blockrandom, res$blockrandom2),]
  res <- res[order(res$blockrandom2),]
  res$blockrandom <- NULL
  res$blockrandom2 <- NULL
  res <- res[!is.na(res$extract.name),]  
  res
}

.generate_template_random <- function(x){
  n <- nrow(x)
  random.index <- sample(1:n)
  res <- x[order(random.index),]
  res
}

.generate_template_base <- function(x){
  res <- x
  res
}



#insert Fetuins and cleans into the generated queue
#TODO: remove Fetuins and cleans after the last sample and add a "startup and a finish sequence"

.insert_qc_samples <- function(x, how.often, how.many, hplc = "", equal= "yes"){
  n <- nrow(x)
  j <- floor(n/how.often)
  v <- how.often*(1:j)
  if (hplc == "easylc"){
    initialf <- as.character("F8")
    initialc <- as.character("F6")
  } else if ( hplc == "waters"){
    initialf <- '"1:F,8"'
    initialc <- '"1:F,6"'
  } else {
    initialf <- "1F08"
    initialc <- "1F06"
  }
  a <- j*how.many
  if(equal == "yes"){
    b <- j* how.many
    c <- how.many
  } else {
    b <- ceiling((j*how.many)/2)  
    c <- 1
  }
  fet <- data.frame(extract.name = rep("Fetuin_400amol",a), extract.id = rep(as.integer(NA),a), Condition = rep("Fetuin",a), Position = rep(initialf,a))
  clean <- data.frame(extract.name = rep("Clean",b), extract.id = rep(as.integer(NA),b), Condition = rep("Clean",b), Position = rep(initialc,b))
  res <- rbind(x, fet, clean)
  indf <- v
  odd <- c(FALSE, TRUE)
  if (equal =="yes"){
    indc <-v
  } else{
    indc <- v[!odd]
  }
  id  <- c(seq_along(x$extract.name), rep(indf+0.75, each = how.many) , rep(indc+0.25, each = c))
  res["idx"] <- id
  res <- res[order(res$idx),]
  res$idx <-NULL
  res
}  