## place Christian Trachsels code here
InsertFetuin <- function(df, group, rowsneeded)
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
  v <- unique(res$Condition)
  n <-length(v)
  for(i in 1:n){
    sub <- res[which(res$Condition == v[i]),]
    bind <- sub[order(rank(sample(1:nrow(res[which(res$Condition == v[i]),])))),]
    df <- rbind(df, bind)  
  }
  print(df)  
}

#helper function1
.format_input_data <- function(data, multiple = 2, hplc = ""){
  n <- nrow(data)
  res <- data
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
  if (multiple > 1) {
    res["extract.name"] <- paste(res$extract.name, rep(LETTERS[1:multiple], times = n), sep = "_") #attaches a letter suffix to the extract name to make them unique again in case multiple injections are requested
  } else {
    
  }
    res
} 

#helper function 2
.generate_template_random_multiple <- function(res, how.often = 2, how.many = 1, hplc = ""){
  n <- nrow(res)
  repeats <- length(unique(res$Condition))
  cond <- max(table(res$Condition))
  res <- block_randomizer(res)
  condition.vector <- as.vector(replicate(repeats, 1:cond))
  blockgroupindex <- which(colnames(res) == "Condition")[1]
  res <- InsertFetuin(res, blockgroupindex, cond)
  res["blockrandom"] <- condition.vector
  res <- res[order(rank(res$blockrandom)),]
  res$blockrandom <- NULL
  m <- nrow(res)
  blocks <- rep(1:((m/how.often)+1), each = how.often)[1:m] #generates an index among which the dataset will be extended
  res["blocks"] <- blocks #attache the index to the data
  groupindex <- which(colnames(res) == "blocks")[1] #fetch the index of the column containing the block information
  rowsneeded <- how.often + how.many #define by how many row each block will be extended
  res[sapply(res, is.factor)] <- lapply(res[sapply(res, is.factor)], as.character)
  res$extract.name[is.na(res$extract.name)] <- "tobedeleted"
  res <- InsertFetuin(res, groupindex, rowsneeded) #extend the sample list by empty rows
  res[sapply(res, is.factor)] <- lapply(res[sapply(res, is.factor)], as.character) #convert all Factors to characters
  if (hplc == "easylc"){
    res$Position[is.na(res$Position)] <- "F8"
  } else if ( hplc == "waters"){
    res$Position[is.na(res$Position)] <- '"1:F,8"'
  } else {
    res$Position[is.na(res$Position)] <- "1F08"
  }
  res$extract.name[is.na(res$extract.name)] <- "Fetuin_400amol"
  res$Condition[is.na(res$Condition)] <- "Fetuin"
  res <- res[-which(res$extract.name == "tobedeleted"),]  
    res
}

.generate_template_base <- function(data, how.often = 2, how.many = 1){
  n <- nrow(data)
  blocks <- rep(1:((n / how.often) + 1), each = how.often)[1:n] #generates an index among which the dataset will be extended
  #m.pos <- paste(rep(1, times = length(data$Sample.Name)), paste(rep(LETTERS[1:6], each = 8)[1:length(data$Sample.Name)], rep(1:8, times = 8)[1:length(data$Sample.Name)], sep = ","), sep = ":") #for M-Class position
  nano.pos <- paste(rep(LETTERS[1:6], each = 8)[1:n], 
                    rep(1:8, times = 8)[1:n], sep = "") #for nano easy position

  res <- data
 
  res["Position"] <- nano.pos #attache rack position
  res["blocks"] <- blocks #attache the index to the data
  str(res)
  groupindex <- which(colnames(res) == "blocks")[1] #fetch the index of the column containing the block information
  
  rowsneeded <- how.often + how.many #define by how many row each block will be extended
  
  res <- InsertFetuin(res, groupindex, rowsneeded) #extend the sample list by empty rows
  
  res$extract.name <- as.character(res$extract.name) #change Sample.Names to character
  
  res$Position[is.na(res$Position)] <- "F8"
  res$extract.id[is.na(res$extract.id)] <- ""
  
  print(res)
  res$extract.name[is.na(res$extract.name)] <- "Fetuin_400amol"
  
  res$Condition[is.na(res$Condition)] <- "Fetuin"
  
  res
}

.generate_folder_name <- function(foldername, area = "Proteomics", instrument, username = "bfabricusername",   res){
  n <- nrow(res)
  rundate <- format(Sys.Date(), format = "%Y%m%d") #produce the date in YYYYMMDD format
  out <- paste(username, rundate, sep = "_")
  if (foldername != ''){
    out <- paste(out, gsub('([[:punct:]])|\\s+', '_', foldername), sep = "_") #concatenate user name and folder name
  }
  out <- paste(area, instrument, out, sep = "\\") #concatenate instrument path strucutre with user name and folder name
  out <- rep(out, times = n)
  out
}

.generate_name <- function(res){
  n <- nrow(res)
  rundate <- format(Sys.Date(), format = "%Y%m%d") #produce the date in YYYYMMDD format
  injection.index <- sprintf("%02d", c(1:n))
  injection.name <- paste(paste(paste(rundate, injection.index, sep = "_"), res$extract.id, sep = "_"), res$extract.name, sep = "_")
  injection.name
}

generate_queue <- function(data, foldername='',projectid=1000, area='Proteomics', instrument='NA', username='cpanse', how.often=2, how.many=1, multiple = 1, hplc = "easylc"){
  res.1 <- .format_input_data(data, multiple, hplc)
  res.2 <- .generate_template_base(res=res.1, how.often, how.many, hplc)
  res.3 <- .generate_folder_name(foldername=foldername, area=area, instrument=instrument, username=username, res=res.2)
  res.4 <- .generate_name(res=res.2)
  #queue <- data.frame(cbind(nam, out, res$Position, InjVol))
  #colnames(queue) <- c("File Name", "Path", "Position", "Inj Vol")
  cbind('File Name' = res.4,
        'Path' = paste("D:\\Data2San", paste('p',projectid,sep=''), res.3, sep="\\"), 
        'Position' = res.2$Position,
        'Inj Vol' = 2
        )
}
