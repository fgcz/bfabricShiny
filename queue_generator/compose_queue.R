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

generate_queue <- function(data, foldername='',projectid=1000, area='Proteomics', instrument='NA', username='cpanse', how.often=2, how.many=1){
  res.1 <- .generate_template_base(data, how.often, how.many)
  res.2 <- .generate_folder_name(foldername=foldername, area=area, instrument=instrument, username=username, res=res.1)
  res.3 <- .generate_name(res=res.1)
  #queue <- data.frame(cbind(nam, out, res$Position, InjVol))
  #colnames(queue) <- c("File Name", "Path", "Position", "Inj Vol")
  cbind('File Name' = res.3,
        'Path' = paste("D:\\Data2San", paste('p',projectid,sep=''), res.2, sep="\\"), 
        'Position' = res.1$Position,
        'Inj Vol' = 2
        )
}
