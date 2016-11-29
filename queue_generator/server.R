
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(jsonlite)
library(httr)

#source('compose_queue.R')

#R

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
  v <- unique(x$Condition)
  n <-length(v)
  for(i in 1:n){
    sub <- x[which(x$Condition == v[i]),]
    bind <- sub[order(rank(sample(1:nrow(x[which(x$Condition == v[i]),])))),]
    df <- rbind(df, bind)  
  }
  df  
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
.generate_template_random_block <- function(x, how.often = 2, how.many = 1, hplc = ""){
  n <- nrow(x)
  repeats <- length(unique(x$Condition))
  cond <- max(table(x$Condition))
  res <- block_randomizer(x)
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
# TODO(cp): find the bug
  #res <- res[-which(res$extract.name == "tobedeleted"),]  
  res
}

.generate_template_random <- function(x, how.often = 2, how.many = 1, multiple = 2){
  n <- nrow(x)
  random.index <- sample(1:n)
  res <- x[order(random.index),]
  nano.pos <- paste(rep(LETTERS[1:6], each = 8)[1:n], rep(1:8, times = 8)[1:n], sep = "") #for nano easy position
  res["Position"] <- nano.pos #attache rack position 
  res <- res[rep(seq_len(nrow(res)), each = multiple),1:4] #each or times as argument for replication?
  m <- nrow(res)
  blocks <- rep(1:((m/how.often)+1), each = how.often)[1:m] #generates an index among which the dataset will be extended
  #m.pos <- paste(rep(1, times = n)), paste(rep(LETTERS[1:6], each = 8)[1:n], rep(1:8, times = 8)[1:n], sep = ","), sep = ":") #for M-Class position
  res["blocks"] <- blocks #attache the index to the data
  groupindex <- which(colnames(res) == "blocks")[1] #fetch the index of the column containing the block information
  rowsneeded <- how.often + how.many #define by how many row each block will be extended
  res <- InsertFetuin(res, groupindex, rowsneeded) #extend the sample list by empty rows
  res[sapply(res, is.factor)] <- lapply(res[sapply(res, is.factor)], as.character) #convert all Factors to characters
  res$Position[is.na(res$Position)] <- "F8"
  res$extract.name[is.na(res$extract.name)] <- "Fetuin_400amol"
  res$Condition[is.na(res$Condition)] <- "Fetuin"
  res
}

.generate_template_base <- function(x, how.often = 2, how.many = 1, hplc=''){
  n <- nrow(x)
  blocks <- rep(1:((n / how.often) + 1), each = how.often)[1:n] #generates an index among which the dataset will be extended

  nano.pos <- paste(rep(LETTERS[1:6], each = 8)[1:n], 
                    rep(1:8, times = 8)[1:n], sep = "") #for nano easy position

  res <- x
 
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

# main method for queue generation
generate_queue <- function(x, foldername='', 
	projectid=1000, 
	area='Proteomics', 
	instrument='FUSION_1', 
	username='cpanse', 
	how.often=2, how.many=1, multiple = 1, hplc = "easylc",
	method='default'){

  if (!'Condition' %in% names(x)){
	  x$Condition <- "C"
  }
	
  res.1 <- .format_input_data(x, multiple, hplc)
	

  if (method == 'default'){
  	res.2 <- .generate_template_base(x=res.1, how.often, how.many, hplc)
  }else if(method == 'random'){
    res.2 <- .generate_template_random(x=res.1, how.often, how.many, hplc)
  }else if(method == 'blockrandom'){
    res.2 <- .generate_template_random_block(x=res.1, how.often, how.many, hplc)
  }else{
	# testing
 	res.2 <- .generate_template_random_multiple(res=res.1, how.often, how.many, hplc)
  }

  res.3 <- .generate_folder_name(foldername=foldername, area=area, instrument=instrument, username=username, res=res.2)
  res.4 <- .generate_name(res=res.2)

  cbind('File Name' = res.4,
        'Path' = paste("D:\\Data2San", paste('p',projectid, sep=''), res.3, sep="\\"), 
       'Position' = res.2$Position,
        'Inj Vol' = 2
        )
}



##
shinyServer(function(input, output, session) {

 # TODOO(cp):
getHPLC <- reactive({list(VELOS_1='eksigent',
                       VELOS_2='eksigent',
                       G2HD_1='waters',
                       QTRAP_1='eksigent',
                       TSQ_1='eksigent',
                       TSQ_2='eksigent',
                       QEXACTIVE_2='easylc',
                       QEXACTIVE_3='easylc',
                       FUSION_1='easylc',
                       FUSION_2='easylc',
                       QEXACTIVEHF_1='waters',
                       QEXACTIVEHF_2='waters')})
	
  getInstrument <- reactive({list(VELOS_1='Xcalibur',
                       VELOS_2='Xcalibur',
                       G2HD_1='MassLynx',
                       QTRAP_1='Xcalibur',
                       TSQ_1='Xcalibur',
                       TSQ_2='Xcalibur',
                       QEXACTIVE_2='Xcalibur',
                       QEXACTIVE_3='Xcalibur',
                       FUSION_1='Xcalibur',
                       FUSION_2='Xcalibur',
                       QEXACTIVEHF_1='Xcalibur',
                       QEXACTIVEHF_2='Xcalibur')})
  
  
  getInstrumentSuffix <- reactive({list(VELOS_1='RAW',
                                  VELOS_2='RAW',
                                  G2HD_1='wiff',
                                  QTRAP_1='wiff',
                                  TSQ_1='RAW',
                                  TSQ_2='RAW',
                                  QEXACTIVE_2='raw',
                                  QEXACTIVE_3='raw',
                                  FUSION_1='raw',
                                  FUSION_2='raw',
                                  QEXACTIVEHF_1='raw',
                                  QEXACTIVEHF_2='raw')})
  
  output$area <- renderUI(({
    res.area <- c("Proteomics", "Metabolomics")
    selectInput('area', 'Area:', res.area, multiple = FALSE, selected = res.area[1])
  }))
  
  output$project <- renderUI({
    res.project <- c(NA, 1000, 1959, 2121)
    numericInput('project', 'Project:', value = res.project,  min = 1000, max = 2500, width=100)
  })
  
  output$howoften <- renderUI({
    res.howoften <- 1:5
    selectInput('howoften', 'How often?:', res.howoften, multiple = FALSE, selected = 2)
  })
  
  output$howmany <- renderUI({
    res.howmany <- 1:3
    selectInput('howmany', 'How many?:', res.howmany, multiple = FALSE, selected = 1)
  })
  
  
  output$instrument <- renderUI({
    res.instrument <- names(getInstrument())
    selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
  })
  

  output$method <- renderUI(({
    selectInput('method', 'Method:', c('default', 'random', 'blockrandom', 'testing'), multiple = FALSE, selected = 'default')
  }))
  
  getSample <- reactive({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/projectid/", input$project, sep='')))
  })
  
  getLogin <- reactive({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/user/", input$project, sep='')))
    res$user
  })
  
  
  output$login <- renderUI({
    if ( input$project %in% 1000:2500){
      selectInput('login', 'Login:', as.character(getLogin()), multiple = FALSE)
    }else{
      selectInput('login', 'Login:', NULL)
    }
  })
  
  
  output$sample <- renderUI({
    if ( input$project %in% 1000:2500){
      res <- getSample()
      selectInput('sample', 'Sample:', paste(res$sample.id, res$sample.name, sep='-'), multiple = TRUE)
    }else{
      selectInput('sample', 'Sample:', NULL)
    }
  })
  

  getExtracts <- reactive({
    sample.id <- sapply(strsplit(input$sample, split='-'), function(x){x[1]})
    res <- do.call('rbind', 
                   lapply(sample.id, 
                          function(sampleid){
                            as.data.frame(fromJSON(paste("http://localhost:5000/sampleid/", sampleid, sep='')))
                            #rv$project.id <- input$project
                            #rv
                          }))
    
   res[, "project.id"]  <- input$project
   
   res[, "Condition"] <- "A"
   res
  })
  
  output$extract <- renderUI({

    if (input$sample == "" || length(input$sample) == 0 || is.null(input$sample)){
      selectInput('extract', 'Extract:', NULL)
    }   else{
      
      res <- getExtracts()
      selectInput('extract', 'Extract:', res$extract.name, multiple = TRUE)  
       
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { "fgcz_queue_test.csv" },
      # paste(unlist(strsplit(input$file, split="[.]"))[1], "csv", sep=".")  },
    content = function(file) {
      write.csv(cat("Bracket Type=4\r\n", file = file, append = FALSE))
      res <- getBfabricContent()
      res <- res[,1:4]
      write.table(res, sep=',', file = file, row.names = FALSE, append = TRUE, quote = FALSE, eol='\r\n')

    }
  )
  
  datasetID <- observeEvent(input$bfabricButton, {

	S <- getBfabricContent()
	if (nrow(S) > 0){
      		rv <- POST(paste("http://localhost:5000/add_dataset", input$project, sep='/'), body = toJSON(getBfabricContent()))

		 observe({
       		session$sendCustomMessage(type = 'testmessage', message = 'try to commit as dataset to bfabric.') 
		 })
	}else{
       		#session$sendCustomMessage(type = 'testmessage', message = 'not enough lines.') 
	}
      
      })
  
  #getContent <- reactive({
  #  res <- getExtracts()
  #  res[, "instrument"] <- input$instrument
  #  idx <- res$extract.name %in% input$extract

  getBfabricContent <- reactive({

    res <- getExtracts()
   # if (nrow(res)>0){

    	res[, "instrument"] <- input$instrument

    	idx <- res$extract.name %in% input$extract

    	rv <- generate_queue(x = res[idx, ],
		   method=input$method,
		   area = input$area,
                   foldername = "",
                   projectid=input$project,
                   username = input$login,
                   how.often = as.integer(input$howoften),
                   how.many = as.integer(input$howmany),
                   instrument = input$instrument)

    	cbind(rv, extract.id = res[idx, 'extract.id'])


    #}else{
    #        return NULL
    #}
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
  
    # call Christian Trachsel's code here!
   # print (input$extract)
    if (input$extract != "" && length(input$extract) > 0){
      getBfabricContent()
    }else{
      #as.data.frame(list(extract.name=NA, sampleid=NA, extract.id=NA))
      as.data.frame(list(output="no data - select extracts first."))
    }
    
    }))

})
