
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(jsonlite)

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

##
shinyServer(function(input, output, session) {

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
      write.csv(cat("Bracket Type=4\n", file = file, append = FALSE))
      write.table(getContent(), sep=',', file = file, row.names = FALSE, append = TRUE, quote = FALSE)
    }
  )
  
  
  getContent <- reactive({
    res <- getExtracts()
    res[, "instrument"] <- input$instrument
    idx <- res$extract.name %in% input$extract
    generate_queue(data=res[idx, ], area = input$area,
                   foldername = "",
                   projectid=input$project,
                   username = input$login,
                   how.often = as.integer(input$howoften),
                   how.many = as.integer(input$howmany),
                   instrument = input$instrument)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
  
    # call Christian Trachsel's code here!
   # print (input$extract)
    if (input$extract != "" && length(input$extract) > 0){
      getContent()
    }else{
      #as.data.frame(list(extract.name=NA, sampleid=NA, extract.id=NA))
      as.data.frame(list(output="no data - select extracts first."))
    }
    
    }))

})
