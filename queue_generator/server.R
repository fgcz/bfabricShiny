
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
 
  groupindex <- which(colnames(res) == "blocks")[1] #fetch the index of the column containing the block information
  
  rowsneeded <- how.often + how.many #define by how many row each block will be extended
  
  res <- InsertFetuin(res, groupindex, rowsneeded) #extend the sample list by empty rows
  
  res$extract.name <- as.character(res$extract.name) #change Sample.Names to character
  
  res[is.na(res)] <- paste(rep("Fetuin_400amol", each = length(res[is.na(res)]))) #fill in sample names of new inserted columns
  
  res$Condition <- as.character(res$Condition) #convert condition to factor
  
  res[is.na(res)] <- rep("Fetuin", each = length(res[is.na(res)])) #fill in condition of new inserted column
  
  res$Position <- as.character(res$Position) #convert Position to factor
  
  res[is.na(res)] <- rep("F8", each = length(res[is.na(res)])) #fill in condition of new inserted column
  
  res
}

generate_queue <- function(data, how.often=2, how.many=1, username='cpanse', instrument='NA', foldername='NA'){
  res <- .generate_template_base(data, how.often, how.many)
  #out <- .generate_folder(username, instrument, foldername, res=res)
  #nam <- .generate_name(res=res)
  #queue <- data.frame(cbind(nam, out, res$Position, InjVol))
  #colnames(queue) <- c("File Name", "Path", "Position", "Inj Vol")
  res
}

##
shinyServer(function(input, output, session) {

  
  output$instrument <- renderUI({
    res.instrument <- c('VELOS_1',
                    'VELOS_2',
                    'ORBI_1',
                    'ORBI_2',
                    'ORBI_3',
                    'G2HD_1',
                    'LTQ_1',
                    'LTQFT_1',
                    'QTRAP_1',
                    'TSQ_1',
                    'TSQ_2',
                    'QEXACTIVE_1',
                    'QEXACTIVE_2',
                    'QEXACTIVE_3',
                    'FUSION_1',
                    'FUSION_2',
                    'QEXACTIVEHF_1',
                    'QEXACTIVEHF_2',
                    'QEXACTIVEHF_3')

    selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
  })
  
  
  output$project <- renderUI({
    res.project <- c(NA, 1000, 1959, 2121)
    selectInput('project', 'Project:', res.project, multiple = FALSE, selected = NA)
  })
  
  
  getSample <- reactive({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/projectid/", input$project, sep='')))
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
   res[, "instrument"] <- input$instrument
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
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
  
    
    
    # call Christian Trachsel's code here!
 
   
    
   # print (input$extract)
    if (input$extract != "" && length(input$extract)>0){
      res <- getExtracts()
      idx <- res$extract.name %in% input$extract
      generate_queue(data=res[idx, ])
    }else{
      #as.data.frame(list(extract.name=NA, sampleid=NA, extract.id=NA))
      as.data.frame(list(output="no data - select extracts first."))
    }
    
    }))

})
