
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# https://github.com/cpanse/bfabricShiny
#

library(bfabricShiny)
library(jsonlite)
library(httr)
library(DT)

##
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(wuid = NULL)
  
  # TODOO(cp):
  getHPLC <- function(){list(VELOS_1='eksigent',
                             VELOS_2='eksigent',
                             G2HD_1='waters',
                             QTRAP_1='eksigent',
                             TSQ_1='eksigent',
                             TSQ_2='eksigent',
                             QEXACTIVE_2='waters',
                             QEXACTIVE_3='easylc',
                             FUSION_1='easylc',
                             FUSION_2='easylc',
                             QEXACTIVEHF_1='waters',
                             QEXACTIVEHFX_1='waters',
                             QEXACTIVEHF_2='waters',
                             IMSTOF_1='eksigent')}
  
  
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
                                  QEXACTIVEHFX_1='Xcalibur',
                                  QEXACTIVEHF_2='Xcalibur',
                                  IMSTOF_1='TOFWERK')})
  
  
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
                                        QEXACTIVEHFX_1='raw',
                                        QEXACTIVEHF_2='raw',
                                        IMSTOF_1='h5')})
  
  output$area <- renderUI(({
    res.area <- c("Proteomics", "Metabolomics")
    selectInput('area', 'Area:', res.area, multiple = FALSE, selected = res.area[1])
  }))
  
  output$folder <- renderUI(({
    textInput('folder', 'Data Folder Name:', "", width = NULL, placeholder ="enter your folder name here")
  }))
  
  output$qctype <- renderUI(({
    selectInput('qctype', 'Type of sample QC:', 
                choices = list("autoQC01" = 1, "autoQC01 and clean" = 2, "autoQC01 and clean every second " = 3),
                selected = 1)
  }))
  
  output$testmethods <- renderUI(({
    res.testmethods <- 1:5
    selectInput('testmethods', 'Number of methods to test:', res.testmethods, multiple = FALSE, selected = res.testmethods[1])
  }))
  
  output$replicates <- renderUI(({
    res.replicates <- 1:9
    selectInput('replicates', 'Number of injections for each method:', res.replicates, multiple = FALSE, selected = res.replicates[1])
  }))
  
  output$project <- renderUI({
    res.project <- c(NA, 1000, 1959, 2121)
    numericInput('project', 'Project:', value = 2066,  min = 1000, max = 2500, width=100)
  })
  
  output$howoften <- renderUI({
    res.howoften <- 1:8
    selectInput('howoften', 'Insert QC sample every:', res.howoften, multiple = FALSE, selected = res.howoften[1])
  })
  
  output$howmany <- renderUI({
    res.howmany <- 1:5
    selectInput('howmany', 'Number of QC samples inserted:', res.howmany, multiple = FALSE, selected = 1)
  })
  
  output$instrument <- renderUI({
    res.instrument <- names(getInstrument())
    selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
  })
  
  
  output$method <- renderUI(({
    selectInput('method', 'Queue Method:', c('default', 'random', 'blockrandom', 'testing'), multiple = FALSE, selected = 'default')
  }))
  
  output$showcondition <- renderUI(({
    checkboxInput('showcondition', 'Insert condition into sample name:', value = FALSE)
  }))
  
# -------checkbox FGCZ naming conventions -----
  output$hubify <- renderUI(({
    checkboxInput('hubify', 'follow FGCZ naming conventions', value = TRUE)
  }))
  
  getLogin <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching user data ...")
    on.exit(progress$close())
    
    if (is.null(input$project)){
      return (NULL)
    }else{
      res <- as.data.frame(fromJSON(paste("http://localhost:5000/user/", 
                                          input$project, sep='')))
      message(paste('got', nrow(res), 'users.'))
      return (res$user)
    }
  })
  
  
  # res <- as.data.frame(fromJSON("http://localhost:5000/sample/2066"))
  getSample <- reactive({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching sample data ...")
    on.exit(progress$close())
    
    
    if (is.null(input$project)){
      return (NULL)
    }else{
      sampleURL <- paste("http://localhost:5000/sample/", 
                         input$project, sep='')
      
      message(sampleURL)
      res <- as.data.frame(fromJSON(sampleURL))
      
      
      
      message(paste('got', nrow(res), 'samples.'))
      
      return (res)
    }
  })
  
  
  output$sample <- renderUI({
    res <- getSample()
    
    if (is.null(res)){
      selectInput('sample', 'Sample:', NULL)
    }else{
      #res <- getSample()
      
      selectInput('sample', 'Sample:', paste(res$samples._id, res$samples.name, sep='-'), multiple = TRUE)
    }
  })
  output$login <- renderUI({
    res <- getLogin()
    if (!is.null(res)){
      selectInput('login', 'Login:', as.character(res), multiple = FALSE)
    }else{
      selectInput('login', 'Login:', NULL)
    }
  })
  
  
  getResourcename <- reactive({
    paste("fgcz-queue-generator_p", input$project, "_", input$instrument, "_", format(Sys.time(), "%Y%m%d"), ".csv", sep='')
  })
  
  
#------------------------ getBfabricContent ----
  getBfabricContent <- reactive({
    if (is.null(input$sample))
    {return(NULL)}
    if (length(input$sample) == 1 && input$sample == ""){
      return (NULL)
    }
    values$wuid <- NULL
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "counting  lambdas 11 ...")
    on.exit(progress$close())
    
    res <- getSample()
    
    res[, "instrument"] <- input$instrument
    
    idx.filter <- (paste(res$samples._id, res$samples.name, sep="-") %in% input$sample)
    
    
    res <- res[idx.filter, c("samples.name", "samples._id", "samples.condition")]
    
    # TODO(cp): replace extract by sample
    names(res) <- c("extract.name", "extract.id", "extract.Condition")
    
    if(any(is.na(res$extract.Condition))){
      
      res$extract.Condition[is.na(res$extract.Condition)] <- "A"
      
    } else{
      
    }
    
    idx.hplc <- getHPLC()[[input$instrument]]
    
    rv <- generate_queue(x = res, 
                         foldername = input$folder,
                         projectid=input$project,
                         area = input$area,
                         instrument = input$instrument,
                         username = input$login,
                         hplc = idx.hplc,
                         how.often = as.integer(input$howoften),
                         how.many = as.integer(input$howmany),
                         nr.methods = as.integer(input$testmethods),
                         nr.replicates = as.integer(input$replicates),
                         showcondition = input$showcondition,
                         qc.type = as.integer(input$qctype),
                         method = as.character(input$method))
    if (input$hubify){
      rv[, 'File Name' ] <- gsub("[^-a-zA-Z0-9_]", "_", rv[, 'File Name' ])
    }
    rv
    
  })
  
  
#----- DT::renderDataTable ----
  output$table <- DT::renderDataTable(DT::datatable({
   
    if (input$sample != "" && length(input$sample) >= 1){
    
      getBfabricContent()
      
    }else{
    
      as.data.frame(list(output="no data - select sample first."))
    }
    
  }))
  
  output$download <- renderUI({
    res <- getBfabricContent()
    
    if (is.null(res)){
      HTML("Download not possible yet.")
      
    }else if (!is.null(values$wuid)){
      # https://fgcz-bfabric-test.uzh.ch/bfabric/userlab/show-workunit.html?id=154014
      actionButton("download",
                   paste("bfabric download workunit", values$wuid),
                   onclick = paste("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=", 
                                   values$wuid, "', '_blank')", sep=''))
    }else{
      actionButton('generate', 'Download configuration')
    }
    
  })
  
#------------------- bfabricUpload --------
  bfabricUpload <- observeEvent(input$generate, {
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "upload configuration to bfabric")
    on.exit(progress$close())
    
    
    res <- getBfabricContent()
    
    
    ########################## WRITE CSV TO BFABRIC
    fn <- tempfile()#pattern = "file", tmpdir = tempdir(), fileext = ".csv")[1]
    print (fn)
    cat("Bracket Type=4\r\n", file = fn, append = FALSE)
    write.table(res, file = fn, 
                sep=',', row.names = FALSE, 
                append = TRUE, quote = FALSE, eol='\r\n')
    
    print ("ALIVE")
    file_content <- base64encode(readBin(fn, "raw", file.info(fn)[1, "size"]), 'csv')
    
    rv <- POST("http://localhost:5000/add_resource",
               body = toJSON(list(base64=file_content,
                                  name='MS configuration',
                                  projectid=input$project, 
                                  applicationid=212,
                                  workunitdescription = paste("The spreadsheet contains a ", input$instrument,
                                                              " queue configuration having ", nrow(res), " rows.\n", 
                                                              "The parameters are:\n", 
                                                              "\thow.often: ", as.integer(input$howoften), "\n",
                                                              "\thow.many:  ", as.integer(input$howmany), "\n",
                                                              "\tnr.methods:",  as.integer(input$testmethods), "\n",
                                                              "\tnr.replicates:", as.integer(input$replicates), "\n",
                                                              "\tshowcondition: ",  input$showcondition, "\n",
                                                              "\tqc.type: ", as.integer(input$qctype), "\n",
                                                              "\tmethod: ",  as.character(input$method), 
                                                              "\nThe resource was generated by using the R package bfabricShiny version ",
                                                              packageVersion('bfabricShiny'), ".\n", sep=''),
                                  resourcename = getResourcename())
               ))
    
   
    wuid <-  (content(rv)$workunit_id)
    values$wuid <- wuid
    ########################## WRITE CSV TO BFABRIC
  }
  )
  
})

