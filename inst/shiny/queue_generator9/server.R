
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# https://github.com/cpanse/bfabricShiny
#

library(bfabricShiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(DT)

 
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(wuid = NULL)
  # ---- getInstruments ---- 
  getInstrument <- reactive({list(#VELOS_1='Xcalibur',
    #VELOS_2='Xcalibur',
    #G2HD_1='MassLynx',
    #QTRAP_1='Xcalibur',
    #TSQ_1='Xcalibur',
    #TSQ_2='Xcalibur',
    QEXACTIVE_2='Xcalibur',
    #QEXACTIVE_3='Xcalibur',
    FUSION_1='Xcalibur',
    FUSION_2='Xcalibur',
    QEXACTIVEHF_1='Xcalibur',
    QEXACTIVEHF_2='Xcalibur',
    QEXACTIVEHFX_1='Xcalibur'
    #IMSTOF_1='TOFWERK'
  )})
  
  
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
  # ---- getHPLCparameter ---- 
  getHPLCparameter <- function(){list(VELOS_1 = c('eksigent', list(.eksigent()), 'F08', 'F07', 'F07','F06'),
                                      VELOS_2 = c('eksigent', list(.eksigent()),'F08', 'F07', 'F07','F06'),
                                      G2HD_1 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                      QTRAP_1 = c('eksigent', list(.eksigent()), 'F08', 'F07', 'F07','F06'),
                                      TSQ_1 = c('eksigent', list(.eksigent()),'F08', 'F07', 'F07','F06'),
                                      TSQ_2 = c('eksigent', list(.eksigent()), 'F08', 'F07', 'F07','F06'),
                                      QEXACTIVE_2 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                      QEXACTIVE_3 = c('easylc', list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                      FUSION_1 = c('easylc',list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                      FUSION_2 = c('easylc', list(.easylc()), 'F8', 'F7', 'F7','F6'),
                                      QEXACTIVEHF_1 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                      QEXACTIVEHF_2 = c('waters', list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                      QEXACTIVEHFX_1 = c('waters',list(.waters()), '"1:F,8"', '"1:F,7"', '"1:F,7"', '"1:F,6"'),
                                      IMSTOF_1 = c('eksigent', list(.eksigent()), 'F08', 'F07', 'F07','F06'))}
  
  getQCsample <- function(){list(extract.name = c('autoQC01', 'autoQC02', 'autoQC4L', 'clean'),
                                 extract.Condition = c(as.character(NA), as.character(NA), as.character(NA), as.character(NA)),
                                 extract.Condition = c('autoQC01', 'autoQC02', 'autoQC4L', 'clean'),
                                 position = c(3, 4, 5, 6))}
  #output list ----
  
  output$area <- renderUI(({
    res.area <- c("Proteomics", "Metabolomics")
    selectInput('area', 'Area:', res.area, multiple = FALSE, selected = res.area[1])
  }))
  
  output$folder <- renderUI(({
    textInput('folder', 'Data Folder Name:', "", width = NULL, placeholder ="enter your folder name here")
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
    #res.project <- c(NA, 1000, 1959, 2121)
    numericInput('project', 'Project:', value = 3000,  min = 1000, max = 3500, width=100)
  })
  
  output$instrument <- renderUI({
    res.instrument <- names(getInstrument())
    selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
  })
  
  output$method <- renderUI(({
    selectInput('method', 'Queue Method:', c('default', 'random', 'blockrandom', 'PRM', 'testing'), multiple = FALSE, selected = 'default')
  }))
  
  output$showcondition <- renderUI(({
    checkboxInput('showcondition', 'Insert condition into sample name:', value = FALSE)
  }))
  
  output$start1 <- renderUI({
    selectInput('start1', "Start 1", c(select = '', autoQC01 = 1, autoQC02 = 2, autoQC4L = 3, clean = 4), selected = 1,multiple = FALSE, selectize = TRUE)
  })
  
  output$start2 <- renderUI({
    selectInput('start2', "Start 2", c(select = '', autoQC01 = 1, autoQC02 = 2, autoQC4L = 3, clean = 4), multiple = FALSE, selectize = TRUE)
  })
  
  output$start3 <- renderUI({
    selectInput('start3', "Start 3", c(select = '', autoQC01 = 1, autoQC02 = 2, autoQC4L = 3, clean = 4), multiple = FALSE, selectize = TRUE)
  })
  
  output$end1 <- renderUI({
    selectInput('end1', "End 1", c(select = '', autoQC01 = 1, autoQC02 = 2, autoQC4L = 3, clean = 4), selected = 4, multiple = FALSE, selectize = TRUE)
  })
  
  output$end2 <- renderUI({
    selectInput('end2', "End 2", c(select = '', autoQC01 = 1, autoQC02 = 2, autoQC4L = 3, clean = 4), selected = 1, multiple = FALSE, selectize = TRUE)
  })
  
  output$end3 <- renderUI({
    selectInput('end3', "End 3", c(select = '', autoQC01 = 1, autoQC02 = 2, autoQC4L = 3, clean = 4), selected = 3, multiple = FALSE, selectize = TRUE)
  })
  
  output$QC01m <- renderUI({
    numericInput("QC01m", h4("How many?"), min = 1, max = 8, step = 1, value = 1)
  })
  
  output$QC01o <- renderUI({
    numericInput("QC01o", h4("How often?"), min = 1, max = 5, step = 1, value = 4)
  })
  
  output$QC02m <- renderUI({
    numericInput("QC02m", h4("How many?"), min = 1, max = 8, step = 1, value = 1)
  })
  
  output$QC02o <- renderUI({
    numericInput("QC02o", h4("How often?"), min = 1, max = 5, step = 1, value = 4)
  })
  
  output$QC4Lm <- renderUI({
    numericInput("QC4Lm", h4("How many?"), min = 1, max = 8, step = 1, value = 1)
  })
  
  output$QC4Lo <- renderUI({
    numericInput("QC4Lo", h4("How often?"), min = 1, max = 5, step = 1, value = 4)
  })
  
  output$cleanm <- renderUI({
    numericInput("cleanm", h4("How many?"), min = 1, max = 8, step = 1, value = 1)
  })
  
  output$cleano <- renderUI({
    numericInput("cleano", h4("How often?"), min = 1, max = 5, step = 1, value = 4)
  })
  
  output$startposition <- renderUI({
    numericInput('startposition', 'Start injection count from:', min = 1, max = 500, step = 1, value = 1)
  })
  
  output$targets <- renderUI({
    numericInput('targets', 'Target lists:', min = 1, max = 20, step = 1, value = 1)
  })
  
  
  # -------checkbox FGCZ naming conventions -----
  
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
      idx <- rev(order(res$samples._id))
      res <- res[idx, ]
      selectInput('sample', 'Sample:', paste(res$samples._id, res$samples.name, sep='-'), size = 35, multiple = TRUE, selectize = FALSE)
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
    
    #generate the actual queue ----
    
    rv <- generate_queue(x = res,
                         foldername = input$folder,
                         projectid = input$project,
                         area = input$area,
                         instrument = input$instrument,
                         username = input$login,
                         nr.methods = as.integer(input$testmethods),
                         nr.replicates = as.integer(input$replicates),
                         method = as.character(input$method),
                         autoQC01 = input$autoQC01,
                         QC01m = input$QC01m,
                         QC01o = input$QC01o,
                         autoQC02 = input$autoQC02,
                         QC02m = input$QC02m,
                         QC02o = input$QC02o,
                         autoQC4L = input$autoQC4L,
                         QC4Lo = input$QC4Lo,
                         QC4Lm = input$QC4Lm,
                         clean = input$clean,
                         cleanm = input$cleanm,
                         cleano = input$cleano,
                         start1 = as.numeric(input$start1),
                         start2 = as.numeric(input$start2),
                         start3 = as.numeric(input$start3),
                         end1 = as.numeric(input$end1),
                         end2 = as.numeric(input$end2),
                         end3 = as.numeric(input$end3),
                         lists = input$targets,
                         startposition = input$startposition)
    
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
    rv
    
  })
  
  
  #----- DT::renderDataTable ----
  output$table <- DT::renderDataTable(DT::datatable({
    
    if (input$sample != "" && length(input$sample) >= 1){
      
       getBfabricContent()
      
    }else{
      
      as.data.frame(list(output="no data - select sample first."))
    }
    
  }, options = list(paging = FALSE)
  ))
  
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


