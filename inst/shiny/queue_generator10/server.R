
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
  getInstrument <- reactive({
    list(
      #QEXACTIVE_1='Xcalibur',
      QEXACTIVE_2='Xcalibur',
      QEXACTIVEHF_2='Xcalibur',
      QEXACTIVEHF_4='Xcalibur',
      QEXACTIVEHFX_1='Xcalibur',
      FUSION_1='Xcalibur',
      FUSION_2='Xcalibur',
      LUMOS_1='Xcalibur'
    )})


  getInstrumentSuffix <- reactive({
    list(VELOS_1='RAW',
         VELOS_2='RAW',
         G2HD_1='wiff',
         QTRAP_1='wiff',
         TSQ_1='RAW',
         TSQ_2='RAW',
         #QEXACTIVE_1='raw',
         QEXACTIVE_2='raw',
         QEXACTIVE_3='raw',
         FUSION_1='raw',
         FUSION_2='raw',
         QEXACTIVEHF_1='raw',
         QEXACTIVEHF_2='raw',
         QEXACTIVEHF_4='raw',
         QEXACTIVEHFX_1='raw',
         LUMOS_1='raw',
         IMSTOF_1='h5')})

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
    if (input$containerType == 'project'){
      numericInput('project', 'Container:', value = 3530,  min = 1000, max = 3500, width=100)
    }
    else{
      # numericInput('project', 'Container:', value = 3181,  min = 1000, max = 3500, width=100)
      textInput('project', 'Container:', value = "3181,3492", width = 1000, placeholder = 'comma separated list of order IDs')
    }
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

  # res <- as.data.frame(fromJSON(paste("http://localhost:5000/user/",  3000, sep='')))
  getLogin <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching user data ...")
    on.exit(progress$close())

    if (is.null(input$project) || grepl(",", input$project)) {
      return(NULL)
    }else{
      res <- as.data.frame(fromJSON(paste("http://localhost:5000/user/",
                                          input$project, sep = '')))
      message(paste('got', nrow(res), 'users.'))
      return(res$user.login)
    }
  })

  #---- getSample ------
  # res <- as.data.frame(fromJSON("http://localhost:5000/sample/2066"))
  .restSample <- function(container=3181, url="http://localhost:5000/sample/"){

    if (is.numeric(container) & container > 999) {
      sampleURL <- paste(url, container, sep = '')

      if (require(RCurl)) {
        if (RCurl::url.exists(sampleURL) == FALSE) {
          message(paste("URL", sampleURL, "for container", container, "does not exists."))
          return(NULL)
        }
      }

      out <- tryCatch({

        progress <- shiny::Progress$new(session = session, min = 0, max = 1)
        progress$set(message = paste("fetching sample data of container",
                                     container, "..."))
        on.exit(progress$close())


        message(sampleURL)
        res <- as.data.frame(fromJSON(sampleURL))
        message(paste0('got ', nrow(res), ' samples of container ', container, "."))
        rownames(res) <- res$samples._id
        df <- res[,c('samples._id', 'samples.name', 'samples.condition')]
        df$containerid = container
        return(df)
      },
      error = function(cond) {
        message(paste("Container", container, "does not seem to have samples:"))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      },
      warning=function(cond) {
        message(paste("REST request caused a warning for container:", container))
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
      },
      finally={
        message(paste("Processed sample query for container:", container))
      }
      )
      return(out)
    }
    NULL
  }

  getSample <- reactive({
    if (input$containerType == 'project') {

      if (is.null(input$project)) {
        return(NULL)
      } else {
        res <- .restSample(input$project)
        return(res)
      }
    } else if (input$containerType == 'order') {
      if (is.null(input$project)) {
        return(NULL)
      } else {

        containerIDs <- as.numeric(strsplit(input$project,
                                            c("[[\ ]{0,1}[,;:]{1}[\ ]{0,1}"),
                                            perl = FALSE)[[1]])
        res <- do.call('rbind', lapply(unique(containerIDs), .restSample))
        return(res)
      }
    }else{
      NULL
    }
  })


  output$sample <- renderUI({
    res <- getSample()

    if (is.null(res)){
      selectInput('sample', 'Sample:', NULL)
    }else{
      idx <- rev(order(res$samples._id))
      res <- res[idx, ]
      selectInput('sample', 'Sample:',
                  paste0("C",res$container, "_S", res$samples._id, "-", res$samples.name),
                  size = 40, multiple = TRUE, selectize = FALSE)
    }
  })

  output$login <- renderUI({
    if (input$containerType == 'project'){
      res <- getLogin()
      if (!is.null(res)){
        selectInput('login', 'Login:', as.character(res), multiple = FALSE)
      }else{
        selectInput('login', 'Login:', NULL)
      }}
    else{
      selectInput('login', 'Login:', "analytics", multiple = FALSE)
    }
  })


  getResourcename <- reactive({
    paste("fgcz-queue-generator_p", input$project, "_", input$instrument, "_", format(Sys.time(), "%Y%m%d"), ".csv", sep='')
  })


  #------------------------ getBfabricContent ----
  getBfabricContent <- reactive({

    if (is.null(input$sample)) {
      return(NULL)
    }

    if (length(input$sample) == 1 && input$sample == "") {
      return (NULL)
    }

    values$wuid <- NULL

    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "counting  lambdas 11 ...")
    on.exit(progress$close())

    res <- getSample()

    res[, "instrument"] <- input$instrument

    #idx.filter <- (paste(res$samples._id, res$samples.name, sep="-") %in% input$sample)
    idx.filter <- (paste0("C", res$container, "_S", res$samples._id, "-", res$samples.name) %in% input$sample)

    print(names(res))
    res <- res[idx.filter, c("samples.name", "samples._id", "samples.condition", "containerid")]

    # TODO(cp): replace extract by sample
    names(res) <- c("extract.name", "extract.id", "extract.Condition", "containerid")

    if(any(is.na(res$extract.Condition))){

      res$extract.Condition[is.na(res$extract.Condition)] <- "A"

    } else{

    }

    # generate the actual queue data.frame ----

    containerid <- input$project
    if(input$containerType == 'order'){
      containerid <- NULL
    }

    rv <- bfabricShiny:::generate_queue_order(x = res,
                         foldername = input$folder,
                         projectid = containerid,
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
    
    containerids <- strsplit(as.character(input$project), ",")[[1]]
    rv <- lapply(containerids, function(containerid){
      POST("http://localhost:5000/add_resource",
           body = toJSON(list(base64=file_content,
                              name='MS configuration',
                              containerid=containerid,
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
           ))})


    wuid <-  (content(rv[[1]])$workunit_id)
    values$wuid <- wuid
    ########################## WRITE CSV TO BFABRIC
  }
  )

})


