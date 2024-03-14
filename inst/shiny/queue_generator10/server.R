# This is the server logic for a Shiny MS configuration web application.
#
# maintainer: Christian Panse <cp@fgcz.ethz.ch>

stopifnot(
  require(bfabricShiny),
  require(base64enc),
  require(jsonlite),
  require(httr),
  require(DT),
  require(dplyr),
  require(XML))


stopifnot(packageVersion('bfabricShiny') >= "0.12.18")

# TODO(cp): replace with CRAN protViz functions
source("queuetools.R", local = FALSE)

shinyServer(function(input, output, session) {
  autoInvalidate <- reactiveTimer(5000)
  values <- reactiveValues(wuid = NULL)

  Rprofile <- reactive({ 
    f <- file.path(Sys.getenv("HOME"), ".Rprofile") 
    if (file.exists(f)){ return (f) }
    
    })

  login <- reactive({
    bfabricShiny:::.login()
  })
  
  posturl <- reactive({
    bfabricShiny:::.posturl()
  })

  webservicepassword <- reactive({
    bfabricShiny:::.webservicepassword()
  })

  # ---- getInstruments ----
  
  .getMetabolomicsInstrument <- function(){
    list(
      ASTRAL_1 = 'Xcalibur',
      EXPLORIS_3 = 'Xcalibur',
      QEXACTIVEHF_2 = 'Xcalibur',
      QUANTIVA_1 = 'Xcalibur',
      QEXACTIVE_2 = 'Xcalibur',
      QEXACTIVE_3 = 'Xcalibur'
    )}

    .getProteomicsInstrument <- function(){
  list(
    ASTRAL_1 = 'Xcalibur',
    QEXACTIVE_2 = 'Xcalibur',
    QEXACTIVEHF_2 = 'Xcalibur',
    QEXACTIVEHF_4 = 'Xcalibur',
    FUSION_2 =  'Xcalibur',
    EXPLORIS_1 = 'Xcalibur',
    EXPLORIS_2 = 'Xcalibur',
    LUMOS_1 = 'Xcalibur',
    LUMOS_2 = 'Xcalibur'
  )}

  
  getInstrument <- reactive({
    if (input$area == "Metabolomics") {
      .getMetabolomicsInstrument()
    }else{
      .getProteomicsInstrument()
    }
  })

  getInstrumentSuffix <- reactive({
    bfabricShiny:::.getInstrumentSuffix()
  })

  #output list ----

  output$instrumentControlSoftware <- renderUI(({
    instrumentControlSoftware <- c("XCalibur", "HyStar")
    selectInput('instrumentControlSoftware', 'instrument control software:',
                instrumentControlSoftware,
                multiple = FALSE,
                selected = instrumentControlSoftware[1])
  }))

  output$lcSystem <- renderUI(({
    lcSystem <- c("nanoElute54_54", "M-CLASS48_48",  "EVOSEP1x12x8")
    if (!is.null(input$instrumentControlSoftware)){
      if (input$instrumentControlSoftware == "XCalibur"){
        lcSystem <- c("M-CLASS48_48")
      }
      else if (input$instrumentControlSoftware == "HyStar"){
        lcSystem <- c("nanoElute54_54", "EVOSEP1x12x8")
      }
      selectInput('lcSystem', 'LC-system:',
                  lcSystem ,
                  multiple = FALSE,
                  selected = lcSystem[1])
    }
  }))

  output$acquisitionType <- renderUI({
    acquisitionType <- c("dda", "dia")
    
    selectInput('acquisitionType', 'acquisition type:',
                acquisitionType ,
                multiple = FALSE,
                selected = acquisitionType[1])
  }
  )
  
  
  ## ========== output$area 
  output$area <- renderUI(({
    res.area <- c("Proteomics", "Metabolomics")
    selectInput('area', 'Area:', res.area, multiple = FALSE, selected = res.area[1])
  }))

  output$folder <- renderUI(({
    textInput('folder', 'Data Folder Name:', "", width = NULL, placeholder = "enter your folder name here")
  }))

  output$testmethods <- renderUI(({
    res.testmethods <- 1:5
    selectInput('testmethods', 'Number of methods to test:', res.testmethods, multiple = FALSE, selected = res.testmethods[1])
  }))

  output$replicates <- renderUI(({
    res.replicates <- 1:9
    selectInput('replicates', 'Number of injections for each method:', res.replicates, multiple = FALSE, selected = res.replicates[1])
  }))

  output$container <- renderUI({
    if (input$containerType == 'project'){
      numericInput('container', 'Container (project or order):', value = 3530,  min = 1000, max = NA, width = 400)
    } else{
      textInput('container', 'Containers (orders):', value = "29941,30021,30041,30057", width = 1000, placeholder = 'comma separated list of order IDs')
    }
  })

  output$instrument <- renderUI({
    if(!is.null(input$containerType) && !is.null(input$instrumentControlSoftware)){
      
      if (input$containerType == 'project') {
        res.instrument <- names(getInstrument())
      }else{
        res.instrument <- names(list(
          QEXACTIVE_1 = 'Xcalibur',
          QEXACTIVE_2 = 'Xcalibur',
          LUMOS_2 = "Xcalibur",
          EXPLORIS_2 = "Xcalibur"
        ))
      }

      if (input$instrumentControlSoftware == "HyStar"){
        res.instrument <- names(list(
          TIMSTOF_1 = 'HyStar',
          TIMSTOFLEX_1 = 'HyStar'
        ))
      }

      selectInput('instrument', 'Instrument:',
                  res.instrument,
                  multiple = FALSE, selected = res.instrument[1])
    }
  })

  output$method <- renderUI(({
    if (is.null(input$instrumentControlSoftware)){return(NULL)}
    if (input$instrumentControlSoftware == "XCalibur"){
      selectInput('method', 'Queue Method:', c('default', 'random', 'blockrandom', 'PRM', 'testing'),
                  multiple = FALSE, selected = 'default')
    }else{
      selectInput('method', 'Queue Method:', c('default', 'blockrandom'), multiple = FALSE, selected = 'blockrandom')
    }
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
    if (is.null(input$container) || grepl(",", input$container)) {
      return(NULL)
    }else{
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = paste("querying user of container",
                                   input$container, "..."))
      on.exit(progress$close())
      
      # define a callback function to be passed to the 'computational' method
      updateProgress <- function(value = NULL, detail = NULL, n = NULL) {
        progress$set(detail = detail)
      }
      
      rv <- bfabricShiny::readPages(login = login(),
                                    webservicepassword = webservicepassword(),
                                    posturl = posturl(),
                                    endpoint = 'user',
                                    query = list(containerid = input$container),
                                    updateProgress = updateProgress) |> 
        lapply(FUN = function(x){x$login}) |>
        unlist()

      return(rv)
    }
  })
  
  getSample <- reactive({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = paste("querying samples of container",
                                   input$container, "..."))
    on.exit(progress$close())
    
    if (input$containerType == 'project') {
      
      if (is.null(input$container)) {
        return(NULL)
      } else {
        res <- bfabricShiny::.getSamples(login(),
                                         webservicepassword(),
                                         posturl = posturl(),
                                         containerid = input$container,
                                         updateProgress = function(value = NULL, detail = NULL, n = NULL, ...) {
                                           value <- value * (progress$getMax() / n)
                                           progress$set(
                                             message = "querying ...",
                                             detail = detail,
                                             value = value)
                                         })

        return(res)
      }
    } else if (input$containerType == 'order') {
      if (is.null(input$container) | is.numeric(input$container)) {
        return(NULL)
      } else {
        containerIDs <- as.numeric(strsplit(input$container,
                                            c("[[\ ]{0,1}[,;:]{1}[\ ]{0,1}"),
                                            perl = FALSE)[[1]])
        
        res <- containerIDs |>
          unique() |>
          lapply(FUN = function(x){
            progress$set(message = paste("querying container", x, "..."))
            bfabricShiny:::.getSamples(
              login = login(),
              webservicepassword = webservicepassword(),
              posturl = posturl(),
              containerid = x,
              updateProgress = function(value = NULL, detail = NULL, n = NULL, ...) {
                                           value <- value * (progress$getMax() / n)
                                           progress$set(
                                             message = "querying ...",
                                             detail = detail,
                                             value = value)
                                         })}
          ) |>
          Reduce(f = rbind)
      
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
      #idx <- rev(order(res$samples._id))
      #res <- res[idx, ]
      selectInput('sample', 'Sample:',
                  paste0("C", res$container, "_S", res$samples._id, "-", res$samples.name),
                  size = 40, multiple = TRUE, selectize = FALSE)
    }
  })

  output$login <- renderUI({
    if (input$containerType == 'project'){
      res <- getLogin()
      if (!is.null(res)){
        selectInput('login', 'Login:', as.character(res), multiple = FALSE)
      }else{
        selectInput('login', 'Login:', "analytic")
      }}
    else{
      selectInput('login', 'Login:', "analytic", multiple = FALSE)
    }
  })

  getResourcename <- reactive({
    paste0("fgcz-queue-generator_c", input$container, "_", input$instrument, "_",
          format(Sys.time(), "%Y%m%d"))
  })


  #------------------------getBfabricContent ----
  getBfabricContent <- reactive({

    message(paste("System configuration:", input$area, input$instrumentControlSoftware, input$lcSystem))
    
    if (is.null(input$sample)) {
      return(NULL)
    }

    if (length(input$sample) == 1 && input$sample == "") {
      return(NULL)
    }

    # TODO: howto reset upload button? at the moment only one download is possible.
    # values$wuid <- NULL

    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "composing content ...")
    on.exit(progress$close())

    res <- getSample()
    if(is.null(res)){
      return(NULL)
    }
    
    res[, "instrument"] <- input$instrument

    idx.filter <- (paste0("C", res$container, "_S", res$samples._id, "-", res$samples.name) %in% input$sample)
    res <- res[idx.filter, c("samples.name", "samples._id", "samples.condition", "containerid")]

    # TODO(cp): replace extract by sample
    names(res) <- c("extract.name", "extract.id", "extract.Condition", "containerid")

    # TODO check if needed
    if (any(is.na(res$extract.Condition))) {
      res$extract.Condition[is.na(res$extract.Condition)] <- "A"
    }
    # generate the actual queue data.frame ----

    containerid <- input$container
    if (input$containerType == 'order') {
      containerid <- NULL
    }
    
    if (input$instrumentControlSoftware == "XCalibur" && input$area == "Proteomics"){
      print(res)
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
                                                startposition = input$startposition,
                                                acquisitionType = input$acquisitionType)
      
      # TODO(cp): add an addidtional parameter
      idx <- rv['Sample Name'] == "autoQC4L" & grepl("EXPLORIS_", rv['Path'])
      rv[idx, 'Inj Vol'] <- 1
      # save(res,rv, file = '/tmp/queue.RData')
      print(res)
      # base::save(res,rv, file="/tmp/ddd.RData")
      print(rv)
      return(rv)
    }else if (input$instrumentControlSoftware == "XCalibur" &&  input$area == "Metabolomics"){
      
      note <- gsub('([[:punct:]])|\\s+', '_', input$folder)
      inputSampleTable <- data.frame(containerid = res$containerid,
                                     id = res$extract.id, 
                                     name = res$extract.name,
                                     condition = res$extract.Condition)
      # protViz::blockRandom("condition") |>
      print(inputSampleTable)
      
      Xpath <- paste0("D:\\Data2San\\p", input$container, "\\",
             input$area, "\\",
             input$instrument, "\\",
             input$login,"_",format(Sys.Date(), format = "%Y%m%d"),  "\\")
      
      rv <- inputSampleTable |>
        protViz::assignPlatePosition() |>
        protViz::insertSamples(howoften = 0, begin = TRUE, end = FALSE,
                               stdPosX = '6', stdPosY = 'F', plate = 1,
                               stdName = "clean", volume = 2) |>
        protViz:::formatXCalibur(Xpath)
      
      
      print(rv)
      
      return(rv)
      
    }else if (input$instrumentControlSoftware == "HyStar"){
      ##==========HyStar========== 
      message("DEBUG")
      message(paste(names(input), collapse = ", "))
      #print(as_tibble(as.data.frame(input)))
      #save(input, file='/tmp/input.RData')
     # list_of_inputs <- reactiveValuesToList(input)
     #  print(list_of_inputs)
      
      note <- gsub('([[:punct:]])|\\s+', '_', input$folder)
      inputSampleTable <- data.frame(container_id = res$containerid,
                       sample_id = res$extract.id, 
                       sample_name = res$extract.name,
                       sample_condition = res$extract.Condition)
      
      if (input$method == 'blockrandom'){
        set.seed(1)
        inputSampleTable <- inputSampleTable %>% .blockRandom(x = "sample_condition", check = FALSE)
      }
      print("DEBUG")
      print(input$autoQC4L)
      ## TODO method files only for clean|autoQC03|autoQC01
      if (input$lcSystem == "EVOSEP1x12x8"){
         rv <- inputSampleTable |>
          .insertStandardsEVOSEP(stdName = "clean", 
                           between=input$clean,
                           howoften = input$cleano,
                           howmany = input$cleanm,
                           volume = 4,
                           begin = "4" %in% c(input$start1, input$start2, input$start3),
                           end = "4" %in% c(input$end1,input$end2, input$end3)) |>
          .insertStandardsEVOSEP(stdName = "autoQC01",
                           between=input$autoQC01,
                           howoften = input$QC01o,
                           howmany = input$QC01m,
                           begin = "1" %in% c(input$start1, input$start2, input$start3),
                           end = "1" %in% c(input$end1, input$end2, input$end3)) |>
           .insertStandardsEVOSEP(stdName = "autoQC02",
                                  between=input$autoQC02,
                                  howoften = input$QC02o,
                                  howmany = input$QC02m,
                                  begin = "2" %in% c(input$start1,input$start2, input$start3),
                                  end = "2" %in% c(input$end1,input$end2, input$end3)) |> 
          .insertStandardsEVOSEP(stdName = paste0("autoQC03", input$acquisitionType),
                           between=input$autoQC4L,
                           howoften = input$QC4Lo,
                           howmany = input$QC4Lm,
                           begin = "3" %in% c(input$start1,input$start2, input$start3),
                           end = "3" %in% c(input$end1,input$end2, input$end3), volume = 2) |>
          .mapPlatePositionEVOSEP(volume = 1) |>
          .formatHyStar(dataPath = paste0("D:\\Data2San\\p", input$container, "\\",
                                                input$area, "\\",
                                                input$instrument, "\\",
                                                input$login,"_",format(Sys.Date(), format = "%Y%m%d"), "_", note, "\\"),
                        Method_Set = paste0("D:\\Methods\\autoQC\\evosepOne\\autoQC03", input$acquisitionType, ".m"),
                        FUN = function(x, y, plate){paste0("S", plate, "-", y, x)}) 
          return(rv)
        
      }else{
        ## nanoElute
        rv <- inputSampleTable %>%
          .mapPlatePositionNanoElute %>%  
          .insertStandardsNanoElute(stdName = "clean", stdPosX='52', stdPosY='1', plate = 2,
                            between=input$clean,
                            howoften = input$cleano,
                            howmany = input$cleanm,
                            volume = 4,
                            begin = "4" %in% c(input$start1,input$start2, input$start3),
                            end = "4" %in% c(input$end1,input$end2, input$end3)) %>% 
          .insertStandardsNanoElute(stdName = "autoQC01", stdPosX='53', stdPosY='1', plate = 2,
                                    between=input$autoQC01,
                                    howoften = input$QC01o,
                                    howmany = input$QC01m,
                                    begin = "1" %in% c(input$start1,input$start2, input$start3),
                                    end = "1" %in% c(input$end1,input$end2, input$end3)) %>% 
          .insertStandardsNanoElute(stdName = "autoQC02", stdPosX='53', stdPosY='1', plate = 2,
                                    between=input$autoQC02,
                                    howoften = input$QC02o,
                                    howmany = input$QC02m,
                                    begin = "2" %in% c(input$start1,input$start2, input$start3),
                                    end = "2" %in% c(input$end1,input$end2, input$end3)) %>% 
          .insertStandardsNanoElute(stdName = "autoQC4L", stdPosX='54', stdPosY='1', plate = 2, 
                                    between=input$autoQC4L,
                                    howoften = input$QC4Lo,
                                    howmany = input$QC4Lm,
                                    begin = "3" %in% c(input$start1,input$start2, input$start3),
                                    end = "3" %in% c(input$end1,input$end2, input$end3), volume = 2) %>% 
          .formatHyStar(dataPath = paste0("D:\\Data2San\\p", input$container, "\\",
                                          input$area, "\\",
                                          input$instrument, "\\",
                                          input$login,"_", format(Sys.Date(), format = "%Y%m%d"), "_", note, "\\"),
                        Method_Set=paste0("D:\\Methods\\autoQC\\nanoElute\\autoQC03", input$acquisitionType,".m"),
                        FUN=function(x, y, plate){paste0( "Slot", plate,":", x)})
        
        #rv <- .blockRandom(rv, x = "sample_condition")
        
        return(rv)
      }}
    NULL
})


  #----- DT::renderDataTable ----
  output$table <- DT::renderDataTable(DT::datatable({
    
    if ((length(input$sample) >= 1)){
      
      getBfabricContent()
      
    }else{
      
      as.data.frame(list(output="no data - select sample first."))
    }
    
  }, options = list(paging = FALSE)
  ))
  
  #=======output$download======
  output$download <- renderUI({
    res <- getBfabricContent()
    message(paste0("debug output$download values$wuid=", values$wuid))
    if (is.null(res)){
      msg <- "The download is not possible yet. Please, select some samples first."
      HTML(msg)
    }else {
      message(paste0("debug output$download values$wuid=", values$wuid))
      if (isFALSE(is.null(values$wuid))){
        # https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=154014
        
        wuUrl <- paste0("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=",
                        values$wuid, "', '_blank')")
        
        actionButton("download",
                     paste("go to B-Fabric workunit", values$wuid),
                     onclick = wuUrl)
      }else{
        actionButton('generate', 'Upload MS configuration\nto B-Fabric')
      }
    }
  })

  #=-----------systemInformation-----------------
  output$systemInformation <- renderUI({
    info <- paste0("system information<br>",
                   "R.version.string: ", R.version.string, "<br>",
                   "Rprofile: ", Rprofile(), "<br>",
                   "posturl: ", posturl(), "<br>",
                   "login: ", login(), "<br>",
                   "workunitID: ", values$wuid)
    HTML(info)
  })
  
  #------------------- bfabricUpload --------
  bfabricUpload <- observeEvent(input$generate, {
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "uploading MS configuration to bfabric")
    on.exit(progress$close())

    res <- getBfabricContent()
    
    if (input$instrumentControlSoftware == "XCalibur"){
      #=====write XCalibur MS configuration to bfabric=========
      fn <- tempfile(tmpdir = tempdir(), fileext = ".csv") 
      message(paste0("Composing XCalibur MS configuration file ", fn, " ..."))
      
      cat("Bracket Type=4\r\n", file = fn, append = FALSE)
      write.table(res, file = fn,
                  sep = ',', row.names = FALSE,
                  append = TRUE, quote = FALSE, eol = '\r\n')
      
      containerids <- strsplit(as.character(input$container), ",")[[1]]
      
      workunitDescription <- paste0(
        "The spreadsheet contains a ", input$instrument,
        " queue configuration having ", nrow(res), " rows.\n",
        "The parameters are:\n",
        "\thow.often: ", as.integer(input$howoften), "\n",
        "\thow.many:  ", as.integer(input$howmany), "\n",
        "\tnr.methods:",  as.integer(input$testmethods), "\n",
        "\tnr.replicates:", as.integer(input$replicates), "\n",
        "\tshowcondition: ",  input$showcondition, "\n",
        "\tqc.type: ", as.integer(input$qctype), "\n",
        "\tmethod: ",  as.character(input$method),
        "\nThe resource was generated by using the R package https://CRAN.R-project.org/package=protViz version ",
        packageVersion('protViz'), ".\n")
      
      rv <- lapply(containerids, function(containerid){
        message(paste("containerid = ", containerid))
        bfabricShiny::uploadResource(login(), webservicepassword(),
                                     posturl = posturl(),
                                     containerid = containerid,
                                     applicationid = 212,
                                     status = 'AVAILABLE',
                                     description = workunitDescription,
                                     workunitname = 'XCalibur MS configuration',
                                     resourcename = paste0(getResourcename(), '.csv'),
                                     file = fn)
        
      })
      values$wuid <- rv[[1]]$workunit[[1]]$`_id`
    } else if (input$instrumentControlSoftware == "HyStar"){
      #=====write HyStar MS configuration to bfabric=========
      fn <- tempfile(tmpdir = tempdir(), fileext = ".xml") 
      message(paste0("Composing HyStar XML MS configuration file ", fn, " ..."))
      
      queue <- dplyr::as_tibble(res) %>%
        dplyr::rename(SampleID = 'Sample ID') %>%
        dplyr::rename(SampleComment = 'Sample Comment') %>%
        dplyr::rename(ACQEND_EXECUTE = 'ACQEnd Execute') %>%
        dplyr::rename(SuperMethod = 'Method Set') %>%
        dplyr::rename(Position = 'Vial') %>%
        dplyr::rename(DataPath = 'Data Path')
      
      
      xml <- xmlTree()
      xml$addTag("SampleTable")
      dump <- lapply(1:nrow(queue), FUN = function(i) xml$addTag("Sample", close=TRUE, attrs=queue[i, ]))
      
      message(paste0("Saving XML ..."))
      rvSave <- saveXML(xml$value(), file = fn, encoding = "utf-8")
      
      workunitDescription <- paste0("\nThe resource was generated by using the R package https://CRAN.R-project.org/package=protViz version ",
                                    packageVersion('protViz'), ".\n")
   
      
      containerids <- strsplit(as.character(input$container), ",")[[1]]
      
      rv <- lapply(containerids, function(containerid){
        message(paste("containerid = ", containerid))
        bfabricShiny::uploadResource(login = login(),
                                               webservicepassword = webservicepassword(),
                                               posturl = posturl(),
                                               containerid = containerid,
                                               applicationid = 289,
                                               status = 'AVAILABLE',
                                               description = workunitDescription,
                                               workunitname = 'HyStar MS configuration',
                                               resourcename = paste0(getResourcename(), '.xml'),
                                               file = fn)
      })
      
      
      message(paste0("DEBUG: wuid=", values$wuid))
      
      values$wuid <- rv[[1]]$workunit[[1]]$`_id`
      message(paste0("DEBUG: rv wuid=", rv[[1]]$workunit[[1]]$`_id`))
      message(paste0("DEBUG: wuid=", values$wuid))
    }else{
      warning("Hit else block of bfabricUpload!")
    }
    TRUE
  })
  
  #------------------- sessionInfo --------
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })

})


