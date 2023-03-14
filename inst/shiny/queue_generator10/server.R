# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# https://github.com/fgcz/bfabricShiny

stopifnot(
  require(bfabricShiny),
  require(base64enc),
  require(jsonlite),
  require(httr),
  require(DT),
  require(dplyr),
  require(XML))

# TODO(cp): replace with CRAN protViz functions
source("queuetools.R", local = FALSE)

shinyServer(function(input, output, session) {

  values <- reactiveValues(wuid = NULL)

  Rprofile <- reactive({ 
    f <- file.path(Sys.getenv("HOME"), ".Rprofile") 
    if (file.exists(f)){ return (f) }
    
    })

  login <- reactive({
    source(Rprofile(), local=TRUE)
    message(paste0("read login ", login, "."))
    return (login)
  })
  
  posturl <- reactive({
    source(Rprofile(), local=TRUE)
    message(paste0("read bfabricposturl ", bfabricposturl, "."))
    return (bfabricposturl)
  })

  webservicepassword <- reactive({
    source(Rprofile(), local=TRUE)
    message(paste0("read webservicepassword for login ", login, "."))
    return(webservicepassword)
  })

  # ---- getInstruments ----
  
  .getMetabolomicsInstrument <- function(){
    list(
      QUANTIVA_1 = 'Xcalibur',
      QEXACTIVE_2 = 'Xcalibur',
      QEXACTIVE_3 = 'Xcalibur'
    )}
  
  getInstrument <- reactive({
    if (input$area == "Metabolomics") {
      .getMetabolomicsInstrument()
    }else{
      bfabricShiny:::.getInstrument()
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
        res.instrument <- names(list(TIMSTOF_1 = input$instrumentControlSoftware))
      }

      selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
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
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching user data ...")
    on.exit(progress$close())

    if (is.null(input$container) || grepl(",", input$container)) {
      return(NULL)
    }else{
      rv <- bfabricShiny::readPages(login(), webservicepassword(),
                                    posturl = posturl(),
                                    endpoint = 'user',
                               query = list(containerid = input$container)) |>
        lapply(FUN=function(x){x$login}) |>
        unlist()
      return(rv)
    }
  })
  
  getSample <- reactive({
    if (input$containerType == 'project') {
      
      if (is.null(input$container)) {
        return(NULL)
      } else {
        progress <- shiny::Progress$new(session = session, min = 0, max = 1)
        progress$set(message = paste("fetching samples of container",
                                     input$container, "..."))
        on.exit(progress$close())
             
        res <- bfabricShiny:::.getSamples(login(), webservicepassword(),
                                   posturl = posturl(),
                                   containerid = input$container)
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
            progress <- shiny::Progress$new(session = session, min = 0, max = 1)
            progress$set(message = paste("fetching samples of container",
                                         x, "..."))
            on.exit(progress$close())
            
            bfabricShiny:::.getSamples(
              login = login(),
              webservicepassword = webservicepassword(),
              posturl = posturl(), containerid=x)}
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
        selectInput('login', 'Login:', "analytic")
      }}
    else{
      selectInput('login', 'Login:', "analytic", multiple = FALSE)
    }
  })

  getResourcename <- reactive({
    paste("fgcz-queue-generator_p", input$container, "_", input$instrument, "_",
          format(Sys.time(), "%Y%m%d"), ".csv", sep='')
  })


  #------------------------ getBfabricContent ----
  getBfabricContent <- reactive({

    message(paste("DEBUG", input$instrumentControlSoftware, input$lcSystem))
    
    if (is.null(input$sample)) {
      return(NULL)
    }

    if (length(input$sample) == 1 && input$sample == "") {
      return(NULL)
    }

    values$wuid <- NULL

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
    
    if (input$instrumentControlSoftware == "XCalibur"){
      
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
      
      # TODO(cp): add an addidtional parameter
      idx <- rv['Sample Name'] == "autoQC4L" & grepl("EXPLORIS_", rv['Path'])
      rv[idx, 'Inj Vol'] <- 1
      # save(res,rv, file = '/tmp/queue.RData')
      print(res)
      # base::save(res,rv, file="/tmp/ddd.RData")
      print(rv)
      return(rv)
    }else if (input$instrumentControlSoftware == "HyStar"){
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
        inputSampleTable <- inputSampleTable %>% .blockRandom(x = "sample_condition", check=FALSE)
      }
      print("DEBUG")
      print(input$autoQC4L)
      ## TODO method files only for clean|autoQC4L|autoQC01
      if (input$lcSystem == "EVOSEP1x12x8"){
         rv <- inputSampleTable %>%
          .insertStandardsEVOSEP(stdName = "washing", 
                           between=input$clean,
                           howoften = input$cleano,
                           howmany = input$cleanm,
                           volume = 4,
                           begin = "4" %in% c(input$start1,input$start2, input$start3),
                           end = "4" %in% c(input$end1,input$end2, input$end3)) %>% 
          .insertStandardsEVOSEP(stdName = "autoQC01",
                           between=input$autoQC01,
                           howoften = input$QC01o,
                           howmany = input$QC01m,
                           begin = "1" %in% c(input$start1,input$start2, input$start3),
                           end = "1" %in% c(input$end1,input$end2, input$end3)) %>% 
           .insertStandardsEVOSEP(stdName = "autoQC02",
                                  between=input$autoQC02,
                                  howoften = input$QC02o,
                                  howmany = input$QC02m,
                                  begin = "3" %in% c(input$start1,input$start2, input$start3),
                                  end = "3" %in% c(input$end1,input$end2, input$end3)) %>% 
          .insertStandardsEVOSEP(stdName = "autoQC4L",
                           between=input$autoQC4L,
                           howoften = input$QC4Lo,
                           howmany = input$QC4Lm,
                           begin = "3" %in% c(input$start1,input$start2, input$start3),
                           end = "3" %in% c(input$end1,input$end2, input$end3), volume = 2) %>% 
          .mapPlatePositionEVOSEP(volume = 1) %>%
          .formatHyStar(dataPath = paste0("D:\\Data2San\\p", input$container, "\\",
                                                input$area, "\\",
                                                input$instrument, "\\",
                                                input$login,"_",format(Sys.Date(), format = "%Y%m%d"), "_", note, "\\"),
                        Method_Set="D:\\Methods\\autoQC\\evosepOne\\autoQC4L.m",
                        FUN=function(x,y,plate){paste0("S",plate,"-", y, x)}) 
          return(rv)
        
      }else{
        ## nanoElute
        rv <- inputSampleTable %>%
          .mapPlatePositionNanoElute %>%  
          .insertStandardsNanoElute(stdName = "washing", stdPosX='52', stdPosY='1', plate = 2,
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
                        Method_Set="D:\\Methods\\autoQC\\nanoElute\\autoQC4L.m",
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

  output$download <- renderUI({
    res <- getBfabricContent()
    
    if (is.null(res)){
      msg <- "Download not possible yet."
      HTML(msg)
      
    }else {
      if (input$instrumentControlSoftware == "XCalibur"){
        if (!is.null(values$wuid)){
          # https://fgcz-bfabric-test.uzh.ch/bfabric/userlab/show-workunit.html?id=154014
          actionButton("download",
                       paste("go to B-Fabric workunit", values$wuid),
                       onclick = paste("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=",
                                       values$wuid, "', '_blank')", sep=''))
        }else{
          actionButton('generate', 'Upload configuration\nto B-Fabric')
        }
        
      }else{
        downloadButton('downloadHyStarXML', 'Download HyStar configuration xml file (experimental)')
      }
    }
    
  })

  output$systemInformation <- renderUI({
    info <- paste0("system information<br>",
                   "R.version.string: ", R.version.string, "<br>",
                   "Rprofile: ", Rprofile(), "<br>",
                   "posturl: ", posturl(), "<br>",
                   "login: ", login())
    HTML(info)
  })
  
  output$downloadHyStarXML <- downloadHandler(
   
    filename = paste("C", input$container, "-", format(Sys.time(), format = "%Y%m%d-%H%M%S"), "_HyStar.xml", sep = ""),
    content = function(file) {
      rv <- getBfabricContent()
      #message("saving bfabric content ...")
      #save(rv, file="/tmp/bfabricContent.RData")
      #message("[DONE]")
      queue <- dplyr::as_tibble(rv)
      rv <- queue %>%
        dplyr::rename(SampleID = 'Sample ID') %>%
        dplyr::rename(SampleComment = 'Sample Comment') %>%
        dplyr::rename(ACQEND_EXECUTE = 'ACQEnd Execute') %>%
        dplyr::rename(SuperMethod = 'Method Set') %>%
        dplyr::rename(Position = 'Vial') %>%
        dplyr::rename(DataPath = 'Data Path')
      
      xml <- xmlTree()
      xml$addTag("SampleTable")
      dump <- lapply(1:nrow(rv), FUN=function(i) xml$addTag("Sample", close=TRUE, attrs=rv[i,]))
      saveXML(xml$value(), file=file, encoding="utf-8")
    }
  )
  
  #------------------- bfabricUpload --------
  bfabricUpload <- observeEvent(input$generate, {
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "upload configuration to bfabric")
    on.exit(progress$close())

    res <- getBfabricContent()
    
    if (input$instrumentControlSoftware == "XCalibur"){
      ########################## WRITE CSV TO BFABRIC
      fn <- tempfile()#pattern = "file", tmpdir = tempdir(), fileext = ".csv")[1]
      message(fn)
      cat("Bracket Type=4\r\n", file = fn, append = FALSE)
      write.table(res, file = fn,
                  sep = ',', row.names = FALSE,
                  append = TRUE, quote = FALSE, eol = '\r\n')
      
      file_content <- base64encode(readBin(fn, "raw", file.info(fn)[1, "size"]), 'csv')
      
      containerids <- strsplit(as.character(input$container), ",")[[1]]
      rv <- lapply(containerids, function(containerid){
        message(paste("containerid = ", containerid))
        
        POST(paste0(posturl(), "add_resource"),
             body = toJSON(list(base64 = file_content,
                                name = 'MS configuration',
                                containerid = containerid,
                                applicationid = 212,
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
                                                            packageVersion('bfabricShiny'), ".\n",
                                                            "\n\nTo help us funding further development, please cite:\nPMID: 36073980 DOI: 10.1515/jib-2022-0031",
                                                            sep=''),
                                resourcename = getResourcename())
             ))})
      
      
      wuid <-  (content(rv[[1]])$workunit_id)
      values$wuid <- wuid
      ########################## WRITE CSV TO BFABRIC
    } else{
    #message("writexl to /tmp/gueue_generator.xls")
      #res <- getBfabricContent()
      #tmp <- write_xlsx(list(HyStar = res), path = "/tmp/gueue_generator.xls")
    }
    
  }
  )
  
  #------------------- sessionInfo --------
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })

})


