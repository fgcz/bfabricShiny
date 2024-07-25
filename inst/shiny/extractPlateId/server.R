#R 
# 2023 Maria dErrico
# 2024-07 Christian Panse <cp@fgcz.ethz.ch>

stopifnot(require(shiny),
          require(bfabricShiny),
          require(stringr))

if (file.exists("configs.R")){ source("configs.R") }else{stop("can not load queue configs.")}
#if (file.exists("configs.R")){ source("helper.R") }else{stop("can not load queue helper.")}

# Define server logic required
shinyServer(function(input, output) {
 
  debugmode <- FALSE
  TIMEdebugmode <- FALSE
  
  
  instruments <- list(
    Metabolomics = c("QEXACTIVEHF_3", "QUANTIVA_1", "QEXACTIVE_2",
                     "QEXACTIVE_3", "ASTRAL_1", "EXPLORIS_3"),
    Proteomics = c("QEXACTIVEHF_2", "QEXACTIVEHF_4", "QEXACTIVE_2", "FUSION_2",
                   "EXPLORIS_1", "EXPLORIS_2", "LUMOS_1", "LUMOS_2",
                   "TIMSTOFFLEX_1"))
  
  
  columnOrder <<- c("File Name",
                   "Path",
                   "Position",
                   "Inj Vol",
                   "L3 Laboratory",
                   "Sample ID",
                   "Sample Name",
                   "Instrument Method") 
  
  plate_idx <- c("Y", "G", "R", "B")
  currentdate <- format(Sys.time(), "%Y%m%d")

  bf <- callModule(bfabricShiny::bfabricLogin,
                   "bfabric8")
  rv <- reactiveValues(download_flag = 0, wuid = NULL)

  output$bfabricUser <- renderUI({
    if (require("bfabricShiny")){
      bfabricInput("bfabric8")
    }
  })
  
  posturl <- reactive({
    bfabricShiny:::.posturl()
  })
  
  user <- reactive({
    shiny::req(bf$login())
    shiny::req(bf$webservicepassword())
 
    try(
      u <- bfabricShiny::read(login = bf$login(),
                              webservicepassword = bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = 'user',
                              query = list(login = bf$login()))$res[[1]])
    validate(shiny::need(try(u$login == bf$login()),
                         "Please provide valid bfabric login and webservicepassword."))
    message(paste("Request started from user", u$login))
    
    return(u)
  })

 # UI ==========
  output$orderID <- renderUI({
    shiny::req(user())
  
    if (user()$login == 'cpanse'){
     return( selectInput(
        "orderID",
        "Order ID:",
        c("31741", "35116", "35464", "34843", "34777", "34778"),
        selected = "31741",
        multiple = FALSE,
        selectize = FALSE
      ))
      
    }
    
    numericInput(
      "orderID",
      "order ID:",
      "",
      min = NA,
      max = NA,
      step = NA,
      width = NULL
    )
  })

  output$plateID <- renderUI({
      shiny::req(input$orderID)
      shiny::req(read_plateid())
      shiny::req(user())
      
      
      selectInput(
          "plateID",
          "List of available plate IDs:",
          read_plateid(),
          selected = "",
          multiple = TRUE,
          selectize = TRUE,
          size = NULL,
          width = NULL
    )
      
  
  })

  output$injvol <- renderUI({
    shiny::req(input$orderID)
    #shiny::req(read_plateid())
    numericInput(
      "injvol",
      "Inj Vol",
      "",
      min = NA,
      max = NA,
      step = NA,
      width = NULL
    )
  })

  output$randomization <- renderUI({
    shiny::radioButtons("randomization", "Randomization:",
                 c("no" = "no",
                   "plate" = "plate",
                   "all" = "all"), inline = TRUE)
  })
  
  # selectqFUN ------------
  output$selectqFUN <- renderUI({
    
    qc <- c("qconfigEVOSEP6x12x8Hystar", "qconfigMetabolomics", "qconfigMetabolomicsVial")
    
    shiny::selectInput(inputId = "qFUN", 
                       label = "Queue configuration:",
                       choices = qc,
                       multiple = FALSE,
                       selected = qc[1],
                       selectize = FALSE)
  })
  
  output$checkSampleSelection <- renderUI({
    if (length(input$plateID) == 0){
      shiny::checkboxInput("booleanSampleSelection",
                           "Select samples", value = FALSE)
    }else{NULL}
  })
  
  output$selectSampleSelection <- renderUI({
    shiny::req(sampleOfContainer)
    shiny::req(input$booleanSampleSelection)
    
    if (input$booleanSampleSelection){
      shiny::tagList(
        shiny::tags$h4("Available samples for your queue:"),
        shiny::tags$h5("use \"shift + click\" or \"click + drag\"  for selecting a block of consecutive samples"),
        shiny::tags$h5("use \"control + click\" to select multiple samples"),
        shiny::tags$h5("use \"control + click + drag\" to select multiple blocks of consecutive samples"),
        selectInput('selectedSample', 'Sample:',
                    sampleOfContainer()$"Sample Name",
                    size = 40, multiple = TRUE, selectize = FALSE)
      )
    }else{
      rv$selectedSampleinput <- sampleOfContainer()$"Sample Name"
    }
  })
  
  oeSelectedSample <- observeEvent(input$selectedSample, {
    shiny::req(input$booleanSampleSelection)
    
    if (input$booleanSampleSelection){
      rv$selectedSample <- input$selectedSample
    }
  })
  
  read_plateid <- reactive({
    shiny::req(user())
    shiny::req(input$orderID)
    res <- bfabricShiny::read(bf$login(),
                              bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "plate",
                              query = list('containerid' = input$orderID))$res
    
    
    plate_ids <- sapply(res, function(x) x$id)
    #if (length(plate_ids) == 0) return(NULL)
    
    shiny::validate(
      shiny::need(try(length(plate_ids) > 0), "There are no plate defined for this order")
    )
    sort(plate_ids)
  })
  
  output$area <- renderUI({
    shiny::req(input$orderID)
    selectInput(
      "area",
      "Area:",
      c("Proteomics", "Metabolomics"),
      multiple = FALSE,
      selected = "",
      selectize = TRUE
    )
  })
  
  output$instrumentMode <- renderUI({
    shiny::req(input$instrument)
    
    if (input$area == "Metabolomics"){
      shiny::radioButtons("mode", "Mode:",
                          c("neg" = "_neg",
                            "pos" = "_pos"), 
                          inline = TRUE)
    }else{NULL}
  })

  output$instrument <- renderUI({
    shiny::req(input$orderID)
    shiny::req(input$area)
    selectInput(
      "instrument",
      "Instrument:",
      instruments[input$area],
      multiple = FALSE,
      selected = "",
      selectize = TRUE
    )
  })
  
  output$extratext <- renderUI({
    shiny::req(input$orderID)
    shiny::req(read_plateid())
    shiny::req(input$area)
    shiny::req(input$instrument)
    list(textInput(
              "extratext",
              "(Optional) Suffix to the folder name in Data2San:",
              "",
              width = NULL
	      ),
	 helpText(paste0("if empty, the file path will be the following one: D:\\Data2San\\p", input$orderID, "\\", input$area, "\\", input$instrument, "\\", bf$login(), "_", currentdate))
	 )
  })

  output$extrameasurement <- renderUI({
    shiny::req(input$orderID)
    shiny::req(read_plateid())
    shiny::req(input$area)
    shiny::req(input$instrument)
    list(textInput(
      "extrameasurement",
      "(Optional) Suffix to the file name in case of duplicate measurements on the same samples:",
      "",
      width = NULL
    ),
    helpText("Note that the suffix above is applied to all samples for all selected plates"))
  })


  read_sampletype <- function(sampleid){
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "sample",
                              query = list('id' = sampleid))
    if ( debugmode == TRUE ) {message(res)}
    if (is.null(res[[1]]$parent)){
        sampletype <- res[[1]]$type
        if (is.null(sampletype)){
            return("Unknown")
	} else {
	    return(sampletype)
	}
    }
    read_sampletype(res[[1]]$parent[[1]]$id)
  }

  sampleOfContainer <- reactive({
    shiny::req(input$orderID)

    .readSampleOfContainer(input$orderID,
                           login = bf$login(),
                           webservicepassword = bf$webservicepassword(),
                           posturl = posturl())
  })
    
  filteredSampleOfContainer <- reactive({
    shiny::req(sampleOfContainer)
   
    if (length(rv$selectedSample) > 0){
      sampleOfContainer() |>
        subset(sampleOfContainer()$`Sample Name` %in% rv$selectedSample)
    }else{
      sampleOfContainer()
    }
  })
  
  ## ====== compose output table ==========
   composeTable <- reactive({
    shiny::req(input$instrument)
    shiny::req(input$injvol)
    shiny::req(input$area)
    #shiny::req(input$plateID)
    shiny::req(input$qFUN)

    QCrow <- "F"
    instrumentMode <- ""
    if (input$area == "Metabolomics"){
      instrumentMode <- input$mode
    }
    
    if (length(input$plateID) == 0){
     ## --------vial block (no plateid)------------ 
     ## we fetch all samples of a container
      QCrow <- "F"
      randomization <- FALSE
      if (input$randomization == 'plate'){
        randomization <- TRUE
      }
      
      filteredSampleOfContainer() |> 
        .composeSampleTable(orderID = input$orderID,
                            instrument = input$instrument,
                            user = bf$login(),
                            injVol = input$injvol,
                            area = input$area,
                            mode = instrumentMode,
                            randomization = randomization) -> df
    }else{
      ## --------plate block ------------ 
      ## we iterate over the given plates
      QCrow <- "H"
      plateCounter <- 1
      input$plateID |>
        lapply(FUN=function(pid){
          readPlate(pid, login = bf$login(),
                    webservicepassword = bf$webservicepassword(),
                    posturl = posturl()) |>
          .composePlateSampleTable(orderID = input$orderID,
                                   instrument = input$instrument,
                                   injVol = input$injvol,
                                   area = input$area,
                                   mode = instrumentMode,
                                   plateCounter = plateCounter,
                                   randomization = input$randomization) -> p
          # global counter
          plateCounter <<- plateCounter + 1
          p[, columnOrder]
        }) |> Reduce(f = rbind) -> df

      if (input$randomization == "all"){
        set.seed(872436)
        df[sample(nrow(df)), ] -> df
      }
    }
    if (FALSE){
      tempfile(pattern = "fgcz_queue_generator_", fileext = ".RData") -> tf
      base::save(df, file = tf)
      message("Output table saved to ", tf)
    } 
    
    ## ------injectSamples------
    ## here we inject the clean|blank|qc runs and finally replace @@@ with run#
    
    if (input$area == "Metabolomics"){
      do.call(what = input$qFUN, args = list(x = df, QCrow = QCrow, mode = instrumentMode)) |>
        .replaceRunIds()
    }else{
      do.call(what = input$qFUN, args = list(x = df)) |>
        .replaceRunIds()
    }
    
   })

  
  output$outputKable <- DT::renderDataTable({
    shiny::req(composeTable())
    DT::datatable(composeTable(),
                  rownames = FALSE,
                  style = 'auto',
                  editable = FALSE,
                  options = list(paging = FALSE))
  })

   csvFilename <- reactive({
     tempfile(pattern = "fgcz_queue_generator_", fileext = ".csv")
   })


  
  #=======output$download======
  output$download <- renderUI({
    #shiny::req(file.exists(csvFilename()))
    #shiny::req(output$outputKable)
    
    res <- composeTable()
    
    message(paste0("debug output$download values$wuid=", rv$wuid))
    message(nrow(res))
    if (is.null(res) || nrow(res) == 0){
      msg <- "The download is not possible yet. "
      HTML(msg)
    }else{
      message(paste0("debug output$download rv$wuid=", rv$wuid))
      if (isFALSE(is.null(rv$wuid))){
        wuUrl <- paste0("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=",
                        rv$wuid, "', '_blank')")
        
        actionButton("download",
                     paste("go to B-Fabric workunit", rv$wuid),
                     onclick = wuUrl)
      }else{
        #if (file.exists(csvFilename())){
          actionButton('generate', 'Upload configuration to b-fabric')
      #  }
        
      }
    }
  })
  
  

  bfabricUploadResource <- observeEvent(input$generate, {
    progress <- shiny::Progress$new(min = 0, max = 1)
    progress$set(message = "upload csv file to bfabric")
    on.exit(progress$close())
    
    #if (rv$download_flag  0){
    message(paste0("writing csv file ", csvFilename()," ..."))
    
    cat("Bracket Type=4\r\n",
        file = csvFilename(),
        append = FALSE)
    utils::write.table(composeTable(),
                       sep = ',',
                       file = csvFilename(),
                       row.names = FALSE,
                       append = TRUE,
                       quote = FALSE,
                       eol = '\r\n')
    
    message("uploading to bfabric ...")
    progress$set(message = "uploading csv file with plate info to bfabric")
    rr <- bfabricShiny::uploadResource(
      login = bf$login(),
      webservicepassword = bf$webservicepassword(),
      posturl = posturl(),
      containerid = input$orderID,
      applicationid = 319,
      status = "PENDING",
      description = "plate queue generator csv file",
      #inputresourceid = rv$bfrv2$resource[[1]]$id,
      workunitname = sprintf("XCaliburMSconfiguration_orderID-%s", input$orderID),
      resourcename = sprintf("queue-C%s_%s.csv", input$orderID,
                             format(Sys.time(), format="%Y%m%d-%H%M%S")),
      file = csvFilename()
    )
    rv$wuid <- rr$workunit$res[[1]]$id
    #}
  })
  
})
