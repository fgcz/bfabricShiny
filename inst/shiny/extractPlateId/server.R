#R 
# 2023 Maria dErrico
# 2024-07 Christian Panse <cp@fgcz.ethz.ch>

stopifnot(require(shiny),
          require(bfabricShiny),
          require(stringr))

if (file.exists("configs.R")){ source("configs.R") }else{stop("can not load queue configs.")}
if (file.exists("configProteomics.R")){ source("configProteomics.R") }else{stop("can not load queue configProteomics.")}
#if (file.exists("configs.R")){ source("helper.R") }else{stop("can not load queue helper.")}

# Define server logic required
shinyServer(function(input, output) {
  
  debugmode <- FALSE
  TIMEdebugmode <- FALSE
  
  
  configInstrument <- reactive({
    f <- file.path(system.file(package = "bfabricShiny"), "extdata", 
                   "instrument.csv")
    stopifnot(file.exists(f))
    read.table(f, header = TRUE, sep = ";")
  })
  
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
  
  
  ## TODO(cp): rename that rv to make it clear it is a global var
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
  # ------ input area ------
  output$area <- renderUI({
   
    shiny::req(input$orderID)
    
    ccc <- container()[[1]]$technology[[1]]
    print(ccc)
    # browser()
   
    if (grepl("Proteomics", ccc)){
      area <- "Proteomics"
    }else{
      area <- "Metabolomics"
    }
    
    selectInput(
      "area",
      "Area:",
      area,
      multiple = FALSE,
      selected = area,
      selectize = FALSE
    )
  })
  # ------ input system ------
  output$system <- renderUI({
    shiny::req(input$area)
    systems <- configInstrument()$system[configInstrument()$area == input$area]
    
    selectInput(
      "system",
      "System:",
      unique(systems),
      multiple = FALSE,
      selected = unique(systems)[1],
      selectize = TRUE
    )
  })
  # ------ input instrument ------
  output$instrument <- renderUI({
    shiny::req(input$orderID)
    shiny::req(input$area)
    
    instruments <- configInstrument()$instrument[configInstrument()$area == input$area &
                                                   configInstrument()$system == input$system] |> sort()
    
    selectInput(
      "instrument",
      "Instrument:",
      instruments,
      multiple = FALSE,
      selected = instruments[1],
      selectize = TRUE
    )
  })
  
  #input queue configuration FUN ------------
  output$selectqFUN <- renderUI({
    shiny::req(input$area)
    shiny::req(input$system)
    shiny::req(read_plateid())
    
    c("qconfigProteomicsEVOSEP6x12x8PlateHystar", "qconfigMetabolomicsPlateXCalibur",
      "qconfigMetabolomicsVialXCalibur") -> qc
    ## filter for area and system
    qc[ base::grepl(pattern = input$area, x = qc) ] -> qc
    qc[ base::grepl(pattern = input$system, x = qc) ] -> qc
    
    if (length(read_plateid()) > 0){
      qc[ base::grepl(pattern = "Plate", x = qc) ] -> qc
    }else{
      qc[ base::grepl(pattern = "Vial", x = qc) ] -> qc
    }
    
    shiny::selectInput(inputId = "qFUN", 
                       label = "Queue configuration:",
                       choices = qc,
                       multiple = FALSE,
                       selected = qc[1],
                       selectize = FALSE)
  })
  
  # input orderID ------------
  output$orderID <- renderUI({
    shiny::req(user())
    
    if (user()$login == 'cpanse'){
      return( selectInput(
        "orderID",
        "Order ID:",
        c("31741", "35116", "35464", "34843", "34777", "34778"),
        selected = "31741",
        multiple = TRUE,
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
  
  # input plateID ------------
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
  
  # input injvol ------------ 
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
  
  #input randomization ------------
  output$randomization <- renderUI({
    shiny::radioButtons("randomization", "Randomization:",
                        c("no" = "no",
                          "plate" = "plate",
                          "all" = "all"), inline = TRUE)
  })
  
  # input orderID ------------
  output$frequency <- renderUI({
    shiny::req(input$orderID) 
      selectInput(
        "frequency",
        "QC frequency:",
        c(1, 2, 4, 8, 16, 32, 48, 64, 1024),
        selected = "16",
        multiple = FALSE,
        selectize = 16
      )
      
    })
  
  # input check sample selection -------------
  output$checkSampleSelection <- renderUI({
    if (length(input$plateID) == 0){
      shiny::checkboxInput("booleanSampleSelection",
                           "Subsetting samples", value = FALSE)
    }else{NULL}
  })
  
  # input select sample ------------
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
  
  
  output$instrumentMode <- renderUI({
    shiny::req(input$instrument)
    
    if (input$area == "Metabolomics"){
      shiny::radioButtons("mode", "Mode:",
                          c("neg" = "_neg",
                            "pos" = "_pos"), 
                          inline = TRUE)
    }else{NULL}
  })
  
  # input extratext (not used)  ------------
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
  
  # input extrameasurement ------------
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
  
  
  # output$download --------------------
  output$download <- renderUI({
    res <- composeTable()
    
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
  
  
  # FUNCTIONS ===========================
  
  # read_plateid FUN ------------
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
  
  
  # read container from bfabric -----------------
  # bfabricShiny::read(login = login, webservicepassword = webservicepassword, posturl = bfabricposturl, endpoint = 'container', query = list('id' = list(34778, 35116)))$res -> rv
  container <- reactive({
    shiny::req(input$orderID)
    shiny::req(bf$login())
    shiny::req(bf$webservicepassword())
    
    res <- bfabricShiny::read(login = bf$login(),
                              webservicepassword = bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "container",
                              maxitems = 100,
                              query = list('id' = input$orderID))$res
    
    #browser()
    res
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
    #df$`Sample Name` <- paste0(df$`Sample Name`, mode)
    if (input$area == "Metabolomics"){
      do.call(what = input$qFUN, args = list(x = df, QCrow = QCrow, mode = instrumentMode, howOftenQC = as.integer(input$frequency))) |>
        .replaceRunIds()
    }else{
      do.call(what = input$qFUN, args = list(x = df, howOftenQC = as.integer(input$frequency))) |>
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
    tempfile(pattern = "fgcz_queue_generator_XCalibur", fileext = ".csv")
  })
  xmlFilename <- reactive({
    tempfile(pattern = "fgcz_queue_generator_Hystar.", fileext = ".xml")
  })

  
  # ObserveEvents ===================================
  
  # observe selected sample ---------------
  oeSelectedSample <- observeEvent(input$selectedSample, {
    shiny::req(input$booleanSampleSelection)
    
    if (input$booleanSampleSelection){
      rv$selectedSample <- input$selectedSample
    }
  })

  # upload to bfabric --------------
  bfabricUploadResource <- observeEvent(input$generate, {
    progress <- shiny::Progress$new(min = 0, max = 1)
    progress$set(message = "upload csv file to bfabric")
    on.exit(progress$close())
    
    filename <- NULL
    workunitname <- NULL
    resourcename <- NULL
    #browser()
    if(grepl("Hystar", input$qFUN)){
      composeTable() |>
        .toHystar(file = xmlFilename())
      filename <- xmlFilename()
      workunitname <- sprintf("Hystar-MS-configuration_orderID-%s", input$orderID[1])
      resourcename = sprintf("queue-C%s_%s.xml",
                             input$orderID[1],
                             format(Sys.time(), format="%Y%m%d-%H%M%S"))
    }else{
      message(paste0("Writing XCalibur csv config file ",
                     csvFilename(), " ..."))
      
      ## to make it readable by XCalibur
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
      filename <- csvFilename()
      workunitname <- sprintf("XCalibur-MS-configuration_orderID-%s", input$orderID[1])
      resourcename = sprintf("queue-C%s_%s.csv",
                             input$orderID[1],
                             format(Sys.time(), format="%Y%m%d-%H%M%S"))
    }
    
    
    message("Uploading queue config file to bfabric ...")
    progress$set(message = "uploading config file with plate info to bfabric")
    
    rr <- bfabricShiny::uploadResource(
      login = bf$login(),
      webservicepassword = bf$webservicepassword(),
      posturl = posturl(),
      containerid = input$orderID,
      applicationid = 319,
      status = "PENDING",
      description = "plate queue generator config file",
      workunitname = workunitname,
      resourcename = resourcename,
      file = filename
    )
    
    ## save wuid
    rv$wuid <- rr$workunit$res[[1]]$id
  })
})

