#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

stopifnot(require(shiny), require(bfabricShiny))

# Define server logic required
shinyServer(function(input, output) {
  
  debugmode <- FALSE
  m_instruments  <- c("QEXACTIVEHF_3", "QUANTIVA_1", "QEXACTIVE_2", "QEXACTIVE_3")
  
  bf <- callModule(bfabricShiny::bfabricLogin,
                   "bfabric8")
  rv <- reactiveValues(download_flag = 0)
  
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
      u <- bfabricShiny::read(bf$login(),
                              bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint='user',
                              list(login=bf$login()))$res[[1]])
    validate(shiny::need(try(u$login == bf$login()), "Please provide valid bfabric login and webservicepassword."))
    message(paste("Request started from user", u$login))
    return(u)
  })


  orderID <- NULL

  output$plateID <- renderUI({
    numericInput(
      "plateID",
      "plate ID",
      "",
      min = NA,
      max = NA,
      step = NA,
      width = NULL
    )
  })
  
  output$instrument <- renderUI({
    selectInput(
      "instrument",
      "Instrument",
      m_instruments,
      multiple = FALSE,
      selected = "",
      selectize = TRUE
    )
  })
  
  read_sample <- function(sampleid){
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "sample",
                              query = list('id' = sampleid))
    if ( debugmode == TRUE ) {message(res[[1]])}
    samplename <- res[[1]][[1]]$name
    orderid <- res[[1]][[1]]$container$`_id`
    if (is.null(res[[1]][[1]]$parent)){
        sampletype <- res[[1]][[1]]$type
    } else {
	sampletype <- read_sampletype(res[[1]][[1]]$parent[[1]]$`_id`)
    }
    list("name" = samplename, "type" = sampletype, "orderID" = orderid)
  }

  read_sampletype <- function(sampleid){
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "sample",
                              query = list('id' = sampleid))
    if ( debugmode == TRUE ) {message(res[[1]])}
    if (is.null(res[[1]][[1]]$parent)){
        sampletype <- res[[1]][[1]]$type
	return(sampletype)
    }
    read_sampletype(res[[1]][[1]]$parent[[1]]$`_id`)
  }

  read_plate <- reactive({
    shiny::req(user())
    shiny::req(input$plateID)
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "plate",
                              query = list('id' = input$plateID))
    sample_ids <- c()
    gridposition <- c()
    samplename <- c()
    samplelist <- res[[1]][[1]]$sample
    # order samplelist by _position to get the runnumber
    samplelist <- samplelist[order(sapply(samplelist, function(x) as.numeric(x$`_position`)))]
    filename <- c()
    paths <- c()
    injvol <- rep(2, length(samplelist))
    laboratory <- rep("FGCZ", length(samplelist))
    instrument <- c()
    if ( debugmode==TRUE) {
	    message("test")
            message(length(res[[1]][[1]]$sample))
            message(res[[1]][[1]]$sample[[2]]$`_id`)
    }
    for (r in 1:length(samplelist)){
      currentdate <- format(Sys.time(), "%Y%m%d")
      sampleid <- samplelist[[r]]$`_id`
      sample_ids <- append(sample_ids, sampleid)
      gridposition <- append(gridposition, samplelist[[r]]$`_gridposition`)
      sample_info <- read_sample(samplelist[[r]]$`_id`)
      if (is.null(orderID)){
	      orderID <- sample_info["orderID"]
      }
      samplename <- append(samplename, sample_info["name"])
      runnumber <- r
      runnumber <- formatC(runnumber, width = 3, format = "d", flag = "0")
      if (sample_info["type"] == "Control Sample"){
	      filename <- append(filename, paste0(currentdate, "_C", sample_info["orderID"], "_", runnumber, "_S", sampleid, "_control"))
	      instrument <- append(instrument, "C:\\Xcalibur\\methods")
      } else if (sample_info["type"] == "Biological Sample - Metabolomics"){
	      filename <- append(filename, paste0(currentdate, "_C", sample_info["orderID"], "_", runnumber, "_S", sampleid, "_", sample_info["name"]))
	      instrument <- append(instrument, "")
      } else {
	      filename <- append(filename, paste0(currentdate, "_C", sample_info["orderID"], "_", runnumber, "_S", sampleid, "_check_sample_type"))
      }
      paths <- append(paths, paste0("D:\\Data2San\\p", sample_info["orderID"], "\\Metabolomics\\", input$instrument, "\\analytic_", currentdate))
    }

    validate(
      need(try(length(sample_ids) > 0), "There are no sample defined for this plate id")
    )
    list(filename, paths, gridposition, injvol, laboratory, sample_ids, unlist(samplename), instrument)
  })
  

  getTable <- reactive({
    shiny::req(input$plateID)
    shiny::req(input$instrument)
    message(paste("Creating table for plate ID =", input$plateID))
    showModal(modalDialog(
             title = "FGCZ - plate info extraction",
	      paste("Extracting samples information from plate id ", input$plateID),
	      HTML("<br />"),
	      #mes2,
	      #HTML("<br />"),
	      #mes3,
	      easyClose = TRUE,
	      footer = "Footer"
	      ))
    content <- read_plate()
    df <- data.frame(content, check.names=FALSE)
    names(df) <- c("file name", "path", "position", "inj vol", "l3 laboratory", "sample id", "sample name", "instrument method")
    df
  })

  output$outputKable <- function(){
    table <- getTable()
    message(table)
    table |>
      kableExtra::kable() |>
      kableExtra::kable_styling("striped", full_width = FALSE)
  }

   csvFilename <- reactive({
       tempdir() |>
	   file.path(sprintf("fgcz-queue-generator_%s_plate%s.csv",  input$instrument, input$plateID))
   })

  output$downloadReportButton <- renderUI({
      shiny::req(input$instrument)
      shiny::req(input$plateID)
      downloadButton("downloadCSV", "Download CSV")
 })


 output$downloadCSV <- downloadHandler(
     filename = function(){
         basename(csvFilename())
     },
     content = function(file) {
	 message("writing csv file")
         message(getTable())
	 rv$download_flag <- rv$download_flag + 1
	 write.csv(getTable(), csvFilename(), row.names = FALSE)
     }
 )

 bfabricUploadResource <- observeEvent(rv$download_flag, {
    progress <- shiny::Progress$new(min = 0, max = 1)
    progress$set(message = "upload csv file to bfabric")
    on.exit(progress$close())

    if (rv$download_flag > 0){
        message("bfabricUpload")
	progress$set(message = "uploading csv file with plate info to bfabric")
	rv$bfrv2 <- bfabricShiny::uploadResource(
             login = bf$login(),
             webservicepassword = bf$webservicepassword(),
             posturl = posturl(),
	     containerid = 3000, #orderID,
	     applicationid = 212,
	     status = "PENDING",
             description = "",
             inputresourceid = rv$bfrv2$resource[[1]]$`_id`,
	     workunitname = sprintf("XCaliburMSconfiguration_orderID-%s_plateID-%s", orderID, input$plateID),
             resourcename = sprintf("plateID-%s_info_%s.csv", input$plateID, format(Sys.time(), format="%Y%m%d-%H%M")),
             file = csvFilename()
	     )
	     print(rv$bfrv2)
	    }
 })


})
