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
  instruments <- list(Metabolomics = c("QEXACTIVEHF_3", "QUANTIVA_1", "QEXACTIVE_2", "QEXACTIVE_3"),
		      Proteomics = c("QEXACTIVEHF_2", "QEXACTIVEHF_4", "QEXACTIVE_2", "FUSION_2", "EXPLORIS_1", "EXPLORIS_2", "LUMOS_1", "LUMOS_2"))
  
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


  output$orderID <- renderUI({
    shiny::req(user())
    numericInput(
      "orderID",
      "order ID",
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
    shiny::req(read_plateid())
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


  read_plateid <- reactive({
	  shiny::req(user())
	  shiny::req(input$orderID)
	  res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
				    posturl = posturl(),
				    endpoint = "plate",
				    query = list('containerid' = input$orderID))[[1]]
	  plate_ids <- c()
	  for (r in 1:length(res)){
	          plate_ids <- append(plate_ids, res[[r]]$`_id`)
	  }
	  validate(
		   need(try(length(plate_ids) > 0), "There are no plate defined for this order")
		   )
	  message(plate_ids)
	  sort(plate_ids)
  })
  
  output$area <- renderUI({
    shiny::req(input$orderID)
    selectInput(
      "area",
      "Area",
      c("Proteomics","Metabolomics"),
      multiple = FALSE,
      selected = "",
      selectize = TRUE
    )
  })

  output$instrument <- renderUI({
    shiny::req(input$orderID)
    shiny::req(input$area)
    selectInput(
      "instrument",
      "Instrument",
      instruments[input$area],
      multiple = FALSE,
      selected = "",
      selectize = TRUE
    )
  })
  
  read_sample <- function(sampleid){
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "sample",
                              query = list('id' = sampleid))[[1]]
    if ( debugmode == TRUE ) {message(res)}
    samplename <- res[[1]]$name
    if (is.null(res[[1]]$parent)){
        sampletype <- res[[1]]$type
    } else {
	sampletype <- read_sampletype(res[[1]]$parent[[1]]$`_id`)
    }
    list("name" = samplename, "type" = sampletype)
  }

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
    read_sampletype(res[[1]]$parent[[1]]$`_id`)
  }

  read_plate <- function(plateid) {
    shiny::req(user())
    message(paste("Reading plate ", plateid))
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "plate",
                              query = list('id' = plateid))[[1]]
    sample_ids <- c()
    gridposition <- c()
    samplename <- c()
    sampletype <- c()
    samplelist <- res[[1]]$sample
    order_idx <- get_reshuffled_position(samplelist)
    samplelist <- samplelist[c(unlist(order_idx["bio_sample"]), unlist(order_idx["control"]))]
    filename <- c()
    paths <- c()
    injvol <- rep(input$injvol, length(samplelist))
    laboratory <- rep("FGCZ", length(samplelist))
    instrument <- c()
    if ( debugmode==TRUE) {
	    message("test")
            message(length(res[[1]]$sample))
            message(res[[1]]$sample[[2]]$`_id`)
    }
    message(paste("Reading", length(samplelist), "samples"))
    showNotification(paste("Reading", length(samplelist), "samples"))
    for (r in 1:length(samplelist)){
      currentdate <- format(Sys.time(), "%Y%m%d")
      sampleid <- samplelist[[r]]$`_id`
      message("Reading sample ID ", sampleid)
      sample_ids <- append(sample_ids, sampleid)
      gridposition <- append(gridposition, samplelist[[r]]$`_gridposition`)
      sample_info <- read_sample(samplelist[[r]]$`_id`)
      samplename <- append(samplename, sample_info["name"])
      sampletype <- append(sampletype, sample_info["type"])
      runnumber <- r
      runnumber <- formatC(runnumber, width = 3, format = "d", flag = "0")
      if (sample_info["type"] == "Control Sample"){
	      filename <- append(filename, paste0(currentdate, "_C", input$orderID, "_", runnumber, "_S", sampleid, "_control"))
	      instrument <- append(instrument, "C:\\Xcalibur\\methods")
      } else if (sample_info["type"] == "Biological Sample - Metabolomics"){
	      filename <- append(filename, paste0(currentdate, "_C", input$orderID, "_", runnumber, "_S", sampleid, "_", sample_info["name"]))
	      instrument <- append(instrument, "")
      } else {
	      filename <- append(filename, paste0(currentdate, "_C", input$orderID, "_", runnumber, "_S", sampleid, "_check_sample_type"))
	      instrument <- append(instrument, "")

      }
      paths <- append(paths, paste0("D:\\Data2San\\p", input$orderID, "\\Metabolomics\\", input$instrument, "\\analytic_", currentdate))
    }

    validate(
      need(try(length(sample_ids) > 0), "There are no sample defined for this plate id")
    )
    data.frame("File Name" = filename,
	       "Path" = paths,
	       "Position" = gridposition,
	       "Inj Vol" = injvol,
	       "L3 Laboratory" = laboratory,
	       "Sample ID" = sample_ids,
	       "Sample Name" = unlist(samplename),
	       "Instrument Method" = instrument,
	       "Sample Type" = unlist(sampletype),
               stringsAsFactors = FALSE)
  }
  

  get_reshuffled_position <- function(samplelist){
       # order samplelist by _position to get the runnumber
       order_by_position <- order(sapply(samplelist, function(x) as.numeric(x$`_position`)))
       set.seed(872436)
       control_raw <- "H"
       order_sample <- c()
       order_control <- c()
       for (f in order_by_position){
           if (grepl(control_raw, samplelist[[f]]$`_gridposition`, fixed=TRUE)){
               order_control <- append(order_control, f)
           } else {
               order_sample <- append(order_sample, f)
           }
       }
       order_sample_rand <- sample(order_sample)
       list("bio_sample" = order_sample_rand, "control" = order_control)
  }

  getTable <- reactive({
    shiny::req(input$instrument)
    shiny::req(input$injvol)
    shiny::req(input$plateID)
    message(paste("Creating table for plate ID =", input$plateID))
    df <- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method", "Sample Type")
    L <- unique(input$plateID)
    for (i in seq(1,length(L))){
	plate_info <- read_plate(L[[i]])
        message(plate_info)
        plate_info$Position <- paste0(i,":",plate_info$Position)
        df <- rbind(df , plate_info)
	message(paste("Plate", L[[i]], "added"))
    }
    colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method", "Sample Type")
    df
  })

  observeEvent(input$run,{
      showNotification("Extracting plate information")
      output$outputKable <- function(){
        table <- getTable()
        message(table)
        table |>
          kableExtra::kable() |>
          kableExtra::kable_styling("striped", full_width = FALSE)
      }
  })

   csvFilename <- reactive({
       tempdir() |>
	   file.path(sprintf("fgcz-queue-generator_%s_plate%s.csv",  input$instrument, input$plateID[[1]]))
   })

  output$run <- renderUI({
      shiny::req(input$instrument)
      shiny::req(input$plateID)
      shiny::req(input$injvol)
     # rv$run_table <- 1
      actionButton("run", "Create table")
 })

  output$downloadReportButton <- renderUI({
      shiny::req(input$instrument)
      shiny::req(input$plateID)
      shiny::req(input$injvol)
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
	     containerid = 3000, #input$orderID,
	     applicationid = 212,
	     status = "PENDING",
             description = "",
             inputresourceid = rv$bfrv2$resource[[1]]$`_id`,
	     workunitname = sprintf("XCaliburMSconfiguration_orderID-%s_plateID-%s", input$orderID, input$plateID[[1]]),
             resourcename = sprintf("plateID-%s_info_%s.csv", input$plateID[[1]], format(Sys.time(), format="%Y%m%d-%H%M")),
             file = csvFilename()
	     )
	     print(rv$bfrv2)
	    }
 })


})
