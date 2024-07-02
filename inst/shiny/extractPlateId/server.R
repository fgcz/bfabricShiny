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
  TIMEdebugmode <- FALSE
  instruments <- list(Metabolomics = c("QEXACTIVEHF_3", "QUANTIVA_1", "QEXACTIVE_2", "QEXACTIVE_3"),
		      Proteomics = c("QEXACTIVEHF_2", "QEXACTIVEHF_4", "QEXACTIVE_2", "FUSION_2", "EXPLORIS_1", "EXPLORIS_2", "LUMOS_1", "LUMOS_2"))
  plate_idx <- c("Y", "G", "R", "B")
  currentdate <- format(Sys.time(), "%Y%m%d")

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
      u <- bfabricShiny::read(login = bf$login(),
                              webservicepassword = bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = 'user',
                              query = list(login = bf$login()))$res[[1]])
    validate(shiny::need(try(u$login == bf$login()),
                         "Please provide valid bfabric login and webservicepassword."))
    message(paste("Request started from user", u$login))
    
    #browser()
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
	  start_plate <- Sys.time()
          message(paste("TIME info current before reading plate ID from order ID:", Sys.time()))
	  res <- bfabricShiny::read(bf$login(),
	                            bf$webservicepassword(),
				    posturl = posturl(),
				    endpoint = "plate",
				    query = list('containerid' = input$orderID))$res
	  
	  end_plate <- Sys.time()
          if ( TIMEdebugmode == TRUE ) { message(paste("TIME to read plateIDs from order ID ", input$orderID, end_plate-start_plate))}
	  plate_ids <- c()
	  for (r in 1:length(res)){
	          plate_ids <- append(plate_ids, res[[r]]$id)
	  }
	  validate(
		   need(try(length(plate_ids) > 0), "There are no plate defined for this order")
		   )
	  message(paste("TIME info for plate ID", plate_ids, Sys.time()-end_plate))
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


  read_sample <- function(samplelist){
    start_fullquery <- Sys.time()
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "sample",
                              query = list('id' = samplelist))[[1]]
    end_fullquery <- Sys.time()
    message(paste("TIME info loop processing full loop:", end_fullquery-start_fullquery))
    if ( debugmode == TRUE ) {message(res[[1]])}
    samplename <- c()
    #sampletype <- c()
    filename <- c()
    for (r in 1:length(res)){
      samplename <- append(samplename, res[[r]]$name)
      #if (is.null(res[[r]]$parent)){
      #    sampletype <- append(sampletype, res[[r]]$type)
      #} else {
      #    message(paste("Enter recursive function for sample ID", res[[r]]$parent[[1]]$id, ": parent info present."))
      #	  sampletype <- append(sampletype, read_sampletype(res[[r]]$parent[[1]]$id))
      #}
      # the run number is added to the file name in the final data frame
      filename <- append(filename, paste0(currentdate, "_C", input$orderID, "_S", res[[r]]$id, "_", res[[r]]$name))
    }
    #list("samplename" = samplename, "sampletype" = sampletype, "filename" = filename)
    list("samplename" = samplename, "filename" = filename)
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
    read_sampletype(res[[1]]$parent[[1]]$id)
  }

  read_plate <- function(plateid) {
    shiny::req(user())
    message(paste("Reading plate ", plateid))
    start_sample <- Sys.time()
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "plate",
                              query = list('id' = plateid))[[1]]
    end_sample <- Sys.time()
    if ( TIMEdebugmode == TRUE ) {message(paste("TIME info - querying the plate ID:", end_sample-start_sample))}
    sample_ids <- c()
    gridposition <- c()
    samplename <- c()
    #sampletype <- c()
    samplelist <- res[[1]]$sample
    order_idx <- get_reshuffled_position(samplelist)
    samplelist <- samplelist[c(unlist(order_idx["bio_sample"]), unlist(order_idx["control"]))]
    filename <- c()
    paths <- c()
    if (input$extratext == ""){
	    paths <- rep(paste0("D:\\Data2San\\p", input$orderID, "\\", input$area, "\\", input$instrument, "\\", bf$login(), "_", currentdate), length(samplelist))
    } else {
	    paths <- rep(paste0("D:\\Data2San\\p", input$orderID, "\\", input$area, "\\", input$instrument, "\\", bf$login(), "_", currentdate, "_", input$extratext), length(samplelist))
    }
    injvol <- rep(input$injvol, length(samplelist))
    laboratory <- rep("FGCZ", length(samplelist))
    if (input$area == "Proteomics"){
        instrument <- rep("", length(samplelist))
    } else {
	instrument <- rep(paste0("D:\\Data2San\\p", input$orderID, "\\", input$area, "\\", input$instrument, "\\methods"), length(samplelist))
    }
    if ( debugmode==TRUE) {
	    message("test")
            message(length(res[[1]]$sample))
            message(res[[1]]$sample[[1]]$id)
    }
    message(paste("Reading", length(samplelist), "samples"))
    showNotification(paste("Reading", length(samplelist), "samples"))
    if ( TIMEdebugmode == TRUE ) {message(paste("TIME info about starting sample info processing", Sys.time() - end_sample))}
    start_loop <- Sys.time()
    for (r in 1:length(samplelist)){
      sampleid <- samplelist[[r]]$id
      message("Reading sample ID ", sampleid)
      sample_ids <- append(sample_ids, sampleid)
      gridposition <- append(gridposition, samplelist[[r]]$`_gridposition`)
    }
    start_sampleinfo <- Sys.time()
    sample_info <- read_sample(sapply(samplelist, function(x) as.numeric(x$id)))
    end_sampleinfo <- Sys.time()
    read_sample_info_time <- end_sampleinfo - start_sampleinfo
    samplename <- sample_info["samplename"]
    #sampletype <- sample_info["sampletype"]
    filename <- sample_info["filename"]

    end_loop <- Sys.time()
    message(paste("TIME info about loop processing:", as.numeric(end_loop-start_loop, units = "secs"), "VS read_sample function:", read_sample_info_time))

    validate(
      need(try(length(sample_ids) > 0), "There are no sample defined for this plate id")
    )
    data.frame("File Name" = filename,
	       "Path" = paths,
	       "Position" = gridposition,
	       "Inj Vol" = injvol,
	       "L3 Laboratory" = laboratory,
	       "Sample ID" = sample_ids,
	       "Sample Name" = samplename,
	       "Instrument Method" = instrument,
	       #"Sample Type" = sampletype,
               stringsAsFactors = FALSE)
  }
  

  get_reshuffled_position <- function(samplelist){
       # order samplelist by _position to get the runnumber
       order_by_position <- order(sapply(samplelist, function(x) as.numeric(x$`_position`)))
       set.seed(872436)
       # control_row depends on the type of plate Layout
       control_row <- "H"
       order_sample <- c()
       order_control <- c()
       for (f in order_by_position){
           if (grepl(control_row, samplelist[[f]]$`_gridposition`, fixed=TRUE)){
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
    #df <- data.frame(matrix(ncol = 9, nrow = 0))
    #colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method", "Sample Type")
    df <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
    L <- unique(input$plateID)
    for (i in seq(1,length(L))){
      plate_info <- read_plate(L[[i]])
      if ( debugmode == TRUE ) {message(plate_info)}
      start_postprocessing <- Sys.time()
      if (input$area == "Proteomics"){
        plate_info$Position <- paste(substr(plate_info$Position,1,1),substr(plate_info$Position,2,nchar(L)),sep = ",")
        plate_info$Position <- paste0(i,":",plate_info$Position)
        } else {
          plate_info$Position <- paste0(plate_idx[[i]],":",plate_info$Position)
          }
      df <- rbind(df , plate_info)
      if (input$extrameasurement != ""){
        plate_info$filename <- lapply(plate_info$filename, function(x) {
          paste0(x, "_", input$extrameasurement)})
        plate_info$Path <- lapply(plate_info$Path, function(x) {
          paste0(x, "_", input$extrameasurement)})
        df <- rbind(df , plate_info)
      }
      message(paste("Plate", L[[i]], "added"))
      if ( TIMEdebugmode == TRUE ) {message(paste("TIME info for sample info post-processing:", as.numeric(Sys.time()-start_postprocessing, units="secs")))}
      message(paste("TIME info current after plate ID ", L[[i]], "is processed:", Sys.time()))
    }
    #colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method", "Sample Type")
    colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
    # adding the run number to the file name
    df[["File Name"]] <- lapply(df[["File Name"]], function(x) {
	   filename_split <- unlist(strsplit(x, "_"))
	   runnumber <- match(x,df[["File Name"]])
	   runnumber <- formatC(runnumber, width = 3, format = "d", flag = "0")
	   paste(c(filename_split[c(1:2)], runnumber, filename_split[c(3:length(filename_split))]), collapse = "_")
	 })
    df[["File Name"]] <- sapply(df[["File Name"]], as.character)
    df
  })

  observeEvent(input$run,{
      showNotification("Extracting plate information")
      output$outputKable <- function(){
        table <- getTable()
        if ( debugmode == TRUE ) {message(table)}
        message(paste("TIME info current after table creation:", Sys.time()))
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
          
          rv$download_flag <- rv$download_flag + 1
          utils::write.csv(getTable(), file, row.names = FALSE)
      }
  )

 bfabricUploadResource <- observeEvent(rv$download_flag, {
     progress <- shiny::Progress$new(min = 0, max = 1)
     progress$set(message = "upload csv file to bfabric")
     on.exit(progress$close())
     
     if (rv$download_flag > 0){
         message(paste0("writing csv file ", csvFilename()," ..."))
         S <- getTable()
         #base::save(S, file = "/tmp/SSS.RData")
         
         utils::write.csv(getTable(), csvFilename(), row.names = FALSE)
         message("uploading to bfabric ...")
         progress$set(message = "uploading csv file with plate info to bfabric")
         rv$bfrv2 <- bfabricShiny::uploadResource(
             login = bf$login(),
             webservicepassword = bf$webservicepassword(),
             posturl = posturl(),
             containerid = input$orderID,
             applicationid = 319,
             status = "PENDING",
             description = "",
             inputresourceid = rv$bfrv2$resource[[1]]$id,
             workunitname = sprintf("XCaliburMSconfiguration_orderID-%s_plateID-%s", input$orderID, input$plateID[[1]]),
             resourcename = sprintf("plateID-%s_info_%s.csv", input$plateID[[1]], format(Sys.time(), format="%Y%m%d-%H%M")),
             file = csvFilename()
         )
         print("bfabric return value:")
         print(rv$bfrv2)
     }
 })


})
