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
    textInput(
      "instrument",
      "Instrument",
      "",
      width = NULL
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
    sampletype <- c()
    order_id <- c()
    samplelist <- res[[1]][[1]]$sample
    # order samplelist by _position to get the runnumber
    samplelist <- samplelist[order(sapply(samplelist, function(x) as.numeric(x$`_position`)))]
    filename <- c()
    paths <- c()
    if ( debugmode==TRUE) {
	    message("test")
            message(length(res[[1]][[1]]$sample))
            message(res[[1]][[1]]$sample[[2]]$`_id`)
    }
    for (r in 1:length(samplelist)){
      currentdate <- format(Sys.time(), "%Y%m%d")
      sampleid <- samplelist[[r]]$`_id`
      sample_ids <- append(sample_ids, samplelist[[r]]$`_id`)
      gridposition <- append(gridposition, samplelist[[r]]$`_gridposition`)
      sample_info <- read_sample(samplelist[[r]]$`_id`)
      samplename <- append(samplename, sample_info["name"])
      sampletype <- append(sampletype, sample_info["type"])
      runnumber <- r
      runnumber <- formatC(runnumber, width = 3, format = "d", flag = "0")
      order_id <- append(order_id, sample_info["orderID"])
      if (sample_info["type"] == "Control Sample"){
	      filename <- append(filename, paste0(currentdate, "_C", sample_info["orderID"], "_", runnumber, "_S", sampleid, "_control"))
      } else if (sample_info["type"] == "Biological Sample - Metabolomics"){
	      filename <- append(filename, paste0(currentdate, "_C", sample_info["orderID"], "_", runnumber, "_S", sampleid, "_", sample_info["name"]))
      } else {
	      filename <- append(filename, paste0(currentdate, "_C", sample_info["orderID"], "_", runnumber, "_S", sampleid, "_check_sample_type"))
      }
      paths <- append(paths, paste0("D:\\Data2San\\orders\\Proteomics\\", input$instrument, "\\analytic_", currentdate))
    }

    validate(
      need(try(length(sample_ids) > 0), "There are no sample defined for this plate id")
    )
    list(filename, paths, unlist(samplename), sample_ids, gridposition, unlist(sampletype), unlist(order_id))
  })
  
  
  output$outputKable <- function(){
    shiny::req(input$plateID)
    shiny::req(input$instrument)
    content <- read_plate()
    message(content)
    df <- data.frame(content, check.names=FALSE)
    names(df) <- c("File Name", "Path", "Sample Name", "Sample ID", "Position", "sampletype", "order_id")
    df |>
      kableExtra::kable() |>
      kableExtra::kable_styling("striped", full_width = FALSE)
  }
    
  
})
