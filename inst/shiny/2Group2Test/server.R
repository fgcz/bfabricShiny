## TESTESTTEST ##
## shiny::runApp("inst/shiny/2Group2Test",port=1234, host="130.60.81.134")
## shiny::runApp('C:/Users/wolski/prog/SRMService/inst/shiny/2Group2Test', port = 1234, host=)

stopifnot(
	require(SRMService),
	require(base64enc),
	require(PKI),
	require(bfabricShiny))

options(shiny.maxRequestSize = 30 * 1024^2)

# Define server logic required to draw a histogram ----
shinyServer( function(input, output, session) {

  bf <- callModule(bfabric,
                   "bfabric8",
                   applicationid = c(168, 185, 224, 286),
                   resoucepattern = 'zip$',
                   resourcemultiple = TRUE)
  grp2 <- NULL

  v_upload_file <- reactiveValues(data = NULL, filenam = NULL, protein = NULL,
                                  condition = NULL,
                                  inputresourceID = NULL)
  v_download_links <- reactiveValues(filename = NULL)
  
  rv <- reactiveValues(download_flag = 0)

  getWorkDir <- function(){
    tmpdir <- tempdir()
    workdir <- file.path(tmpdir, gsub(" |:","_",date()))
    return(workdir)
  }

  rawFileNames <- reactive({
    if (is.null(v_upload_file$protein)){
      NULL
    }else{
      rv <- gsub("Intensity\\.", "", grep("Intensity\\.",colnames(v_upload_file$protein), value = TRUE) )
      v_upload_file$condition <- rep(NA, length(rv))
    }
    rv
  })


  #observes file upload ----
  loadProteinGroups <- observeEvent(input$load,{
    resources <- bf$resources()

    v_upload_file$inputresourceID = resources$resourceid[resources$relativepath == input$relativepath][1]
    print(v_upload_file$inputresourceID)

    filename <- file.path('/srv/www/htdocs/', input$relativepath)
    # for debugging on my windows machine.
    # filename <- file.path('Y:', input$relativepath)

    print(input$relativepath)

    v_upload_file$filenam <- filename

    if (file.exists(v_upload_file$filenam)){
      v_upload_file$protein <- bfabricShiny:::.unzip(zipfile = filename, file = 'proteinGroups.txt')
    }else{
      v_upload_file$protein <- bfabricShiny:::.ssh_unzip(zipfile = filename,
                                                         file = 'proteinGroups.txt',
                                                         user=bf$login(),
                                                         host='fgcz-r-035.uzh.ch')
    }
  })



  ## Create output button
  output$generatereportbutton <- renderUI({
    if(is.null(v_upload_file$filenam)){
      NULL
    }else{
      actionButton("generateReport", "Generate Report" )
    }
  })
  output$test <- renderUI({HTML("TEST")})

  annotation <- reactive({
    protein <- v_upload_file$protein

    ## Prepare annotation table ###
    rawF <- rawFileNames()
    condition <- v_upload_file$condition
    #quantable::split2table(rawF)[,3]


    annotation <- data.frame(Raw.file = rawF,
                             Condition = condition,
                             BioReplicate = paste("X",1:length(condition), sep=""),
                             Run = 1:length(condition),
                             IsotopeLabelType = rep("L",length(condition)), stringsAsFactors = FALSE)

    v_upload_file$annotation <- annotation
    v_upload_file$minPeptides <- max(protein$Peptides)


    v_upload_file$pint <- protein[, grep("Intensity\\.", colnames(protein))]
    v_upload_file$maxNA <- ncol(v_upload_file$pint)
    v_upload_file$maxMissing <- ncol(v_upload_file$pint) - 4

    annotation
  })

  # UI - fileInformation ----
  output$fileInformation <- renderUI({
    if(is.null(v_upload_file$filenam)){
      ("Please choose and load a MaxQuant resouce zip file.")
    }else{

      annotation <- annotation()
      protein <- v_upload_file$protein

      ## number of peptides plot ####
      nrPep <- cumsum(rev(table(protein$Peptides)))
      nrPeptidePlot <- renderPlot(barplot(nrPep[(length(nrPep)-5):length(nrPep)],
                                        ylim=c(0, length(protein$Peptides)),
                                        xlab='nr of proteins with at least # peptides'))


      ## number of NA's plot ###
      pint <- v_upload_file$pint
      pint[pint == 0] <- NA


      pint2 <- pint[protein$Peptides >= 2,]
      nrNA <- apply(pint , 1, function(x){sum(is.na(x))})
      nrNA2 <- apply(pint2 , 1, function(x){sum(is.na(x))})

      naPlot <- renderPlot({
        par(mfrow=c(1,2))
        barplot((table(nrNA)),xlab="nr of NA's per protein (1 or more peptides)")
        barplot((table(nrNA2)),xlab="nr of NA's per protein (2 or more peptides)")
      })

      v_upload_file$pint2 <- pint[protein$Peptides >= 2,]
      v_upload_file$conditions <- rownames(table(annotation$Condition))

      version <- packageVersion("SRMService")
      ## prepare gui output
      list(renderTable(annotation),
           renderTable(table(annotation$Condition)),
           nrPeptidePlot,
           naPlot,
           HTML(paste("input resource : " ,v_upload_file$filenam,
                      "SRMService package version : ",
                      version, "----"
                      ,sep = "<br/>")))
    }
  })

  # UI Parameter ----
  output$parameterUI <- renderUI({

    if (is.null(v_upload_file$filenam)) {
      ("Please choose and load a MaxQuant resouce zip file.")
    } else{
      annotation <- annotation()
      sp_title <- strsplit(input$relativepath, "/")[[1]]
      if (nrow(annotation) > 0) {
        list(
          textInput(
            "experimentID",
            "Experiment Title Name",
            paste("MQ-report", sp_title[2], sp_title[9], sep = '-')
          ),
          tags$hr(),
          textInput("inGroup1", "Set Group1 Label", "Group1"),
          textInput("inGroup2", "Set Group2 Label", "Group2"),
          tags$hr(),
          selectInput(
            "selectGroup1",
            "Group1",
            choices = annotation$Raw.file,
            multiple = TRUE
          ),
          selectInput(
            "selectGroup2",
            "Group2",
            choices = annotation$Raw.file,
            multiple = TRUE
          ),
          actionButton("updateConditions", "Update Conditions"),
          selectInput(
            "select",
            label = h3("Select Reference"),
            choices = annotation$Condition,
            selected = 1
          ),
          numericInput(
            "minPeptides",
            "Nr of Peptides per protein:",
            2,
            max = v_upload_file$minPeptides
          ),
          numericInput(
            "maxMissing",
            "Maximum number of NAs: ",
            value = v_upload_file$maxMissing,
            min = 0,
            max = v_upload_file$maxNA
          ),
          tags$hr(),
          numericInput(
            "qValue",
            "q value threshold",
            value = 0.05,
            min = 0,
            max = 1,
            step = 0.01
          ),
          numericInput(
            "FCthreshold",
            "foldchange threshold",
            value = 2,
            min = 0,
            step = 0.05
          ),
          selectInput(
            inputId = "normalization",
            label = "Intensity Normalization Method",
            choices = c("robustscale", "none"),
            selected = 1
          )
        )
      }
    }
  })

  observe({
    x1 <- input$inGroup1
    x2 <- input$inGroup2
    # Can use character(0) to remove all choices
    if (is.null(x1))
      x1 <- character(0)
    if (is.null(x2))
      x2 <- character(0)
    # Can also set the label and select items
    updateSelectInput(session, "selectGroup1",
                      label = x1
    )
    # Can also set the label and select items
    updateSelectInput(session, "selectGroup2",
                      label = x2
    )

  })

  updateCondition <- observeEvent(input$updateConditions,{
    raw <- rawFileNames()
    v_upload_file$condition[raw %in% input$selectGroup1] <- input$inGroup1
    v_upload_file$condition[raw %in% input$selectGroup2] <- input$inGroup2
  })


  ## Create some summary of the loaded data

  progress <- function(howmuch, detail){
    incProgress(howmuch, detail = detail)
  }

  # generateReport eventReactive(input$generateReport, ----

  ## react on GO button
  ## this method does all the computation
  generateReport <- eventReactive(input$generateReport, {
    #here will processing happen!
    if (is.null(v_upload_file$protein)) {
      print("No protein report was uploaded")
    }
    withProgress(message = 'Generating Report', detail = "part 0", value = 0, {
      ### Rendering report

      print(names(input))
      annotation <- input$fileInformation
      print("Annotation!")

      cat("SELECT", input$select, "\n")
      grp2 <- SRMService::Grp2Analysis(v_upload_file$annotation,
                                       input$experimentID,
                                       projectID = bf$projectid(),
                                       workunitID = bf$workunitid(),
                                       maxNA = input$maxMissing,
                                       nrPeptides = input$minPeptides,
                                       reference = input$select,
                                       normalizationMethod = input$normalization
      )


      grp2$setMQProteinGroups(v_upload_file$protein)
      grp2$setQValueThresholds(qvalue = input$qValue , qfoldchange = input$FCthreshold)
      incProgress(0.1, detail = paste("part", "Set up objects"))


      workdir <- getWorkDir()

      #if(dir.exists(workdir)){
      #  paste(workdir)
      #}
      message("will be processing in ", workdir)
      if (!dir.exists(workdir)) {

        if (!dir.create(workdir)) {
          warning("can't create", workdir)
          stopApp(7)
        }
      }else{
        message("workdir Already Exists :", workdir)
      }

      SRMService::RMD_MQ_Quant_2GrpAnalysis(workdir = workdir)
      rmdfile2run <- file.path(workdir ,"Grp2Analysis.Rmd")

      # generate the LFQ report
      rmarkdown::render(rmdfile2run,
                        bookdown::pdf_document2(),
                        params = list(grp = grp2))

      incProgress(0.1, detail = paste("part", "Rendering"))
      v_download_links$pdfReport <- file.path(workdir, "Grp2Analysis.pdf")

      ### Writing p-values
      write.table(grp2$getResultTableWithPseudo(), file=file.path(workdir,"pValues.txt"), quote=FALSE, sep = "\t", col.names=NA)
      incProgress(0.1, detail = paste("part", "report"))
      v_download_links$tsvTable <- file.path(workdir,"pValues.txt")
    })
    return(v_download_links$filename)
  })


  # UI downolad Report ----
  output$downloadreport <- renderUI({
    files <- generateReport()
    downloads <- c("downloadReport"="Download Report (.pdf)", "downloadData" = "Data (.txt)")
    ll <- list()
    for(i in 1:length(downloads)){
      ll[[i]] <- downloadButton(names(downloads)[i], label=downloads[i])
    }
    return(ll)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$experimentID, "txt", sep = ".")
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      cat("file",file,"\n")
      print(v_download_links$tsvTable)
      # Write to a file specified by the 'file' argument
      file.copy(v_download_links$tsvTable, file)
    }
  )

  #------------------- downloadReport --------
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$experimentID, "pdf", sep = ".")
    },
    content = function(file) {

      rv$download_flag <- rv$download_flag + 1
      
      file.copy(v_download_links$pdfReport, file)

    }
  )

 
  .backup <- function(){
    {
      if(! file.exists(v_download_links$pdfReport)){
        warning("File does not exist" , v_download_links$pdfReport)
      }
      
      file_pdf_content <- base64enc::base64encode(readBin(v_download_links$pdfReport, "raw",
                                                          file.info(v_download_links$pdfReport)[1, "size"]), "pdf")
      
      wuid <- bfabric_upload_file(login = bf$login(),
                                  webservicepassword = bf$webservicepassword(),
                                  projectid = bf$projectid(),
                                  file_content = file_pdf_content,
                                  inputresource = v_upload_file$inputresourceID,
                                  workunitname = input$experimentID,
                                  resourcename = paste0(input$experimentID, ".pdf"),
                                  applicationid = 217)
      
      message(wuid)
      
      if(! file.exists(v_download_links$tsvTable)){
        warning("File does not exist" , v_download_links$tsvTable)
      }
      file_csv_content <- base64enc::base64encode(readBin(v_download_links$tsvTable, "raw",
                                                          file.info(v_download_links$tsvTable)[1, "size"]), "txt")
      
      bfabricShiny:::.saveResource(login = bf$login(),
                                   webservicepassword = bf$webservicepassword(),
                                   workunitid = wuid,
                                   content = file_csv_content,
                                   name =  paste0(input$experimentID, ".txt"))
    }### copy to b-fabric
    
  }
  
  #------------------- uploadResource --------
  bfabricUploadResource <- observeEvent(rv$download_flag, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "upload report to bfabric")
    on.exit(progress$close())
    
    if (rv$download_flag > 0){
      message("bfabricUpload")
      print("bfabricUpload")
      
      progress$set(message = "uploading Quantify Sample Summary Reports file to bfabric")
      rv$bfrv1 <- bfabricShiny::uploadResource(
        login = bf$login(),
        webservicepassword = bf$webservicepassword(),
        containerid = bf$projectid(),
        applicationid = 217,
        status = "PENDING",
        description = "",
        inputresourceid = v_upload_file$inputresourceID,
        workunitname = "MaxQuant2Gr",
        resourcename = sprintf("MaxQuant2Gr-C%s-%s",
                               bf$projectid(),
                               format(Sys.time(),
                                      format="%Y%m%d-%H%M")),
        file = v_download_links$pdfReport
      )
      
      if(file.exists(v_download_links$tsvTable)){
        
        file_csv_content <- base64enc::base64encode(readBin(v_download_links$tsvTable, "raw",
                                                            file.info(v_download_links$tsvTable)[1, "size"]), "txt")
        
        bfabricShiny:::.saveResource(login = bf$login(),
                                     webservicepassword = bf$webservicepassword(),
                                     workunitid = rv$bfrv1$workunit[[1]]$`_id`,
                                     content = file_csv_content,
                                     name =  paste0(input$experimentID, ".txt"))
      }else{
        warning("File does not exist" , v_download_links$tsvTable)
      }
    }
  })
  
  #------------------- sessionInfo --------
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
})
