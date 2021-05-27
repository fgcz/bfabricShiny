
library(bfabricShiny)
library(p389Devel)

stopifnot( packageVersion('p389Devel') >= "0.1.4")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  values <- reactiveValues(pdf = NULL,
                           inputresouceid = NULL,
                           wuid = NULL,
                           notpressed = TRUE)

  bf <- callModule(bfabric, "bfabric8",  applicationid = c(226), resoucepattern = 'zip$')

  # cpanse@fgcz-r-021:~ > unzip -p /srv/www/htdocs/Data2San/p1000/Metabolomics/Analysis/ProgenesisQI/cpanse_20171109_test/p1896_o3296_BPs_HILIC_pH9_SOP_G2_filter08.zip *measurements*csv|wc -l
  #2800
  #cpanse@fgcz-r-021:~ > unzip -vl /srv/www/htdocs/Data2San/p1000/Metabolomics/Analysis/ProgenesisQI/cpanse_20171109_test/p1896_o3296_BPs_HILIC_pH9_SOP_G2_filter08.zip
  #Archive:  /srv/www/htdocs/Data2San/p1000/Metabolomics/Analysis/ProgenesisQI/cpanse_20171109_test/p1896_o3296_BPs_HILIC_pH9_SOP_G2_filter08.zip
  #Length   Method    Size  Cmpr    Date    Time   CRC-32   Name
  #--------  ------  ------- ---- ---------- ----- --------  ----
  #  2375961  Defl:N   295602  88% 2017-11-03 14:55 fe49e114  .././elaczko_20171109_ProgenesisQI_output_files_for_software_development/p1896_o3296_BPs_HILIC_pH9_SOP_G2_filter08_identifications_20171103.csv
  #1142877  Defl:N   460087  60% 2017-11-03 14:54 28057abf  .././elaczko_20171109_ProgenesisQI_output_files_for_software_development/p1896_o3296_BPs_HILIC_pH9_SOP_G2_filter08_measurements_20171103.csv
  #--------          -------  ---                            -------
  #  3518838           755689  79%                            2 files
  #cpanse@fgcz-r-021:~ > unzip -p /srv/www/htdocs/Data2San/p1000/Metabolomics/Analysis/ProgenesisQI/cpanse_20171109_test/p1896_o3296_BPs_HILIC_pH9_SOP_G2_filter08.zip *identifications*csv|wc -l
  #8968

  zip_filename <- eventReactive(input$load, {
    resources <- bf$resources()
    paste("/srv/www/htdocs/", input$relativepath, sep='')
  })

  get_identifications <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching identifications ...")
    on.exit(progress$close())

    cmd <- paste("unzip -p", zip_filename(), "*identifications*csv")

    if (!file.exists(zip_filename())){
      cmd <- paste("ssh fgcz-r-021 '", cmd, "'", sep='')
    }
    message(cmd)

    preprocessQIIdent(readIdentFile(pipe(cmd), sep=';'))
  })

  get_measurements <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching measurements ...")
    on.exit(progress$close())

    cmd <- paste("unzip -p", zip_filename(), "*measurements*csv")

    if (!file.exists(zip_filename())){
      cmd <- paste("ssh fgcz-r-021 '", cmd, "'", sep='')
    }

    message(cmd)

    # rv <- read.csv(pipe(cmd), sep=input$sep,  stringsAsFactors = FALSE, header = TRUE,  skip = 2)
    p389Devel::ProgenesisRead(pipe(cmd), sep=';')
  })

  output$measurements <- DT::renderDataTable({
    get_measurements()
  })

  output$identifications<- DT::renderDataTable({
    get_identifications()
  })



  output$distPlot <- renderPlot({
    plot(0,0, main='no data yet')
  })


  output$generateReportButton <- renderUI({
    if(nrow(get_identifications()) > 1 && values$notpressed){
      list(actionButton("generateReport", "Generate PDF Report" ))
      # br(),
      #downloadLink('downloadData', paste("Download QC data as '", values$qccsvfilename, "'.")))
    }else{
      NULL
    }
  })

  generateReport <- observeEvent(input$generateReport, {
    #here will processing happen!
    if(!is.null(get_identifications())){

      if (!values$notpressed){return}

      values$notpressed <- FALSE
      message("generating report ... ")

      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "generating report")
      on.exit(progress$close())

      progress$set(message = "render document", detail= "using rmarkdown", value = 0.1)

      QI_Data <- list(
        Identification = get_identifications(),
        Intensities = get_measurements(),
        resourceid = bf$resources()$resourceid[bf$resources()$relativepath == input$relativepath]
      )

      # TODO(cp): TESTING
      QI_Data_prepared <- p389Devel::prepareQIDATA(QI_Data)

      # for debugging
      (values$pdf <-  file.path(tempdir(), "made4-BGA1.pdf"))

      message("XXXXXXXXXXXXXXXXXXXXXXX",tempdir())

      # for download
      # QI_Data$Int_ID
      ###
      progress$set(message = "write quantitative data to bfabric ... ", detail= "using rmarkdown", value = 0.2)

      (fn <- tempfile(pattern = "file-", tmpdir = tempdir(), fileext = ".csv"))
      write.table(QI_Data_prepared$Int_ID,
                  file = fn,
                  sep='\t',
                  row.names = FALSE,
                  append = TRUE,
                  quote = FALSE, eol='\r\n')

      file_txt_content <- base64encode(readBin(fn, "raw", file.info(fn)[1, "size"]), 'csv')

      description <- ""
      if(length(unique(QI_Data_prepared$Annotation$Condition)) <= 1){
        description <- "No valid group information to conduct a report."
        progress$set(message = description, detail= "using rmarkdown", value = 0.1)
      }

      ###
      progress$set(message = "register workunit", detail= "in bfabric", value = 0.95)
      wuid <- bfabric_upload_file(login = bf$login(),
                                  webservicepassword = bf$webservicepassword(),
                                  projectid = bf$projectid(),
                                  file_content = file_txt_content,
                                  inputresource = values$inputresouceid,
                                  description = description,
                                  workunitname = input$experimentID,
                                  resourcename = paste("identified-quantitative-values-",
                                                       bf$workunitid(), ".csv", sep=''),
                                  status = 'available',
                                  applicationid = 227)
      values$wuid <- wuid

      if(length(unique(QI_Data_prepared$Annotation$Condition)) > 1){
        progress$set(message = "render PDF document", detail= "using rmarkdown", value = 0.5)
        markdownFile <- RMD_p389_BGA(workdir = tempdir())

        message("XXXXXXXXXXXXXXX :",markdownFile)
        message("File path ", file.path(tempdir(),markdownFile))
        message("Whats in tempdir ",dir(tempdir()))
        message("dim QI_Data_prepared$Int_ID", dim(QI_Data_prepared$Int_ID))
        message("", QI_Data_prepared$Annotation$Condition)

        bga_res_id <- p389Devel::runBGA(QI_Data_prepared$Int_ID, QI_Data_prepared$Annotation )
        bga_res_all <- p389Devel::runBGA(QI_Data_prepared$Int_ID_All, QI_Data_prepared$Annotation )


        rmarkdown::render(file.path(tempdir(),markdownFile),
                          output_file = values$pdf,
                          output_format = "pdf_document",
                          params=list(bga_res_id = bga_res_id,
                                      bga_res_all = bga_res_all,
                                      resourceid = QI_Data$resourceid),
                          envir = new.env())

        message(values$pdf)
        file_pdf_content <- base64encode(readBin(values$pdf, "raw",
                                                 file.info(values$pdf)[1, "size"]),
                                         "pdf")

        progress$set(message = "saving PDF to bfabric ...", detail= "using rmarkdown", value = 0.8)
        bfabricShiny:::saveResource(login = bf$login(),
                                    webservicepassword = bf$webservicepassword(),
                                    workunitid = wuid,
                                    content = file_pdf_content,
                                    name =  paste("made4-report_", bf$workunitid(), ".pdf", sep='')
        )

      }else{
        message("no valid group information.")
      }

      progress$set(message = paste("set workunit", wuid, "available."), detail= "status to 'available'", value = 0.95)

      rv <- bfabricShiny::save(bf$login(), bf$webservicepassword(), endpoint = 'workunit',
                         query =  list(status = 'available', id=wuid));

      message(paste("generate report DONE. the report was written to workunit ID", wuid, "in bfabric."))
    }
  })


  output$wuid <- renderUI({
    if (!is.null(values$wuid)){
      actionButton("download",
                   paste("bfabric download workunit", values$wuid),
                   onclick = paste("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=",
                                   values$wuid, "', '_blank')", sep=''))
    }
  })
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
})
