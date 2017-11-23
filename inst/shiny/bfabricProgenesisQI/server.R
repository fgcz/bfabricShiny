
library(bfabricShiny)
library(p389Devel)

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
   # read.csv(pipe(cmd), 
    #         sep=input$sep, 
    #         stringsAsFactors = FALSE,
    #         header = TRUE, 
    #         skip = 2)
    
    p389Devel::preprocessQIIdent(p389Devel::readIdentFile(pipe(cmd), sep=';'))
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

  output$measurements<- DT::renderDataTable({
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
      if(is.null(get_identifications())){
        message("DUMM")
      }
      else{
        if (!values$notpressed){return}
        values$notpressed <- FALSE
        message("generating report ... ")
        
        progress <- shiny::Progress$new(session = session, min = 0, max = 1)
        progress$set(message = "generating report")
        on.exit(progress$close())
        
        progress$set(message = "render document", detail= "using rmarkdown", value = 0.9)
        
        QI_Data <<- list(
          Identification = get_identifications(),
          Intensities = get_measurements(),
          Annotation = ProgenesisBuildAnnotation(get_measurements())$annotation)
        
        # for debugging
        # save(QI_Data, file="/tmp/QI_Data.RData")
        values$pdf <-  file.path(tempdir(), "BGA1.pdf")
        
        markdownFile <- RMD_p389_BGA(workdir = tempdir())
        
        message(tempdir())
        
        rmarkdown::render(file.path(tempdir(), "BGAAnalysis.Rmd"), 
                          output_file = values$pdf, 
                          output_format = "pdf_document")
        
        message(values$pdf)
        file_pdf_content <- base64encode(readBin(values$pdf, "raw", 
                                                 file.info(values$pdf)[1, "size"]), 
                                         "pdf")  
        
        progress$set(message = "register workunit", detail= "in bfabric", value = 0.95)
        wuid <- bfabric_upload_file(login = bf$login(),
                                    webservicepassword = bf$webservicepassword(),
                                    projectid = bf$projectid(),
                                    file_content = file_pdf_content, 
                                    inputresource = values$inputresouceid,
                                    workunitname = input$experimentID,
                                    resourcename = paste("made4 report",
                                                         bf$workunitid(), ".pdf", sep=''),
                                    status = 'available',
                                    applicationid = 227)
        
        values$wuid <- wuid
        
        progress$set(message = paste("set workunit", wuid), detail= "status to 'available'", value = 0.95)
        
        rv <- bfabric_save(bf$login(), bf$webservicepassword(), endpoint = 'workunit', 
                           query =  list(status = 'available', id=wuid));
        
        message(paste("generate report DONE. the report was written to workunit ID", wuid, "in bfabric."))
      }
    })
    output$wuid <- renderUI({
      if (!is.null(values$wuid)){
        actionButton("download",
                     paste("bfabric download workunit", values$wuid),
                     onclick = paste("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?workunitId=", 
                                     values$wuid, "', '_blank')", sep=''))
      }
    })
    output$sessionInfo <- renderPrint({
      capture.output(sessionInfo())
    })
})
