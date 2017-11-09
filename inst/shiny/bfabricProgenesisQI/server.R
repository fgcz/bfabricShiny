
library(bfabricShiny)
library(protViz)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
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
    read.csv(pipe(cmd), sep=input$sep, 
             stringsAsFactors = FALSE, header = TRUE)
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
    
   read.csv(pipe(cmd), sep=input$sep, 
                  stringsAsFactors = FALSE, header = TRUE)
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
  
})
