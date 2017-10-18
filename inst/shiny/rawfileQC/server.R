#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(bfabricShiny)


# Define server logic required to draw a histogram
shinyServer( function(input, output, session) {
  
  bf <- callModule(bfabric, "bfabric8", 
                   applicationid = c(89, 90, 160, 161, 162, 163, 171, 176, 177, 197), 
                   resoucepattern = 'raw$')
  
  values <- reactiveValues(pdf = NULL,
                           inputresouceid = NULL,
                           wuid = NULL,
                           qccsvfilename = "qc.csv",
                           notpressed = TRUE)
  
  ### observes file upload
  rawfileInfo <- eventReactive(input$load, {
    
    resources <- bf$resources()
    
    values$inputresouceid <- resources$resourceid[resources$relativepath == input$relativepath][1]
    values$qccsvfilename <- paste("p", bf$project(),  "_R", resources$resourceid, '_', basename(input$relativepath), '.qc.csv', sep='')
 
    rawfileQC.parameter <- list(
      mono = 'mono',
      exe.ssh = '~cpanse/bin/fgcz_raw.exe',
      exe =  system.file("exec/fgcz_raw.exe", package = "bfabricShiny"),
      rawfile = paste("/srv/www/htdocs/",input$relativepath, sep='')
    )
    
    cmd <- ''
    if (file.exists(rawfileQC.parameter$rawfile)){
      cmd <- paste(rawfileQC.parameter$mono," ", rawfileQC.parameter$exe, 
                   " ", rawfileQC.parameter$rawfile,
                   " info | grep ':' | sed -e 's/:\ /;/'",
                   sep = '')
    }
    else{
      cmd <- paste("ssh fgcz-r-021 '", rawfileQC.parameter$mono," ", rawfileQC.parameter$exe.ssh, 
                 " ", rawfileQC.parameter$rawfile,
                 " info' | grep ':' | sed -e 's/:\ /;/'",
                 sep = '')
    }
    
    message(cmd)
    
    S <- read.csv(pipe(cmd), sep=';', 
                  stringsAsFactors = FALSE, header = FALSE,
                  col.names = c('attribute', 'value'))
    
    return (S)
  })
  
  
  rawfileQC <- reactive({
    
    resources <- bf$resources()
    
    values$inputresouceid = resources$resourceid[resources$relativepath == input$relativepath][1]
    
    rawfileQC.parameter <- list(
      mono = 'mono',
      exe =  system.file("exec/fgcz_raw.exe", package = "bfabricShiny"),
      exe.ssh = "~cpanse/bin/fgcz_raw.exe",
      rawfile = paste("/srv/www/htdocs/",input$relativepath, sep='')
    )
    
    cmd <- ''
    if (file.exists(rawfileQC.parameter$rawfile)){
      cmd <- paste(rawfileQC.parameter$mono," ", rawfileQC.parameter$exe, 
                   " ", rawfileQC.parameter$rawfile,
                   " qc",
                   sep = '')
    }
    else{
      cmd <- paste("ssh fgcz-r-021 '", rawfileQC.parameter$mono," ", rawfileQC.parameter$exe.ssh, 
                   " ", rawfileQC.parameter$rawfile,
                   " qc'",
                   sep = '')
    }
    
    message(cmd)
    
    S <- read.csv(pipe(cmd), sep=';', 
                  stringsAsFactors = FALSE, header = TRUE)
    
    message(paste("dim of data frame =", dim(S), sep=''))
    return (S)
  })
  
  
  output$fileInformation <- renderTable({
    rawfileInfo()
  })
  
  output$downloadData <- downloadHandler(
       filename = function() {
         values$qccsvfilename
       },
       content = function(con) {
         write.csv(rawfileQC(), con, row.names = FALSE)
       }
     )
  
  output$generateReportButton <- renderUI({
    if(nrow(rawfileInfo()) > 1 && values$notpressed){
      list(actionButton("generateReport", "Generate PDF Report" ),
           br(),
      downloadLink('downloadData', paste("Download QC data as '", values$qccsvfilename, "'.")))
    }else{
      NULL
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
  
  generateReport <- observeEvent(input$generateReport, {
    #here will processing happen!
    if(is.null(rawfileInfo())){
      message("DUMM")
    }
    else{
      if (!values$notpressed){return}
      values$notpressed <- FALSE
      message("generating report ... ")
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "generating report")
      on.exit(progress$close())
        
        rawfileQC.parameter <<- list(
          mono = 'mono',
          exe =  system.file("exec/fgcz_raw.exe", package = "bfabricShiny"),
          rawfile = paste("/srv/www/htdocs/", input$relativepath, sep=''),
          pdf = tempfile(fileext = ".pdf"),
          progress = progress,
          resourceid = bf$resources()$resourceid[bf$resources()$relativepath == input$relativepath]
        )
        
        rawfileQC.parameter$progress$set(message = "render document", detail= "using rmarkdown", value = 0.9)
        render(input = paste(path.package("bfabricShiny"),
                             "/report/rawfileQC.Rmd", sep='/'),
               output_file = rawfileQC.parameter$pdf,
               intermediates_dir = tempdir(),
               knit_root_dir = tempdir()) 
     
      message(tempdir())
      values$pdf <- rawfileQC.parameter$pdf
      
      file_pdf_content <- base64encode(readBin(values$pdf, "raw", 
                                               file.info(values$pdf)[1, "size"]), 
                                       "pdf")  

      rawfileQC.parameter$progress$set(message = "register workunit", detail= "in bfabric", value = 0.95)
      wuid <- bfabric_upload_file(login = bf$login(),
                                  webservicepassword = bf$webservicepassword(),
                                  projectid = bf$projectid(),
                                  file_content = file_pdf_content, 
                                  inputresource = values$inputresouceid,
                                  workunitname = input$experimentID,
                                  resourcename = paste("Thermo Fisher raw file QC of ",
                                                       bf$workunitid(), ".pdf", sep=''),
                                  status = 'available',
                                  applicationid = 225)
      
      values$wuid <- wuid
      
      rawfileQC.parameter$progress$set(message = paste("set workunit", wuid), detail= "status to 'available'", value = 0.95)
      
      rv <- bfabric_save(bf$login(), bf$webservicepassword(), endpoint = 'workunit', 
                         query =  list(status = 'available', id=wuid));
      
      message(paste("generate report DONE. the report was written to workunit ID", wuid, "in bfabric."))
      message(rawfileQC.parameter$pdf)
    }
  })
    
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
})
