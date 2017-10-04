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
  
  values <- reactiveValues(pdf = NULL, inputresouceid=NULL)
  
  ### observes file upload
  rawfileInfo <- eventReactive(input$load, {
    
    resources <- bf$resources()
    
    values$inputresouceid = resources$resourceid[resources$relativepath == input$relativepath][1]
    #print (v_upload_file$inputresouceid)
    
    rawfileQC.parameter <- list(
      mono = 'mono',
      exe = '~cpanse/bin/fgcz_raw.exe',
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
      cmd <- paste("ssh fgcz-r-021 '", rawfileQC.parameter$mono," ", rawfileQC.parameter$exe, 
                 " ", rawfileQC.parameter$rawfile,
                 " info' | grep ':' | sed -e 's/:\ /;/'",
                 sep = '')
    }
    
    message(cmd)
    
    S <- read.csv(pipe(cmd), sep=';', 
                  stringsAsFactors = FALSE, header = FALSE,
                  col.names = c('attribute', 'value'))
    
    message(nrow(S))
    return (S)
  })
  
  output$fileInformation <- renderTable({
    rawfileInfo()
  })
  
  output$generateReportButton <- renderUI({
    if(nrow(rawfileInfo()) > 1){
      
      actionButton("generateReport", "Generate Report" )
    }else{
      NULL
    }
  })
  
  generateReport <- observeEvent(input$generateReport, {
    #here will processing happen!
    if(is.null(rawfileInfo())){
      message("DUMM")
    }
    else{
      message("generating report ... ")
      
      
        
        rawfileQC.parameter <<- list(
          mono = 'mono',
          exe = '~cpanse/bin/fgcz_raw.exe',
          rawfile = paste("/srv/www/htdocs/", input$relativepath, sep=''),
          pdf = tempfile(fileext = ".pdf")
        )
        
        render(input = paste(path.package("bfabricShiny"), "/report/rawfileQC.Rmd", sep='/'),
               output_file = rawfileQC.parameter$pdf) 
     
      values$pdf <- rawfileQC.parameter$pdf
      
      file_pdf_content <- base64encode(readBin(values$pdf, "raw", 
                                               file.info(values$pdf)[1, "size"]), 
                                       "pdf")  

      
      wuid <- bfabric_upload_file(login = bf$login(),
                                  webservicepassword = bf$webservicepassword(),
                                  projectid = bf$projectid(),
                                  file_content = file_pdf_content, 
                                  inputresource = values$inputresouceid,
                                  workunitname = input$experimentID,
                                  resourcename = paste("Thermo Fisher raw file QC of ", bf$workunitid(), ".pdf", sep=''),
                                  applicationid = 225)
      
      
      message(paste("generate report DONE. the report was written to workunit ID", wuid, "in bfabric."))
      message(rawfileQC.parameter$pdf)
    }
  })
    
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
})
