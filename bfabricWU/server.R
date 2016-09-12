# see also RJSON on 
# http://fgcz-intranet.uzh.ch/tiki-index.php?page=cp

library(shiny)
library(jsonlite)


shinyServer(function(input, output, session) {

  v <- reactiveValues(data = NULL, filename = NULL, msg='')
 
  observeEvent(input$loadButton, {
        root <- "/srv/www/htdocs"
	RDataFile <- paste(root, input$file, sep="/")

	load(RDataFile)
	dump.grepl <- grepl ("F[0-9]+$", dump <- ls()) 
	tt <- dump[dump.grepl]

        v$data <- unlist(lapply(get(tt), function(msm){msm$rtinseconds}))
        v$filename <- RDataFile

  })
  
  output$bfabric_resources <- renderUI({
    wuid <- input$wuId
    url <- paste("http://localhost:5000/bfabric/api/workunitid", wuid, sep='/')
    res <- fromJSON(url)
    selectInput('file', 'file', res$resources)
  })

  
  output$distPlot <- renderPlot({

	if (length((v$data)) < 1){
		plot(0, 0, type='n', axes=FALSE, xlab='', ylab='')
		text(0,0,"NO DATA", cex=4)
		return()
	}

    hist(v$data, breaks = input$bins, col = 'darkgray', border = 'white', 
    	main=v$filename)

  })

})
