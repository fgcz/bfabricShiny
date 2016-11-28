
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(jsonlite)
library(httr)

source('compose_queue.R')

##
shinyServer(function(input, output, session) {

  getInstrument <- reactive({list(VELOS_1='Xcalibur',
                       VELOS_2='Xcalibur',
                       G2HD_1='MassLynx',
                       QTRAP_1='Xcalibur',
                       TSQ_1='Xcalibur',
                       TSQ_2='Xcalibur',
                       QEXACTIVE_2='Xcalibur',
                       QEXACTIVE_3='Xcalibur',
                       FUSION_1='Xcalibur',
                       FUSION_2='Xcalibur',
                       QEXACTIVEHF_1='Xcalibur',
                       QEXACTIVEHF_2='Xcalibur')})
  
  
  getInstrumentSuffix <- reactive({list(VELOS_1='RAW',
                                  VELOS_2='RAW',
                                  G2HD_1='wiff',
                                  QTRAP_1='wiff',
                                  TSQ_1='RAW',
                                  TSQ_2='RAW',
                                  QEXACTIVE_2='raw',
                                  QEXACTIVE_3='raw',
                                  FUSION_1='raw',
                                  FUSION_2='raw',
                                  QEXACTIVEHF_1='raw',
                                  QEXACTIVEHF_2='raw')})
  
  output$area <- renderUI(({
    res.area <- c("Proteomics", "Metabolomics")
    selectInput('area', 'Area:', res.area, multiple = FALSE, selected = res.area[1])
  }))
  
  output$project <- renderUI({
    res.project <- c(NA, 1000, 1959, 2121)
    numericInput('project', 'Project:', value = res.project,  min = 1000, max = 2500, width=100)
  })
  
  output$howoften <- renderUI({
    res.howoften <- 1:5
    selectInput('howoften', 'How often?:', res.howoften, multiple = FALSE, selected = 2)
  })
  
  output$howmany <- renderUI({
    res.howmany <- 1:3
    selectInput('howmany', 'How many?:', res.howmany, multiple = FALSE, selected = 1)
  })
  
  
  output$instrument <- renderUI({
    res.instrument <- names(getInstrument())
    selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
  })
  
  
  getSample <- reactive({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/projectid/", input$project, sep='')))
  })
  
  getLogin <- reactive({
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/user/", input$project, sep='')))
    res$user
  })
  
  
  output$login <- renderUI({
    if ( input$project %in% 1000:2500){
      selectInput('login', 'Login:', as.character(getLogin()), multiple = FALSE)
    }else{
      selectInput('login', 'Login:', NULL)
    }
  })
  
  
  output$sample <- renderUI({
    if ( input$project %in% 1000:2500){
      res <- getSample()
      selectInput('sample', 'Sample:', paste(res$sample.id, res$sample.name, sep='-'), multiple = TRUE)
    }else{
      selectInput('sample', 'Sample:', NULL)
    }
  })
  

  getExtracts <- reactive({
    sample.id <- sapply(strsplit(input$sample, split='-'), function(x){x[1]})
    res <- do.call('rbind', 
                   lapply(sample.id, 
                          function(sampleid){
                            as.data.frame(fromJSON(paste("http://localhost:5000/sampleid/", sampleid, sep='')))
                            #rv$project.id <- input$project
                            #rv
                          }))
    
   res[, "project.id"]  <- input$project
   
   res[, "Condition"] <- "A"
   res
  })
  
  output$extract <- renderUI({

    if (input$sample == "" || length(input$sample) == 0 || is.null(input$sample)){
      selectInput('extract', 'Extract:', NULL)
    }   else{
      
      res <- getExtracts()
      selectInput('extract', 'Extract:', res$extract.name, multiple = TRUE)  
       
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { "fgcz_queue_test.csv" },
      # paste(unlist(strsplit(input$file, split="[.]"))[1], "csv", sep=".")  },
    content = function(file) {
      write.csv(cat("Bracket Type=4\r\n", file = file, append = FALSE))
      res <- getBfabricContent()
      res <- res[,1:4]
      write.table(res, sep=',', file = file, row.names = FALSE, append = TRUE, quote = FALSE, eol='\r\n')

    }
  )
  
  datasetID <- observeEvent(input$bfabricButton, {

	S <- getBfabricContent()
	if (nrow(S) > 0){
      		rv <- POST(paste("http://localhost:5000/add_dataset", input$project, sep='/'), body = toJSON(getBfabricContent()))

		 observe({
       		session$sendCustomMessage(type = 'testmessage', message = 'try to commit as dataset to bfabric.') 
		 })
	}else{
       		#session$sendCustomMessage(type = 'testmessage', message = 'not enough lines.') 
	}
      
      })
  
  #getContent <- reactive({
  #  res <- getExtracts()
  #  res[, "instrument"] <- input$instrument
  #  idx <- res$extract.name %in% input$extract
  #  generate_queue(data=res[idx, ], area = input$area,
  #                 foldername = "",
  #                 projectid=input$project,
  #                 username = input$login,
  #                 how.often = as.integer(input$howoften),
  #                 how.many = as.integer(input$howmany),
  #                 instrument = input$instrument)
  #})

  getBfabricContent <- reactive({

    res <- getExtracts()
   # if (nrow(res)>0){
    	res[, "instrument"] <- input$instrument
    	idx <- res$extract.name %in% input$extract

    	rv <- generate_queue(data=res[idx, ], area = input$area,
                   foldername = "",
                   projectid=input$project,
                   username = input$login,
                   how.often = as.integer(input$howoften),
                   how.many = as.integer(input$howmany),
                   instrument = input$instrument)

    	cbind(rv, extract.id=res[idx,'extract.id'])
    #}else{
    #        return NULL
    #}
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
  
    # call Christian Trachsel's code here!
   # print (input$extract)
    if (input$extract != "" && length(input$extract) > 0){
      getBfabricContent()
    }else{
      #as.data.frame(list(extract.name=NA, sampleid=NA, extract.id=NA))
      as.data.frame(list(output="no data - select extracts first."))
    }
    
    }))

})
