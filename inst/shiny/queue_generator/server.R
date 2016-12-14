
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(bfabricShiny)

##
shinyServer(function(input, output, session) {

 # TODOO(cp):
getHPLC <- reactive({list(VELOS_1='eksigent',
                       VELOS_2='eksigent',
                       G2HD_1='waters',
                       QTRAP_1='eksigent',
                       TSQ_1='eksigent',
                       TSQ_2='eksigent',
                       QEXACTIVE_2='easylc',
                       QEXACTIVE_3='easylc',
                       FUSION_1='easylc',
                       FUSION_2='easylc',
                       QEXACTIVEHF_1='waters',
                       QEXACTIVEHF_2='waters',
                       IMSTOF_1='eksigent')})
	
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
                       QEXACTIVEHF_2='Xcalibur',
                       IMSTOF_1='TOFWERK')})
  
  
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
                                  QEXACTIVEHF_2='raw',
                                  IMSTOF_1='h5')})
  
  output$area <- renderUI(({
    res.area <- c("Proteomics", "Metabolomics")
    selectInput('area', 'Area:', res.area, multiple = FALSE, selected = res.area[1])
  }))
  
  output$folder <- renderUI(({
      textInput('folder', 'Data Folder Name:', "enter your folder name here ", width = NULL, placeholder = NULL)
  }))
  
  output$qctype <- renderUI(({
    res.qctype <- c("Fetuin only", "Fetuin and clean", "Fetuin and clean every second ")
    selectInput('qctype', 'Type of sample QC:', res.qctype, multiple = FALSE, selected = res.qctype[1])
  }))
  
  output$testmethods <- renderUI(({
    res.testmethods <- 1:5
    selectInput('testmethods', 'Number of methods to test:', res.testmethods, multiple = FALSE, selected = res.testmethods[1])
  }))
  
  output$replicates <- renderUI(({
    res.replicates <- 1:5
    selectInput('replicates', 'Number of injections for each method:', res.replicates, multiple = FALSE, selected = res.replicates[1])
  }))
  
  output$project <- renderUI({
    res.project <- c(NA, 1000, 1959, 2121)
    numericInput('project', 'Project:', value = 1000,  min = 1000, max = 2500, width=100)
  })
  
  output$howoften <- renderUI({
    res.howoften <- 1:5
    selectInput('howoften', 'Insert QC sample every:', res.howoften, multiple = FALSE, selected = res.howoften[2])
  })
  
  output$howmany <- renderUI({
    res.howmany <- 1:3
    selectInput('howmany', 'Number of QC samples inserted:', res.howmany, multiple = FALSE, selected = 1)
  })
  
  output$instrument <- renderUI({
    res.instrument <- names(getInstrument())
    selectInput('instrument', 'Instrument:', res.instrument, multiple = FALSE, selected = res.instrument[1])
  })
  

  output$method <- renderUI(({
    selectInput('method', 'Queue Method:', c('default', 'random', 'blockrandom', 'testing'), multiple = FALSE, selected = 'default')
  }))
  
  output$condition <- renderUI(({
    res.condition <- c("TRUE", "FALSE")
    selectInput('condition', 'Insert condition into sample name:', res.condition, multiple = FALSE, selected = res.condition[1])
  }))
  
  getSample <- reactive({
    if (is.null(input$project)){
      return (NULL)
    }else{
      sampleURL <- paste("http://localhost:5000/projectid/", 
                         input$project, sep='')

     
      res <- as.data.frame(fromJSON(sampleURL))
      message(paste('got', nrow(res), 'samples.'))
      return (res)
    }
    })
  getLogin <- reactive({
    if (is.null(input$project)){
      return (NULL)
    }else{
    res <- as.data.frame(fromJSON(paste("http://localhost:5000/user/", 
                                        input$project, sep='')))
    message(paste('got', nrow(res), 'users.'))
    return (res$user)
    }
  })
  
  getExtracts <- reactive({
    if (is.null(input$project)){
      return (NULL)
    }else{
      extractURL <- paste("http://localhost:5000/extract/", input$project, sep='')
      res <- as.data.frame(fromJSON(extractURL ))
      message (paste('got', nrow(res), 'extracts from url', extractURL))
      res[, "project.id"] <- input$project
      if (!"extract.Condition" %in% names(res)){
        res[, "extract.Condition"] <- "A" #res$sampleid
      }
      res <- res[order(res$extract.id, decreasing = TRUE),]
    }
    return (res)
  })
  
  output$sample <- renderUI({
    res <- getSample()
    if (is.null(res)){
      selectInput('sample', 'Sample:', NULL)
    }else{
      res <- getSample()
      selectInput('sample', 'Sample:', paste(res$sample.id, res$sample.name, sep='-'), multiple = TRUE)
    }
  })
  output$login <- renderUI({
    res <- getLogin()
    if (!is.null(res)){
      selectInput('login', 'Login:', as.character(res), multiple = FALSE)
    }else{
      selectInput('login', 'Login:', NULL)
    }
  })
  output$extract <- renderUI({
    res <- getExtracts()
    if (!is.null(res)){
      selectInput('extract', 'Extract:', res$extract.name, multiple = TRUE)  
    }   else{
      selectInput('extract', 'Extract:', NULL)
     
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { "fgcz_queue_test.csv" },
      # paste(unlist(strsplit(input$file, split="[.]"))[1], "csv", sep=".")  },
    content = function(file) {
      write.csv(cat("Bracket Type=4\r\n", file = file, append = FALSE))
      res <- getBfabricContent()
      res <- res[,c(1,3:5)]
      write.table(res, sep=',', file = file, row.names = FALSE, append = TRUE, quote = FALSE, eol='\r\n')

    }
  )
  
  datasetID <- observeEvent(input$bfabricButton, {

	S <- getBfabricContent()
	if (nrow(S) > 0){
      		rv <- POST(paste("http://localhost:5000/add_resource", input$project, sep='/'), body = toJSON(getBfabricContent()))

		 observe({
       		session$sendCustomMessage(type = 'testmessage', message = 'try to commit as dataset to bfabric.') 
		 })
	}else{
       		#session$sendCustomMessage(type = 'testmessage', message = 'not enough lines.') 
	}
      
      })
  

  getBfabricContent <- reactive({

    res <- getExtracts()

    	res[, "instrument"] <- input$instrument

    	# TODO(cp): check of extract names are unique
    	idx <- res$extract.name %in% input$extract

    	rv <- generate_queue(x = res[idx, c("extract.name", "extract.id", "extract.Condition")],
		   method=input$method,
		   area = input$area,
                   foldername = "",
                   projectid=input$project,
                   username = input$login,
                   how.often = as.integer(input$howoften),
                   how.many = as.integer(input$howmany),
                   instrument = input$instrument)

    	#cbind(rv, extract.id = res[idx, 'extract.id'])
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    #print(paste("extract", input$extract, sep='='))

      if (input$extract != "" && length(input$extract) > 1){
      getBfabricContent()
    }else{
      #as.data.frame(list(extract.name=NA, sampleid=NA, extract.id=NA))
      as.data.frame(list(output="no data - select extracts first."))
    }
    
    }))

})
