
library(bfabricShiny)
library(protViz)
library(PKI)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  callModule(bfabric, "bfabric8",  applicationid = c(61),
             resoucepattern = 'fasta$') -> bf
  
  output$test <- renderUI({
    selectInput('test', "test", bf$posturl(), multiple = FALSE)
  })
  
  ### observes file upload
  get_tryptic_peptides <- eventReactive(input$load, {
    #progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    #progress$set(message = "Processing FASTA ...")
    #on.exit(progress$close())
    
    resources <- bf$resources()
 
    FASTA.parameter <- list(
      FASTAfile = paste("/srv/www/htdocs/", input$relativepath, sep='/')
    )

    # TODO(cp,leo): hotfix on 2025-07-04; make correct automounter
    gsub(x = FASTA.parameter$FASTAfile, replacement = "/FASTA/", pattern = "/fasta/") -> FASTA.parameter$FASTAfile
    
    cmd <- paste("cat", FASTA.parameter$FASTAfile, "| fcat | tryptic-digest", sep=" ")
    
    if (!file.exists(FASTA.parameter$FASTAfile)){
      shiny::showNotification(paste0("Can not access ", FASTA.parameter$FASTAfile, "trying ssh ..."),
                              duration = 10, type = 'message')
      
      cmd <- paste("ssh fgcz-r-035 '", cmd, "'", sep='')
      
      shiny::showNotification(paste0("cmd = ", cmd))
    }
    
    withProgress(message = 'Reading and digesting FASTA', value = 0.1, {
      S <- scan(pipe(cmd), what='character')
    })
    
    withProgress(message = 'Filtering FASTA', value = 0.1, {
      # sanity check take only valid AA
      S[nchar(S) > 2 & nchar(S) < 60 & grepl("^[WFLIMVYATPEDCSQGNRHK]+$", S)]
    })
  })
  
  parent_ion_mass <- reactive({
    withProgress(message = 'Determine parent ion mass ...', {
      parentIonMass(get_tryptic_peptides())
    })
  })
  
  hyd <- reactive({
    percentage <- 0
    withProgress(message = 'Performing SSRC ...', {
      sapply(get_tryptic_peptides(), function(x){
        length(get_tryptic_peptides()) -> n
        percentage <<- percentage + 1 / n * 100
        incProgress(1/n, detail = paste0("Progress: ", round(percentage, 1)))
        ssrc(x)
        })
    })
  })
  
  number_amino_acids <- reactive({
    withProgress(message = 'Counting amino acids ...', {
      sapply(get_tryptic_peptides(), nchar)
    })
  })
  
  output$distPlot <- renderPlot({
    
    par(mfrow = c(3, 1))
    
    hist(parent_ion_mass(),
         input$bins, col = 'darkgray', border = 'white', main = input$relativepath)
    
    hist(hyd(),
         input$bins, col = 'darkgray', border = 'white', main = input$relativepath)
    
    hist(number_amino_acids(),
         input$bins, col = 'darkgray', border = 'white', main = input$relativepath)
  })
  
})
