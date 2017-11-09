
library(bfabricShiny)
library(protViz)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  bf <- callModule(bfabric, "bfabric8",  applicationid = c(61), resoucepattern = 'fasta$')
  
  ### observes file upload
  get_tryptic_peptides <- eventReactive(input$load, {
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "fetching FASTA ...")
    on.exit(progress$close())
    
    resources <- bf$resources()
 
    FASTA.parameter <- list(
      FASTAfile = paste("/srv/www/htdocs/",input$relativepath, sep='')
    )
    cmd <- paste("cat", FASTA.parameter$FASTAfile, "| fcat | tryptic-digest", sep=" ")
    
    if (!file.exists(FASTA.parameter$FASTAfile)){
      cmd <- paste("ssh fgcz-r-021 '", cmd, "'", sep='')
    }
    
    message(cmd)
    
    S<-scan(pipe(cmd), what='character')
    S[nchar(S)>2 & nchar(S)<60 & grepl("^[WFLIMVYATPEDCSQGNRHK]+$",S)]
  })
  
  parent_ion_mass <- reactive({parentIonMass(get_tryptic_peptides())})
  hyd <- reactive({sapply(get_tryptic_peptides(), ssrc)})
  number_amino_acids <- reactive({sapply(get_tryptic_peptides(), nchar)})
  
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
