#R
# This is the server logic of a rawDiag Shiny web application version 2.

stopifnot(
  require(rawDiag),
  require(bfabricShiny),
  require(fgczqcms),
  require(shinydashboard),
  packageVersion('bfabricShiny') >= "0.13.4",
  packageVersion('rawDiag') >= "1.3")

rawrrrUI <- function(id){
  htmlOutput(NS(id, "plot"))
}

rawrrrServer <- function(id, rawfiles, msg){
  moduleServer(id,
               function(input, output, session) {
                 rawrrrServerReady <- reactiveVal(FALSE)
                 
                 observeEvent(rawfiles(), {
                   if(length(rawfiles()) > 0 && !is.null(rawfiles())){
                     
                     rawfiles() |> 
                       lapply(FUN = function(x){
                         fgczqcms::rawrrServer(id = paste0("rawrrr-", x),
                                               vals = reactiveValues(fn = x, mZ = fgczqcms:::.iRTmz()),
                                               msg = msg)
                       })
                     
                     rawrrrServerReady(TRUE)
                   }
                 })

                 
                 output$plot <- renderUI({
                   shiny::req(rawrrrServerReady())
                   
                   tryCatch({
                     rawfiles() |>
                       lapply(FUN = function(x) {
                         fgczqcms::rawrrUI(session$ns(paste0("rawrrr-", x)))
                       })}, error = function(e) {
                         message("Error in module rawrrr renderUI: ", e$message)
                         list()  # Return an empty list in case of error
                       }) -> plot_output_list
                   
                   # Convert the list to a tagList - this is necessary for the list of items
                   # to display properly.
                   do.call(tagList, plot_output_list)
                 })
               })
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("rawDiag - Brings Orbitrap Mass Spectrometry Data to Life; Fast and Colorful"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      tagList(
        bfabricShiny::bfabricInput("bfabric13"),
        htmlOutput("bfabricWorkunitId"),
        actionButton('resetBfabricWorkunitId', 'resetWuId'),
        actionButton('generatePDF', 'generatePDF')
      )
    ),
    mainPanel(
      bslib::navset_card_underline(
        bslib::nav_panel("rawDiag", htmlOutput("rawDiag")),
        bslib::nav_panel("Biognosys iRT XIC profiles",  htmlOutput("rawrrr")),
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  vals <- reactiveValues(
    generatePDF = 0,
    rawfile  = NULL,
    gp = NULL,
    pdfFileName = NULL,
    name = "",
    plot = NULL,
    bfabricWorkunitId = NULL)
  
  
  bf <- callModule(bfabric, "bfabric13",
                   applicationid = c(7, 160, 161, 162, 163, 176, 177, 197, 214,
                                     232, 248, 268, 269, 301, 309, 333, 337,
                                     338, 376, 379),
                   resoucepattern = "raw$|RAW$",
                   resourcemultiple = TRUE)
  
  .resetVals <- function(){
    vals$bfabricWorkunitId <- NULL
    vals$gp <- NULL
    vals$pdfFileName <- NULL
  }
  
  observeEvent(input$generatePDF, {
    vals$generatePDF <- vals$generatePDF + 1
  })
  
  
  observeEvent(input$bfabricWorkunitId,
               {vals$bfabricWorkunitId <- NULL})
  
  observeEvent(input$load,
               {
                 .resetVals()
                 
                 resources <- bf$resources()$relativepath
                 resourcesSelected <- resources[resources %in% input$relativepath] 
                 
                 c('/srv/www/htdocs/',
                   file.path(Sys.getenv('HOME'), 'Downloads/dump')) |>
                   Filter(f = dir.exists) -> rootdir
                 
                 stopifnot(length(rootdir) >= 1)
                 file.path(rootdir[1], resourcesSelected) -> resourcesSelected
                 
                 # mes8sage("resources: ", paste0(resources, collapse = ",\n\t"))
                 message("resourcesSelected: ", paste0(resourcesSelected, collapse = ", "))
                 
                 vals$rawfile <- resourcesSelected |>
                   Filter(f = file.exists)
                 
                 message("vals$rawfile: ", paste0( vals$rawfile, collapse = ",\n\t"))
               })
  
  msg <- reactiveVal("starting rawDiag shiny application ...")
  rawrrrServerReady <- reactiveVal(FALSE)
  
  observeEvent(msg(), {
    shiny::showNotification(msg(), type = "message", duration = 3)
    cat(msg())
  })
  
  ### ==== rawrr UI / Servers ====
  observeEvent(vals$rawfile, {
    if(length(vals$rawfile) > 0 && !is.null(vals$rawfile)){
      rawrrrServer("Orbitrap-rawrr", rawfiles = reactiveVal(vals$rawfile), msg = msg)
      rawrrrServerReady(TRUE)
    }
  })
  
  output$rawrrr <- renderUI({
    rawrrrUI(session$ns("Orbitrap-rawrr"))
  })
  
  ### ==== rawDiag UI / Servers ====
  observeEvent(vals$rawfile, {
    shiny::req(rawrrrServerReady())
    if(length(vals$rawfile) > 0 && !is.null(vals$rawfile)){
      rawDiag:::rawDiagServer("Orbitrap-rawDiag", vals)
    }
  })
  
  output$rawDiag <- renderUI({
    rawDiag::rawDiagUI(session$ns("Orbitrap-rawDiag"))
  })
  
  bfabricUpload <- observeEvent(input$generate, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "uploading to B-Fabric ...")
    on.exit(progress$close())
    
    resources <- bf$resources()
    rvUpload <- bfabricShiny::uploadResource(
      login = bf$login(),
      webservicepassword = bf$webservicepassword(),
      containerid = bf$containerid(),
      posturl = bf$posturl(),
      applicationid = 225,
      status = "AVAILABLE",
      description = sprintf("input files:\n%s", (input$relativepath |> format() |> paste(collapse='\n'))),
      inputresourceid = resources$resourceid[resources$relativepath == input$relativepath],
      workunitname = vals$plot,
      resourcename = sprintf("%s.pdf", "rawDiag"),
      file = vals$pdfFileName
    )
    print( rvUpload )
    vals$bfabricWorkunitId <- rvUpload$workunit$res[[1]]$id
    msg <- paste0("The current plot is available as workunit ", vals$bfabricWorkunitId, ".")
    message(msg)
    progress$set(message = msg)
  })
  
  observeEvent(vals$plot,{
    vals$bfabricWorkunitId <- NULL
  })
  
  observeEvent(input$resetBfabricWorkunitId, {
    vals$bfabricWorkunitId <- NULL
  })
  
  output$bfabricWorkunitId <- renderUI({
    if(isFALSE(is.null(vals$gp))){
      ## displays the button linking to the B-Fabric workunit iff 
      if (isFALSE(is.null(vals$bfabricWorkunitId))){
        
        wuUrl <- paste0("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=",
                        vals$bfabricWorkunitId, "', '_blank')")
        shiny::showNotification(paste0("Rendering actionButton to link workunit ", vals$bfabricWorkunitId, " in B-Fabric."), type = "message", duration = 5)
        tagList(
          shiny::helpText(paste0("The current plot '", vals$pdfFileName, "' is available as workunit in B-Fabric.")),
          actionButton(inputId = "B-FabricDownload",
                       label = paste("b-fabric download workunit", vals$bfabricWorkunitId),
                       onclick = wuUrl))
        
      }else if (isFALSE(is.null(vals$pdfFileName))){
        tagList(
          shiny::helpText(paste0("Upload the current plot '", vals$pdfFileName, "' to B-Fabric.")),
          actionButton('generate', 'Upload PDF\nto B-Fabric')
        )
      } else{
        HTML("Opps, something went wrong.\nPlease contact <cp@fgcz.ethz.ch>.")
      }
    }else{
      HTML("no pdf.")
    }
  })
})


# Run the application
shinyApp(ui = shinydashboard::dashboardPage(shinydashboard::dashboardHeader(disable = TRUE),
                                            shinydashboard::dashboardSidebar(disable = TRUE),
                                            body = shinydashboard::dashboardBody(ui)),
         server = server)
