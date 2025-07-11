#R
# This is the server logic of a rawDiag Shiny web application version 2.

stopifnot(
  require(rawDiag),
  require(bfabricShiny),
  packageVersion('bfabricShiny') >= "0.13.4",
  packageVersion('rawDiag') >= "1.3"
)

shinyServer(function(input, output, session) {

  vals <- reactiveValues(
    generatePDF = 0,
    rawfile  = NA,
    gp = NULL,
    pdfFileName = NULL,
    name = "",
    plot = NULL,
    bfabricWorkunitId = NULL)
  
  rawDiag:::rawDiagServer("OrbitrapFun02", vals)
  
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

   iRTmz <- reactive({c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384, 683.8282,
                       683.8541, 699.3388, 726.8361, 776.9301) -> mZ
    names(mZ) <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
                   "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
                   "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
                   "LFLQFGAQGSPFLK")
    return(mZ)
  })

  
  
  rawrrServerModules <- reactive({
    shiny::req(vals$rawfile)
    #BiocParallel::bplapply(FUN = function(x){
    vals$rawfile |> 
    lapply(FUN = function(x){
      fgczqcms::rawrrServer(id = x,
         vals = reactiveValues(fn = x, mZ = iRTmz()))
    })
  })

  output$rawrr <- renderUI({
    shiny::req(rawrrServerModules())

    vals$rawfile |> 
    lapply(FUN = function(x){
     tagList(
       h2(basename(x)),
       fgczqcms::rawrrUI(x),
       hr()
       )
     }
     )
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

