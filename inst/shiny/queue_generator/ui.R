
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# https://github.com/cpanse/bfabricShiny

shinyUI(fluidPage(

  fluidPage(
  # Application title
  titlePanel(paste("FGCZ MS Queue Generator", packageVersion('bfabricShiny'), sep = ' - version ')),
  fluidRow(
    column(2, htmlOutput("project")),
    
    column(3, htmlOutput("sample")),
    column(7, htmlOutput("extract"))
    ),
  fluidRow(
    column(3, htmlOutput("area")),
    column(3, htmlOutput("instrument")),
    column(3, htmlOutput("folder")),
    column(3, htmlOutput("login"))
    ),
 
  fluidRow(
    column(3, htmlOutput("method")),
    column(3, htmlOutput("qctype")),
    column(3, htmlOutput("howoften")),
    column(3, htmlOutput("howmany"))
  ),

  fluidRow(
    column(3, htmlOutput("showcondition"))
      ),
  
  fluidRow(
    column(12, h3("The selections below are only used in combination with the queue method testing. Ignore them if you use another queue generation method")) #code("the below selections are only used in combination with method "testing""))
  ),
  
  fluidRow(
    column(3, htmlOutput("testmethods")),
    column(3, htmlOutput("replicates"))
  ),
    # Show a plot of the generated distribution
    
      fluidRow(
        DT::dataTableOutput("table")
      ),
 #fluidRow(
 column(4, wellPanel(
 	downloadButton('downloadData', 'Download'),
    	#actionButton("bfabricButton", "Add table as csv resource to bfabric."),
    	singleton( tags$head(tags$script(src = "message-handler.js")))
    )
  )
) 
)
)
