
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# https://github.com/cpanse/bfabricShiny

shinyUI(
  fluidPage(
    
    titlePanel(paste("MS Queue Generator", packageVersion('bfabricShiny'), sep = ' - version ')),
    #copied from mockup
    fluidRow(
      column(12,
             #tags$h1(paste("FGCZ", "Queue Generator Version 2.0", sep = " ")),
             tags$hr(),
             
             fluidRow(
               column(3, 
                      tags$h3("Available samples for your queue:"),
                      tags$h5("use \"shift + click\" or \"click + drag\"  for selecting a block of consecutive samples"),
                      tags$h5("use \"control + click\" to select multiple samples"),
                      tags$h5("use \"control + click + drag\" to select multiple blocks of consecutive samples"),
                      htmlOutput("sample")
                      #actionButton("go", "Generate Queue", icon = icon("cogs"),
                      #style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
               # Main panel: options ----     
               column(9,
                      #content ----
                      tags$h3("Select System:"),
                      fluidRow(
                        column(4, htmlOutput("instrumentControlSoftware")),
                        column(4, htmlOutput("lcConfiguration"))
                      ),
                      tags$h3("Select LC vendor:"),
                      tags$h3("Select options for your queue:"),
                      radioButtons(
                        "containerType",
                        "Type of queue:",
                        choices = list("single container (default)" = "project",  "multi order (for PAG use only)" = "order"),
                        selected = 'project',
                        inline = TRUE,
                        width = NULL,
                        choiceNames = NULL,
                        choiceValues = NULL
                      ),
                      fluidRow(
                        column(4, htmlOutput("project")),
                        column(4, htmlOutput("login")), #tags$h4("user name:")),
                        column(4, htmlOutput("folder"))#htmlOutput("project"))  #tags$h4("folder name:"))
                      ),
                      fluidRow(
                        column(4, htmlOutput("instrument")), #tags$h4("instrument:")),
                        column(4, htmlOutput("area")), #tags$h4("area:")),
                        column(4, htmlOutput("method"))  #tags$h4("method:"))
                        
                      ),
                      fluidRow(
                        column(4, htmlOutput("startposition")), #tags$h4("instrument:")),
                        column(4,
                               conditionalPanel(condition = c("input.method == 'PRM' || input.method == 'testing'"),
                                                conditionalPanel(condition = "input.method == 'PRM'",             
                                                                 htmlOutput("targets")),
                                                conditionalPanel(condition = "input.method == 'testing'",             
                                                                 htmlOutput("testmethods"))
                               )
                        ),
                        column(4, 
                               conditionalPanel(condition = "input.method == 'testing'", 
                                                htmlOutput("replicates"))
                        )
                      ),
                      #Main panel: QC inserts ----                      
                      tags$hr(),
                      tags$h3("Define QC and clean inserts:"),
                      fluidRow(
                        column(3, 
                               checkboxInput("autoQC01", "insert autoQC01:", TRUE),
                               conditionalPanel(condition = "input.autoQC01 == true", htmlOutput("QC01m")),
                               conditionalPanel(condition = "input.autoQC01 == true", htmlOutput("QC01o"))
                        ),
                        column(3,
                               checkboxInput("autoQC02", "insert autoQC02:"),
                               conditionalPanel(condition = "input.autoQC02 == true", htmlOutput("QC02m")),
                               conditionalPanel(condition = "input.autoQC02 == true", htmlOutput("QC02o"))
                        ),
                        column(3,
                               checkboxInput("autoQC4L", "insert autoQC4L:"),
                               conditionalPanel(condition = "input.autoQC4L == true", htmlOutput("QC4Lm")),
                               conditionalPanel(condition = "input.autoQC4L == true", htmlOutput("QC4Lo"))
                        ),
                        column(3,
                               checkboxInput("clean", "insert clean:"),
                               conditionalPanel(condition = "input.clean == true", htmlOutput("cleanm")),
                               conditionalPanel(condition = "input.clean == true", htmlOutput("cleano"))
                        )
                      ),   
                      #Main panel: additional options ---- 
                      tags$hr(),
                      tags$h3("Queue start and end options:"),
                      
                      fluidRow(
                        column(4,
                               htmlOutput("start1")
                        ),
                        column(4,
                               htmlOutput("start2")
                        ),
                        column(4,
                               htmlOutput("start3")
                        )
                      ),
                      
                      fluidRow(
                        column(4,
                               htmlOutput("end1")
                        ),
                        column(4,
                               htmlOutput("end2")
                        ),
                        column(4,
                               htmlOutput("end3")
                        )
                      )
               )
             ),
             
             #copied from mockup
             
             
             
             # Show a plot of the generated distribution
             
             fluidRow(
               #textOutput("result2"), 
               #textOutput("result3")
               DT::dataTableOutput("table")
             ),
             #fluidRow(
             column(4, 
                    wellPanel(
                      htmlOutput("download")
                    )
             )
      ) 
    )
  )
)