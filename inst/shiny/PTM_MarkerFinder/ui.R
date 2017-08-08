#
# This is a Shiny web application. 
#
#    https://cran.r-project.org/package=protViz
#
#
# $HeadURL: svn+ssh://cp@fgcz-148.uzh.ch/home/cp/__SUBVERSION_REPOSITORY__/__projects/2016/20160704_pptm_shiny/ui.R $
# $Id: ui.R 915 2017-04-11 12:36:53Z cp $
# $Date: 2017-04-11 14:36:53 +0200 (Tue, 11 Apr 2017) $
#
# runApp('inst/shiny/PTM_MarkerFinder', port=8282)
 
library(bfabricShiny)
library(DT)

# source("ptm_marker_finder.R")

  
shinyUI(fluidPage(
     # Application title
     titlePanel(paste("PTM Marker Finder -- https://CRAN.R-project.org/package=protViz Version:", packageVersion('protViz'))),
     
     
     sidebarLayout(
        sidebarPanel(
	   htmlOutput("mZmarkerIons")),
        
        # Show a plot of the generated distribution
          mainPanel(
            tabsetPanel(
              tabPanel("bfabric", bfabricInput("bfabric8")),
              tabPanel("table - long format", DT::dataTableOutput("findMzTableLong")),
              tabPanel("table - wide format", DT::dataTableOutput("findMzTableWide")),
              tabPanel("sessionInfo", verbatimTextOutput("sessionInfo")),
              tabPanel("lc-ms map", 
                       tagList(
                        selectInput('charges', 'charges:',  choices = 1:6, selected = 2, multiple = TRUE),
                        splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput("findMzPlotBrush", height = 500, click = "plot_click"),
                                    plotOutput("linkedlcmsmap", height = 500, 
                                               brush = "plot_brush",
                                               click = "plot_click",
                                               hover = hoverOpts(
                                                 id = "plot_hover",
                                                 delayType = "throttle",
                                                 delay = 500
                                               )
                                    )),
                        plotOutput("peakplot_click", height = 300, click = "plot_click"),
                        tableOutput("plot_hoverinfo"),
                        verbatimTextOutput("info")
                        #plotOutput("findMzPlot", height = 700)
                        )),
              tabPanel("download", htmlOutput("download"))
        ))
     ), 
	  hr(),
	  p('citation: Nanni, P., Panse, C., Gehrig, P., Mueller, S., Grossmann, J. and Schlapbach, R. (2013), PTM MarkerFinder, a software tool to detect and validate spectra from peptides carrying post-translational modifications. Proteomics, 13: 2251–2255. ', a('DOI: 10.1002/pmic.201300036', href='http://onlinelibrary.wiley.com/doi/10.1002/pmic.201300036/abstract;jsessionid=717FB314BBA9A6BD2E722BD257D3D2A9.f01t04'))
  ))

