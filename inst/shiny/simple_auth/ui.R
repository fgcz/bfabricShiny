#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(bfabricShiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("FGCZ bfabric Web Service Password Test"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput('login', 'bfabric Login', "cpanse"),
       passwordInput('webservicepassword', 'Web Service Password', "$2a$10$We8McOYkCp7iCFzaTCgDoepBe2KkrzkiLKvh0o.v9u8tIQCYmD.D6"),
       htmlOutput("project"),
       htmlOutput("resources"),
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
