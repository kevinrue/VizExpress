
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage(

  title = "VizExpress",
  
  tabPanel(
    
    title = ("Raw data table"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "CSVfile",
          "Input CSV:",
          accept=c('text/csv')),
        width = 3
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("rawTable")
        #plotOutput("volcanoPlot")
        )
      
      )
    ),
  
  tabPanel(
    
    title = "Volcano plot",
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        
        sliderInput(
          "FDR",
          "FDR:",
          min = 0,
          max = 1,
          value = 0.05,
          step = 0.01),
        
        checkboxInput(
          "symmetric",
          "Symmetric X axis?",
          value = TRUE
        )
        
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("volcanoPlot"),
          #plotOutput("volcanoPlot")
          width = 6
          )
        )
      )
  
  ))
