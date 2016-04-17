
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(navbarPage(

  title = "VizExpress",
  
  tabPanel(
    
    title = ("Raw data table"),
    
    fluidRow(
      
      column(
          fileInput(
            "CSVfile",
            "Input CSV:",
            accept=c('text/csv')),
          width = 3
        ),
      
      column(
        h3("Data set:"),
        textOutput("datasetName"),
        width = 9
        )
        
      ),

    fluidRow(
      
      column(
        mainPanel(
          dataTableOutput("rawTable")
          ),
        width = 12
        )
      
      )
    
    ),
  
  tabPanel(
    
    title = "Volcano / QQ",

    sidebarLayout(
      sidebarPanel(
        
        checkboxInput(
          "symmetric",
          "Symmetric X axis?",
          value = TRUE
        ),
        
        selectInput(
          "volcano.logFC",
          "Column for logFC",
          choices = c()),
        
        selectInput(
          "volcano.pval",
          "Column for P-value",
          choices = c()),
        
        selectInput(
          "volcano.padj",
          "Column for adjusted P-value",
          choices = c()),
        
        sliderInput(
          "FDR",
          "Adjusted P-value:",
          min = 0,
          max = 1,
          value = 0.05,
          step = 0.01),
        
        selectInput(
          "volcano.symbol",
          "Column for gene symbol",
          choices = c())
        
        ),
        
        mainPanel(
          plotOutput("volcanoPlot"),
          tags$hr(),
          plotOutput("QQplot"),
          width = 6
          )
        )
      )
  
  ))
