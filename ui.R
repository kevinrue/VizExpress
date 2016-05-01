
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("uiFunctions.R")

shinyUI(navbarPage(

  title = "VizExpress",
  
  tabPanel(
    title = "Welcome!",
    fluidRow(
      column(
        3,
        wellPanel(
          radioButtons(
            "type_input",
            label = h3("Input type"),
            choices = list(
              "Differential expression" = 1,
              "Expression" = 2), 
            selected = 1),
          hr(),
          checkboxInput(
            "multiple_input",
            label = "Multiple files?",
            value = FALSE
            )
          )
        )
      )
    ),
  
  tabPanel(
    title = "Input",
    fluidRow(
      column(
        3,
        wellPanel(
          conditionalPanel(
            "input.multiple_input == false",
            singleFileInput("input_file")
          ),
          conditionalPanel(
            "input.multiple_input == true",
            multipleFilesInput("input_files")
          )
        )
      ),
      column(
        h3("Data set"),
        textInput(
          "dataset_name",
          label = "Name",
          value = randomDatasetName,
          placeholder = "Dataset name"
        ),
        width = 9
      )
    ),
    fluidRow(
      column(
        dataTableOutput("rawTable"),
        width = 12
      )
    )
  ),
  
  tabPanel(
    title = "Plots",
    sidebarLayout(
      sidebarPanel(
        
        checkboxInput(
          "symmetric",
          "Symmetric X axis?",
          value = TRUE
        ),
        
        selectInput(
          "volcano_logFC",
          "Column for logFC",
          choices = c()),
        
        selectInput(
          "volcano_pval",
          "Column for P-value",
          choices = c()),
        
        selectInput(
          "volcano_padj",
          "Column for adjusted P-value",
          choices = c()),
        
        sliderInput(
          "FDR",
          "Adjusted P-value:",
          min = 0,
          max = 1,
          value = 0.05,
          step = 0.01),
        
        radioButtons(
          "FC_input",
          label = "Fold-change cutoff",
          choices = list(
            "Fold-change" = 1,
            "Log fold-change" = 2), 
          selected = 2),
        
        conditionalPanel(
          "input.FC_input == 1",
          sliderInput(
            "fold_change",
            "Fold-change:",
            min = 1,
            max = 1024,
            value = 1.5,
            step = 0.5)
        ),
        
        conditionalPanel(
          "input.FC_input == 2",
          numericInput(
            "log_fold_change",
            "Log2 fold-change:",
            min = 1,
            max = 10,
            value = 0.5849625)
        ),
        
        selectInput(
          "volcano_symbol",
          "Column for gene symbol",
          choices = c()),
        
        selectInput(
          "ma_mean",
          "Column for mean signal",
          choices = c()),
        
        checkboxInput(
          "logMean",
          expression("Apply log2 to mean?"),
          value = TRUE
        )
        
        ),
      
      mainPanel(
        plotOutput("volcanoPlot"),
        tags$hr(),
        plotOutput("QQplot"),
        tags$hr(),
        plotOutput("MAplot"),
        width = 5
        )
      )
    )
  
  ))
