
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(DT)
library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  dataset.name <- reactive({
    
    infile <- input$CSVfile
    message("Reading CSV:", infile$datapath)
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    infile$datapath
    
  })
  
  raw.data <- reactive({
    
    infile <- input$CSVfile
    message("Reading CSV:", infile$datapath)
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  data.NA <- reactive({
    
    raw.data()[!is.na(raw.data()[,"padj"]),]
    
  })
  
  output$rawTable <- DT::renderDataTable({
    
    DT::datatable(
      data = raw.data(),
      options = list(
        lengthMenu = c(10, 50, 100),
        pageLength = 10
      )
    )
    
  })
  
  output$volcanoPlot <- renderPlot({
    
    if (input$symmetric){
      xlimits <- rep(max(abs(data.NA()[,"log2FoldChange"]))) * c(-1, 1)
    } else {
      xlimits <- range(data.NA()[,"log2FoldChange"])
    }
    
    
    ggplot(
      data = data.NA(),
      mapping = aes(
        x = log2FoldChange,
        y = -log10(pvalue))) +
      geom_point(
        colour = as.numeric(data.NA()[,"padj"] <= input$FDR) + 1,
        size = 2) +
      geom_text(
        data = data.NA()[data.NA()[,"padj"] <= input$FDR,],
        mapping = aes(label = SYMBOL),
        check_overlap = TRUE) +
      ggtitle(dataset.name()) +
      scale_x_continuous(limits = xlimits)

  })

})
