
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(ggplot2)

shinyServer(function(input, output, clientData, session) {
  
  observe({
    
    updateSelectInput(
      session, "volcano.logFC",
      choices = data.numCols(),
      selected = FC.default()
    )
    
    updateSelectInput(
      session, "volcano.pval",
      choices = data.numCols(),
      selected = pval.default()
    )
    
    updateSelectInput(
      session, "volcano.padj",
      choices = data.numCols(),
      selected = padj.default()
    )
  
  })
    
  dataset.name <- reactive({
    
    infile <- input$CSVfile
    message("Reading CSV:", infile$datapath)
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(sample.datasetName)
    } else{
      return(
        gsub(pattern = '\\.csv$', replacement = '', x = infile$name))
    }
    
  })
  
  output$datasetName <- renderText({
    
    dataset.name()
    
  })
  
  raw.data <- reactive({
    
    infile <- input$CSVfile
    message("Reading CSV:", infile$datapath)
    if (is.null(infile)) {
      # Randomly generated sample data set
      return(sample.data)
    }
    
    read.csv(infile$datapath)
    
  })
  
  data.numCols <- reactive({
    
    names(which(lapply(X = raw.data(), FUN = class) == "numeric"))
    
  })
  
  data.charCols <- reactive({
    
    names(which(lapply(X = raw.data(), FUN = class) == "character"))
    
  })
  
  FC.default <- reactive({
    
    # Guess logFC column by name
    FC.colname <- colnames(raw.data())[grep(
      pattern = "(log)?[[:digit:]]*f(old)?c(change)?",
      x = tolower(colnames(raw.data())))[1]]
    # Otherwise, assume logFC column is the one with most symmetrical values
    if (length(FC.colname) == 0){
      sumMinMax <- lapply(
        X = data.NA()[,data.numCols()],
        FUN = function(x){sum(range(raw.data()))})
      FC.colname <- colnames(raw.data())[which.min(sumMinMax)]
    }
    
    message("FC.colname:", FC.colname)
    FC.colname
    
  })
  
  pval.default <- reactive({
    
    # Guess logFC column by name
    pval.colname <- colnames(raw.data())[grep(
      pattern = "p.?val(ue)?",
      x = tolower(colnames(raw.data())))[1]]
    # Otherwise, assume logFC column is the one with values between 0 and 1
    # with the most values close to 1
    if (length(pval.colname) == 0){
      if (length(data.numCols()) > 0){
        ranges <- do.call("rbind", lapply(
          X = raw.data()[,data.numCols()],
          FUN = range,
          na.rm = TRUE))
        ranges.ZeroOne <- which(ranges[,1] >= 0 & ranges[,2] <= 1)
        if (length(ranges.ZeroOne) > 1){
          # Pick the one with the largest number of values close to 1
          pval.sum <- colSums(raw.data[,ranges.ZeroOne], na.rm = TRUE)
          pval.colname <- colnames(raw.data[,ranges.ZeroOne])[
            which.max(pval.sum)]
        } else {
          pval.colname <- names(ranges.ZeroOne)
        }
      }
    }
    
    pval.colname
    
  })
  
  padj.default <- reactive({
    
    # Guess logFC column by name
    padj.colname <- colnames(raw.data())[grep(
      pattern = "p?adj(usted)?p?",
      x = tolower(colnames(raw.data())))[1]]
    # Otherwise, assume Padj column is the one with values between 0 and 1
    # with the most values close to 0
    if (length(padj.colname) == 0){
      if (length(data.numCols()) > 0){
        ranges <- do.call("rbind", lapply(
          X = raw.data()[,data.numCols()],
          FUN = range,
          na.rm = TRUE))
        ranges.ZeroOne <- which(ranges[,1] >= 0 & ranges[,2] <= 1)
        if (length(ranges.ZeroOne) > 1){
          # Pick the one with the largest number of values close to 0
          pval.sum <- colSums(raw.data[,ranges.ZeroOne], na.rm = TRUE)
          padj.colname <- colnames(raw.data[,ranges.ZeroOne])[
            which.min(pval.sum)]
        } else {
          padj.colname <- names(ranges.ZeroOne)
        }
      }
    }
    
    padj.colname
    
  })
  
  symbol.default <- reactive({
    
    # Guess logFC column by name
    symbol.colname <- colnames(raw.data())[grep(
      pattern = "symbol|gene|name",
      x = tolower(colnames(raw.data())))[1]]
    # Otherwise, assume symbol column is the one with unique character values
    if (length(symbol.colname) == 0){
      unique <- lapply(
        X = data.NA()[,data.charCols()],
        FUN = function(x){length(unique(x)) == length(x)})
      symbol.colname <- colnames(raw.data())[unique[1]]
    }
    
    message("symbol.colname:", symbol.colname)
    symbol.colname
    
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
    
    # print(dimnames(data.NA()))
    # message("input$volcano.logFC: ", input$volcano.logFC)
    # message("input$volcano.pval: ", input$volcano.pval)
    # message("input$FDR: ", input$FDR)
    message("dataset.name: ", input$dataset.name)
    if (input$symmetric){
      xlimits <- rep(max(abs(data.NA()[,input$volcano.logFC]))) * c(-1, 1)
    } else {
      xlimits <- range(data.NA()[,input$volcano.logFC])
    }
    
    ggplot(
      data = data.NA(),
      mapping = aes_string(
        x = input$volcano.logFC,
        y = paste("-log10(",input$volcano.pval,")", sep = ""))) +
      geom_point(
        colour = as.numeric(data.NA()[,input$volcano.padj] <= input$FDR) + 1,
        size = 2) +
      geom_text(
        data = data.NA()[data.NA()[,input$volcano.padj] <= input$FDR,],
        mapping = aes_string(label = "SYMBOL"),
        check_overlap = TRUE) +
      ggtitle(dataset.name()) +
      scale_x_continuous(limits = xlimits) +
      xlab(input$volcano.logFC) +
      ylab(paste("-log10(",input$volcano.pval,")"))

  })
  


})
