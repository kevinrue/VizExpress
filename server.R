
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
  
  })
    
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
  
  data.numCols <- reactive({
    
    names(which(lapply(X = raw.data(), FUN = class) == "numeric"))
    
  })
  
  FC.default <- reactive({
    
    # Guess logFC column by name
    FC.colname <- grep(
      pattern = "[Ll]og[Ff]2{0,1}[Cc]|[Ff]old[Cc]hange",
      x = colnames(raw.data()),
      value = TRUE)[1]
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
    pval.colname <- grep(
      pattern = "[Pp][\\.]{0,1}[Vv]al(ue){0,1}",
      x = colnames(raw.data()),
      value = TRUE)[1]
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
      xlimits <- rep(max(abs(data.NA()[,FC.default()]))) * c(-1, 1)
    } else {
      xlimits <- range(data.NA()[,FC.default()])
    }
    
    ggplot(
      data = data.NA(),
      mapping = aes_string(
        x = input$volcano.logFC,
        y = paste("-log10(",input$volcano.pval,")", sep = ""))) +
      geom_point(
        colour = as.numeric(data.NA()[,"padj"] <= input$FDR) + 1,
        size = 2) +
      geom_text(
        data = data.NA()[data.NA()[,"padj"] <= input$FDR,],
        mapping = aes_string(label = "SYMBOL"),
        check_overlap = TRUE) +
      ggtitle(dataset.name()) +
      scale_x_continuous(limits = xlimits) +
      xlab(input$volcano.logFC) +
      ylab(paste("-log10(",input$volcano.pval,")"))

  })
  


})
