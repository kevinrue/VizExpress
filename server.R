
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("serverFunctions.R")

shinyServer(function(input, output, clientData, session) {
  
  observe({
    
    updateSelectInput(
      session, "volcano_logFC",
      choices = data.numCols(),
      selected = FC.default()
    )
    
    updateSelectInput(
      session, "volcano_pval",
      choices = data.numCols(),
      selected = pval.default()
    )
    
    updateSelectInput(
      session, "volcano_padj",
      choices = data.numCols(),
      selected = padj.default()
    )
    
    updateSelectInput(
      session, "volcano_symbol",
      choices = c(volcanoSymbolNone, data.charCols()),
      selected = symbol.default()
    )
    
    updateTextInput(
      session, "dataset_name",
      value = single.datasetName()
    )
    
    updateSelectInput(
      session, "ma_mean",
      choices = data.numCols(),
      selected = mean.default()
    )
    
  })

  single.datasetName <- callModule(
    setSingleDataset, "input_file",
    stringsAsFactors = FALSE)

  output$datasetName <- renderText({

    single.datasetName()

  })

  single.rawdata <- callModule(
      readSingleFile, "input_file",
      stringsAsFactors = FALSE)

  data.numCols <- reactive({

    names(which(lapply(X = single.rawdata(), FUN = class) == "numeric"))

  })

  data.charCols <- reactive({

    names(which(lapply(X = single.rawdata(), FUN = class) == "character"))

  })

  FC.default <- reactive({

    # Guess logFC column by name
    FC.colname <- colnames(single.rawdata())[grep(
      pattern = "(log)?[[:digit:]]*f(old)?c(change)?",
      x = tolower(colnames(single.rawdata())))[1]]
    # Otherwise, assume logFC column is the one with most symmetrical values
    if (length(FC.colname) == 0){
      sumMinMax <- lapply(
        X = data.NA()[,data.numCols()],
        FUN = function(x){sum(x, na.rm = TRUE)})
      FC.colname <- colnames(single.rawdata())[which.min(sumMinMax)]
    }

    message("FC.colname:", FC.colname)
    FC.colname

  })

  pval.default <- reactive({

    # Guess logFC column by name
    pval.colname <- colnames(single.rawdata())[grep(
      pattern = paste0(c("p.?val(ue)?", p.adjust.methods), collapse = "|"),
      x = tolower(colnames(single.rawdata())))[1]]
    # Otherwise, assume logFC column is the one with values between 0 and 1
    # with the most values close to 1
    if (length(pval.colname) == 0){
      if (length(data.numCols()) > 0){
        ranges <- do.call("rbind", lapply(
          X = single.rawdata()[,data.numCols()],
          FUN = range,
          na.rm = TRUE))
        ranges.ZeroOne <- which(ranges[,1] >= 0 & ranges[,2] <= 1)
        if (length(ranges.ZeroOne) > 1){
          # Pick the one with the largest number of values close to 1
          pval.sum <- colSums(single.rawdata[,ranges.ZeroOne], na.rm = TRUE)
          pval.colname <- colnames(single.rawdata[,ranges.ZeroOne])[
            which.max(pval.sum)]
        } else {
          pval.colname <- names(ranges.ZeroOne)
        }
      }
    }

    message("pval.colname:", pval.colname)
    pval.colname

  })

  padj.default <- reactive({

    # Guess logFC column by name
    padj.colname <- colnames(single.rawdata())[grep(
      pattern = "p?adj(usted)?p?",
      x = tolower(colnames(single.rawdata())))[1]]
    # Otherwise, assume Padj column is the one with values between 0 and 1
    # with the most values close to 0
    if (length(padj.colname) == 0){
      if (length(data.numCols()) > 0){
        ranges <- do.call("rbind", lapply(
          X = single.rawdata()[,data.numCols()],
          FUN = range,
          na.rm = TRUE))
        ranges.ZeroOne <- which(ranges[,1] >= 0 & ranges[,2] <= 1)
        if (length(ranges.ZeroOne) > 1){
          # Pick the one with the largest number of values close to 0
          pval.sum <- colSums(single.rawdata[,ranges.ZeroOne], na.rm = TRUE)
          padj.colname <- colnames(single.rawdata[,ranges.ZeroOne])[
            which.min(pval.sum)]
        } else {
          padj.colname <- names(ranges.ZeroOne)
        }
      }
    }

    message("padj.colname:", padj.colname)
    padj.colname

  })

  symbol.default <- reactive({

    # Guess logFC column by name
    # Prefer a column including "symbol"
    symbol.colname <- colnames(single.rawdata())[grep(
      pattern = "symbol",
      x = tolower(colnames(single.rawdata())))[1]]
    # A few alternatives
    if (length(symbol.colname) == 0){
      symbol.colname <- colnames(single.rawdata())[grep(
        pattern = "gene|name",
        x = tolower(colnames(single.rawdata())))[1]]
    }
    # Otherwise, assume symbol column is the first one
    # with unique character values
    if (length(symbol.colname) == 0){
      unique <- lapply(
        X = data.NA()[,data.charCols()],
        FUN = function(x){length(unique(x)) == length(x)})
      if (length(unique) > 1){
        symbol.colname <- colnames(single.rawdata())[unique[1]]
      }
      else{
        symbol.colname <- NA
      }
    }

    message("symbol.colname:", symbol.colname)
    symbol.colname

  })
  
  mean.default <- reactive({
    
    # Guess logFC column by name
    mean.colname <- colnames(single.rawdata())[grep(
      pattern = "(base)?mean|av(era)?g(e)?", # (log)?[[:digit:]]*f(old)?c(change)?
      x = tolower(colnames(single.rawdata())))[1]]
    # Otherwise, assume logFC column is the one with the largest positive values
    if (length(mean.colname) == 0){
      sum <- lapply(
        X = data.NA()[,data.numCols()],
        FUN = function(x){sum(x, na.rm = TRUE)})
      mean.colname <- colnames(single.rawdata())[which.max(sumMinMax)]
    }
    
    message("mean.colname:", mean.colname)
    mean.colname
    
  })

  data.NA <- reactive({

    single.rawdata()[!is.na(single.rawdata()[,input$volcano_padj]),]

  })

  output$rawTable <- renderDataTable({

    DT::datatable(
      data = single.rawdata(),
      options = list(
        lengthMenu = c(10, 50, 100),
        pageLength = 10
      )
    )

  })

  output$volcanoPlot <- renderPlot({

    if (input$symmetric){
      xlimits <- rep(max(abs(data.NA()[,input$volcano_logFC]))) * c(-1.05, 1.05)
    } else {
      xlimits <- range(data.NA()[,input$volcano_logFC]) * 1.05
    }

    gg <- ggplot(
      data = data.NA(),
      mapping = aes_string(
        x = input$volcano_logFC,
        y = paste("-log10(",input$volcano_pval,")", sep = ""))) +
      geom_point(
        colour = sigNon.colours[
          as.numeric(data.NA()[,input$volcano_padj] <= input$FDR) + 1],
        size = 2) +
      ggtitle(paste(
        "Volcano plot",
        input$dataset_name, sep = "\n")) +
      scale_x_continuous(limits = xlimits) +
      xlab("log (fold-change)") +
      ylab(expression(-log[10]*" (unadjusted "*italic("P")*"-"*value*")")) +
      theme(
        axis.text = element_text(size = rel(1.25)),
        title = element_text(size = rel(1.5))
      )

    if (!input$volcano_symbol == volcanoSymbolNone){
      gg <- gg +
        geom_text(
          data = data.NA()[data.NA()[,input$volcano_padj] <= input$FDR,],
          mapping = aes_string(label = input$volcano_symbol),
          check_overlap = TRUE)
    }

    gg

  })

  output$QQplot <- renderPlot({

    qq.data <- cbind(
      data.NA()[order(data.NA()[,input$volcano_pval]),],
      expected = sort(runif(n = nrow(data.NA()), min = 0, max = 1)))

    gg <- ggplot(
      data = qq.data,
      mapping = aes_string(
          x = "-log10(expected)",
          y = paste("-log10(",input$volcano_pval,")", sep = ""))) +
      geom_point(
        colour = sigNon.colours[
          as.numeric(data.NA()[,input$volcano_padj] <= input$FDR) + 1],
        size = 2) +
      geom_abline(slope = 1, intercept = 0, colour = "red") +
      ggtitle(paste(
        "Quantile-Quantile Plot",
        input$dataset_name, sep = "\n")) +
      xlab(
        expression(
          Expected*" "*"-"*log[10]*" ( unadjusted "*italic(P)*"-"*value*")")
        ) +
      ylab(
        expression(
          Observed*" "*"-"*log[10]*" ( unadjusted "*italic(P)*"-"*value*")")
        ) +
      scale_x_continuous(
        limits = range(-log10(qq.data[,"expected"]) * 1.05)) +
      scale_y_continuous(
        limits = range(-log10(qq.data[,input$volcano_pval]) * 1.05)) +
      theme(
        axis.text = element_text(size = rel(1.25)),
        title = element_text(size = rel(1.5))
        )

    if (!input$volcano_symbol == volcanoSymbolNone){
      gg <- gg +
        geom_text(
          data = qq.data[qq.data[,input$volcano_padj] <= input$FDR,],
          mapping = aes_string(label = input$volcano_symbol),
          check_overlap = TRUE)
    }

    gg

  })
  
  output$MAplot <- renderPlot({
    
    ma.data <- cbind(data.NA())
    
    if (input$logMean){
      ma.data[,input$ma_mean] <- log2(data.NA()[,input$ma_mean])
      meanLabel <- expression(log[2]*" (mean signal)")
    } else {
      meanLabel <- "Mean signal"
    }

    gg <- ggplot(
      data = ma.data,
      mapping = aes_string(
        x = input$ma_mean,
        y = input$volcano_logFC)) +
      geom_point(
        colour = sigNon.colours[
          as.numeric(data.NA()[,input$volcano_padj] <= input$FDR) + 1],
        size = 2) +
      ggtitle(paste(
        "Minus-Average Plot",
        input$dataset_name, sep = "\n")) +
      xlab(meanLabel) +
      ylab("log (fold-change)") +
      theme(
        axis.text = element_text(size = rel(1.25)),
        title = element_text(size = rel(1.5))
      )
    
    if (!input$volcano_symbol == volcanoSymbolNone){
      gg <- gg +
        geom_text(
          data = ma.data[ma.data[,input$volcano_padj] <= input$FDR,],
          mapping = aes_string(label = input$volcano_symbol),
          check_overlap = TRUE)
    }
    
    gg
    
  })

})
