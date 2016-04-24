# Random data set before CSV is provided

randomDataDiff <- function(){
  log2FC <- rnorm(n = 10E3, mean = 0, sd = 1)
  log2FC <- log2FC[order(abs(log2FC), decreasing = TRUE)]
  
  Pvalue <- c(
    runif(n = 10E3, min = 0, max = 1)
  )
  Pvalue <- Pvalue[order(Pvalue)]
  
  padj <- p.adjust(p = Pvalue, method = "BH")
  SYMBOL <- paste("Gene", sprintf("%04.0f", 1:1000), sep = "_")
  
  data.frame(
    log2FC = log2FC,
    Pvalue = Pvalue,
    padj = padj,
    SYMBOL = SYMBOL,
    stringsAsFactors = FALSE
  )
  
}

readSingleFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <-  reactive({ input$file })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    
    # Return a random data frame if no file was uploaded yet
    if (is.null(userFile())){
      return(randomDataDiff())
    }
    
    read.csv(
      file = userFile()$datapath,
      stringsAsFactors = stringsAsFactors)
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

setSingleDataset <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <-  reactive({ input$file })
  
  # The user's data, parsed into a data frame
  datasetName <- reactive({
    
    # Return a random data frame if no file was uploaded yet
    if (is.null(userFile())){
      return(randomDatasetName)
    }
    
    gsub(pattern = '\\.[[:alpha:]]+$', replacement = '', x = userFile()$name)
  
  })
  
  # Return the reactive that yields the data frame
  return(datasetName)
}