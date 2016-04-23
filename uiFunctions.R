
singleFileInput <- function(id, label = "Input file") {
  ns <- NS(id)
  
  tagList(
    fileInput(
      inputId = ns("file"),
      label = label,
      multiple = FALSE,
      accept = c('text/csv', '.csv')
      )
    )
}

multipleFilesInput <- function(id, label = "Input files") {
  ns <- NS(id)
  
  tagList(
    fileInput(
      inputId = ns("files"),
      label = label,
      multiple = TRUE,
      accept = c('text/csv', '.csv')
    )
  )
}