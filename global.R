
# Random data set before CSV is provided
log2FC <- rnorm(n = 10E3, mean = 0, sd = 2)
log2FC <- log2FC[order(abs(log2FC), decreasing = TRUE)]

Pvalue <- c(
  runif(n = 100, min = 0, max = 0.05),
  runif(n = 9900, min = 0, max = 1)
)
Pvalue <- Pvalue[order(Pvalue)]

padj <- p.adjust(p = Pvalue, method = "BH")
SYMBOL <- paste("Gene", sprintf("%04.0f", 1:1000), sep = "_")

sample.data <- data.frame(
  log2FC = log2FC,
  Pvalue = Pvalue,
  padj = padj,
  SYMBOL = SYMBOL
)

sample.datasetName <- "Randomly generated data"