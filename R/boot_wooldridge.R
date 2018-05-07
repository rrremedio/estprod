boot_wooldridge <- function(data, index, ...){
  data <- data[index, ]
  results <- wooldridge(data = data, verify = FALSE, ...)
  c(results[["Coefficients"]][, 1])
}
