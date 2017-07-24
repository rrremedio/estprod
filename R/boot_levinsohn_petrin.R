boot_levinsohn_petrin <- function(data, index, ...){
  data <- data[index, ]
  results <- levinsohn_petrin(data = data, verify = FALSE, ...)
  c(results[["Coefficients"]])
}
