boot_olley_pakes <- function(data, index, ...){
  data <- data[index, ]
  results <- olley_pakes(data = data, verify = FALSE, ...)
  c(results[["Coefficients"]])
}