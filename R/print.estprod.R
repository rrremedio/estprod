#' @export
print.estprod <- function(x, ...){

  #-----------------------------------------------------
  #Results with and without bootstrap
  #-----------------------------------------------------
  
  results <- list()
  
  if (any(class(x) == "boot")) {
    
    results[["Call"]] <- x$Call
    
    se <- apply(x$t, 2, function(z) sqrt(var(z)))
    
    #lower.tail = FALSE = 2*(1-pnorm())
    Coefficients <- cbind(Estimate = x$t0, `Std. Error` = se, 
                          `z value` = x$t0/se, `Pr(>|z|)` = 2 * pnorm(abs(x$t0/se), lower.tail = FALSE))

    Coefficients <- Coefficients[1:length(x$varnames),] #filtra para não icluir termos do polinomio
    
    rownames(Coefficients) <- x$varnames
    
    results[["Coefficients"]] <- Coefficients

    
  } else {
    results <- x
  }
  
  if (results$Call[[1]] == "levinsoh_petrin") {
    cat("Levinsoh-Petrin Production Function Estimator\n\n")
  } else if (results$Call[[1]] == "olley_pakes") {
    cat("Olley-Pakes Production Function Estimator\n\n")
  } else if (results$Call[[1]] == "wooldridge") {
    cat("Wooldridge Production Function Estimator\n\n")
  }
  
  class(results) <- "list"

  cat("Call\n")
  print(results[["Call"]])
  cat("\n")
  
  cat("Coefficients\n")
  printCoefmat(results[["Coefficients"]])
  cat("\n")
  
  if (any(class(x) == "boot")) {
    cat("#Bootstraped standard errors.")
  }
  
}
