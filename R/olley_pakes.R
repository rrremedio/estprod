utils::globalVariables(c("phi", "myprobit_lag", "beta_fs_matrix", "beta_fs_free", "beta_fs_controls", "ss_reg", "nls.lm.control"))
#' @title Olley-Pakes Estimation of Production Functions
#' @description This function aims the estimation of production functions using \href{https://www.jstor.org/stable/2171831?seq=1#page_scan_tab_contents}{Olley-Pakes (1996)}.
#' @param data A data.frame or tibble containing the variables of the model.
#' @param formula An object of the class \code{\link[stats]{formula}}.
#' @param exit An optional formula with the name of the variabe indicator of firm's last period. \code{~exit}, for example.
#' @param id A character with the name of the indicator variable.
#' @param time A character with the name of the time variable.
#' @param bootstrap An optional logical. If TRUE calculate bootstrap standard errors.
#' @param reps The number of bootstrap replications.
#' @param degree A vector with the number of the polynomial interactions in each stage of the routine.
#' @param verify Verify if inputs are sorted.
#' @param maxiter Parameter of \code{nls.lm} at second stage.
#' @param ... Additional arguments.
#' @details Multipart formula must be specified in the following order: \code{y ~ free | capital | proxy | controls}. Additional controls are optional.
#' It is possible to use more than one variable, although the use of more than one capital may not be theoretically identified.
#' The function returns an object of the estprod or boot classes (if \code{bootstrap} is TRUE).
#' @examples data(estprod_data)
#' olley_pakes(data = estprod_data, var1 ~ var2 | var3 | var4, 
#' exit = ~exit, id = "id", time = "year", bootstrap = TRUE)
#' @export

olley_pakes <- function(data, formula = y ~ free | capital | proxy | controls, exit = NULL, id = "id", time = "year", 
                            bootstrap = TRUE, reps = 2, degree = c(3, 2), verify = TRUE, maxiter = 100, ...){
  
  formula <- Formula::Formula(formula)
  
  #-----------------------------------------------------
  #verify it has minimun arguments
  #-----------------------------------------------------
  if (max(length(formula)) < 3){
    stop("The right hand side of the formula must constain at least the first three parts!")
  }
  if (!exists("data")){
    stop("Data is missing!")
  }
  
  
  data_df <- as.data.frame(data)
  
  id <- data_df[, id]
  
  time <- data_df[, time]
  
  
  if (!identical(order(id, time), 1:length(id))) {
    if (verify) {
      stop("Panel not sorted.") 
    }
  }
  
  
  called_func <- match.call()
  
  #-----------------------------------------------------
  #Matrizes a partir da formula_fs
  #-----------------------------------------------------
  n_length <- dim(data)[1]
  
  free <- as.matrix(model.frame(formula, data = data, lhs = 0, rhs = 1))
  colnames(free) <- labels(terms(formula(formula, lhs = 0, rhs = 1)))
  free_names <- colnames(free)
  
  capital <- as.matrix(model.frame(formula, data = data, lhs = 0, rhs = 2))
  colnames(capital) <- labels(terms(formula(formula, lhs = 0, rhs = 2)))
  capital_names <- colnames(capital)
  
  proxy <- as.matrix(model.frame(formula, data = data, lhs = 0, rhs = 3))
  colnames(proxy) <- labels(terms(formula(formula, lhs = 0, rhs = 3)))
  proxy_names <- colnames(proxy)
  
  if (length(formula)[2] == 4){
    controls <- as.matrix(model.frame(formula, data = data, lhs = 0, rhs = 4))
    colnames(controls) <- labels(terms(formula(formula, lhs = 0, rhs = 4)))
    controls_names <- colnames(controls)
    controls_length <- dim(controls)[2]
  } else {
    #será zero para primeiro estagio funcionar
    controls <- as.matrix(rep(0, n_length))
    controls_length <- 0
    controls_names <- "zero"
    colnames(controls) <- controls_names
  }
  
  
  y <- as.matrix(model.frame(formula, data = data, lhs = 1, rhs = 0))
  
  
  k_length <- dim(capital)[2]
  
  free_length <- dim(free)[2]
  
  proxy_length <- dim(proxy)[2]
  
  
  
  
  ############################################
  #Primeiro Estágio
  ############################################  
  
  first_stage <- fs(data = data_df, y = y, free = free, capital = capital, proxy = proxy, controls = controls, exit = exit, id = id, time = time, formula = formula, degree = degree)
  
  ############################################
  #Segundo Estágio
  ############################################
  
  second_stage <- ss_op(y = y, free = free, capital = capital, proxy = proxy, controls = controls, id = id, time = time, phi = phi, myprobit_lag = myprobit_lag, 
                        degree = degree, k_length = k_length, proxy_length = proxy_length, controls_length = controls_length, 
                        beta_fs_matrix = beta_fs_matrix, beta_fs_free = beta_fs_free, beta_fs_controls = beta_fs_controls, maxiter = maxiter)
  
  #############################################
  #bootstrap
  #############################################
  if (bootstrap == TRUE){
    
    call_boot <- lazyeval::call_modify(called_func, new_args = list(data = quote(data_df), statistic = quote(boot_olley_pakes), R = reps, strata = quote(id), bootstrap = FALSE))
    
    call_boot[[1]] <- quote(boot::boot)
    
    boot_result <- eval(call_boot)
    
    boot_result[["Call"]] <- called_func
    
    boot_result[["varnames"]] <-  c(free_names,
                                    capital_names,
                                    switch((controls_length > 0) + 1 , NULL, controls_names))
    
    class(boot_result) <- c("estprod", "boot")
    
    return(boot_result)
  }
  
  #-----------------------------------------------------
  #Results without bootstrap
  #-----------------------------------------------------
  
  results <- list()
  
  results[["Call"]] <- called_func
  
  Coefficients <- matrix(c(beta_fs_free,
                           ss_reg$par[2:(k_length + 1)],
                           switch((controls_length > 0) +1 , NULL, beta_fs_controls)), ncol = 1)

  rownames(Coefficients) <-  c(free_names,
                               capital_names,
                               switch((controls_length > 0) + 1 , NULL, controls_names))
  
  colnames(Coefficients) <- c("Estimate")
  
  results[["Coefficients"]] <- Coefficients
  
  attr(results, "class") <- c("estprod", "list")
  
  return(results)
  
}