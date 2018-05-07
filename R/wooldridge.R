utils::globalVariables(c("phi", "myprobit_lag", "beta_fs_matrix", "beta_fs_free", "beta_fs_controls", "ss_reg", "nls.lm.control"))
#' @title Wooldridge Estimation of Production Functions (Cobb-Douglas)
#' @description This function aims the estimation of Cobb-Douglas production functions using \href{https://doi.org/10.1016/j.econlet.2009.04.026}{Wooldridge (2009)} method.
#' @param data A data.frame or tibble containing the variables of the model.
#' @param formula An object of the class \code{\link[stats]{formula}}.
#' @param gross If TRUE dependent variable is gross output.
#' @param id A character with the name of the indicator variable.
#' @param time A character with the name of the time variable.
#' @param bootstrap An optional logical. If TRUE calculate bootstrap standard errors.
#' @param reps The number of bootstrap replications.
#' @param degree A vector with the number of the polynomial interactions in each stage of the routine.
#' @param verify Verify if inputs are sorted.
#' @param ... Additional arguments.
#' @details Multipart formula must be specified in the following order: \code{y ~ free | capital | proxy | controls}. Additional controls are optional.
#' It is possible to use more than one variable, although the use of more than one capital may not be theoretically identified.
#' The function returns an object of the estprod or boot classes (if \code{bootstrap} is TRUE).
#' @examples data(estprod_data)
#' wooldridge(data = estprod_data, var1 ~ var2 | var3 | var4, 
#' id = "id", time = "year", bootstrap = TRUE)
#' @export

wooldridge <- function(data, formula = y ~ free | capital | proxy | controls, gross = FALSE, id = "id", time = "year",
                       bootstrap = FALSE, reps = 2, degree = c(3, 2), verify = TRUE, ...){
  
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
    
    if (any(controls_names == "Null")){
      stop("Controls names can not be 'Null', it's a reserved word.")
    }
    
  } else {
    #será zero para primeiro estagio funcionar
    controls <- as.matrix(rep(0, n_length))
    controls_length <- 0
    controls_names <- "Null"
    colnames(controls) <- controls_names
  }
  
  
  y <- as.matrix(model.frame(formula, data = data, lhs = 1, rhs = 0))
  
  
  k_length <- dim(capital)[2]
  
  free_length <- dim(free)[2]
  
  proxy_length <- dim(proxy)[2]
  
  
  ############################################
  #Wooldridgw specific matrix
  ############################################  
  
  free_lag <- panel_lag(free, id, time, 1, verify = F)
  
  colnames(free_lag) <- paste0("lag_", colnames(free))
  
  free_lag_names <- colnames(free_lag)
  
  
  
  interaction_vars <- poly(cbind(capital, proxy), degree = degree[1])
  
  interaction_vars <- interaction_vars[,seq(attr(interaction_vars, "degree"))[attr(interaction_vars, "degree") > 1]]
  
  interactions_length <- dim(interaction_vars)[2]
  
  colnames(interaction_vars) <- paste0("inter_", colnames(interaction_vars))
  
  interaction_vars_names <- colnames(interaction_vars)
  
  

  
  interaction_lag_vars <- panel_lag(interaction_vars, id = id, time = time, verify = F)
  
  interaction_lag_length <- dim(interaction_lag_vars)[2]
  
  colnames(interaction_lag_vars) <- paste0("lag_", colnames(interaction_lag_vars))
  
  interaction_lag_vars_names <- colnames(interaction_lag_vars)
  
  
  y_name <- colnames(y)
  
  
  df_ss <- cbind(y, free, free_lag, proxy, capital, controls, interaction_vars, interaction_lag_vars)
  
  df_ss <- df_ss[complete.cases(df_ss),]  


  
  ############################################
  #GMM
  ############################################  
  
  if (controls_names == "Null") {
    eq1 <- as.formula(paste0(y_name, "~", paste0(c(free_names, capital_names, interaction_vars_names), collapse =  "+")))
    
    eq2 <- as.formula(paste0(y_name, "~", paste0(c(free_names, capital_names, interaction_lag_vars_names), collapse =  "+")))
    
    eq_list <- list(eq1, eq2)
    
    z_list <- list(as.formula(paste( "~ ", paste0(c(free_names, capital_names, interaction_vars_names), collapse = "+"))), 
                   as.formula(paste( "~ ", paste0(c(free_lag_names, capital_names, interaction_lag_vars_names), collapse = "+"))))
    
  } else if (controls_names != "Null") {
    
    eq1 <- as.formula(paste0(y_name, "~", paste0(c(free_names, capital_names, controls_names, interaction_vars_names), collapse =  "+")))
    
    eq2 <- as.formula(paste0(y_name, "~", paste0(c(free_names, capital_names, controls_names, interaction_lag_vars_names), collapse =  "+")))
    
    eq_list <- list(eq1, eq2)
    
    z_list <- list(as.formula(paste( "~ ", paste0(c(free_names, capital_names, controls_names, interaction_vars_names), collapse = "+"))), 
                   as.formula(paste( "~ ", paste0(c(free_lag_names, capital_names, controls_names, interaction_lag_vars_names), collapse = "+"))))
    
  } else {
    stop("Some error occured!")
  }

  woold <- gmm::sysGmm(eq_list, z_list, data = as.data.frame(df_ss), crossEquConst = (2):(1 + free_length + k_length + controls_length))
  
  
  #############################################
  #bootstrap
  #############################################
  if (bootstrap == TRUE){
    
    call_boot <- lazyeval::call_modify(called_func, new_args = list(data = quote(data_df), statistic = quote(boot_wooldridge), R = reps, strata = quote(id), bootstrap = FALSE))
    
    call_boot[[1]] <- quote(boot::boot)
    
    boot_result <- eval(call_boot)
    
    boot_result[["Call"]] <- called_func
    
    boot_result[["varnames"]] <-  c(free_names,
                                    capital_names,
                                    switch((controls_length > 0) + 1 , NULL, controls_names))
    
    class(boot_result) <- c("estprod", "boot")
    
    return(boot_result)
  }
  
  #############################################
  #Without bootstrap
  #############################################
  
  results <- list()
  
  woold <- summary(woold)
  
  
  results[["Call"]] <- called_func
  
  results[["gmm_coefficients"]] <- woold[["coefficients"]]
  
  
  
  Coefficients <- woold[["coefficients"]][[1]][2:(1 + free_length + k_length + controls_length),]
  
  results[["Coefficients"]] <- Coefficients
  
  
  
  attr(results, "class") <- c("estprod", "list")
  
  return(results)
  

}