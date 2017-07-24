ss_lp_va <- function(y, free, capital, proxy, controls, id, time, phi, myprobit_lag, degree, k_length, proxy_length, controls_length, beta_fs_matrix, beta_fs_free, maxiter, ...) {
  #-----------------------------------------------------
  #Segundo estágio (ss) valor adicionad
  #-----------------------------------------------------

  capital_lag = panel_lag(capital, id, time, verify = FALSE)
  colnames(capital_lag) <- paste0("lag_", colnames(capital_lag))
  names_capital_lag <- colnames(capital_lag)
  
  controls_lag = panel_lag(controls, id, time, verify = FALSE)
  colnames(controls_lag) <- paste0("lag_", colnames(controls))
  names_controls_lag <- colnames(controls_lag)
  
  df_ss <- cbind(y_ss = c(y),
                 free,
                 capital,
                 capital_lag,
                 phi = c(phi),
                 phi_lag = c(panel_lag(phi, id, time, verify = FALSE)),
                 controls,
                 controls_lag)
  
  df_ss <- df_ss[complete.cases(df_ss),]
  
  #rm(phi, capital_lag, controls_lag)
  
  #objective function = sum of residuals ^2
  objective <- function(start, data_ss = df_ss) {
    #constante: primeira variável. beta_k: (1 + 1) até o numero de k + 1  
    #beta_poly: as demais
    
    beta_k <- start[1:k_length]
    beta_ss_controls <- start[-1:-k_length]
    
    data_ss <- as.matrix(data_ss)
    
    #omega = phi - beta_k * k
    data_ss <- cbind(data_ss, 
                     omega = c(data_ss[, "phi"] - as.matrix(data_ss[, colnames(capital)]) %*% as.matrix(beta_k) - data_ss[, colnames(controls)] %*% as.matrix(beta_ss_controls)),
                     lag_omega = c(data_ss[, "phi_lag"] - data_ss[, names_capital_lag] %*% as.matrix(beta_k) - data_ss[, names_controls_lag] %*% as.matrix(beta_ss_controls))) 
    
    
    omega_pol <- cbind(const = 1, poly(cbind(data_ss[, "lag_omega"], myprobit_lag), degree = degree[2]))
    
    
    omega_pol <- qr(omega_pol)
    omega_hat <- qr.fitted(omega_pol, data_ss[, "omega"])
    
    data_ss <- as.matrix(data_ss) 
    
    lp <- data_ss[, colnames(free)] %*% as.matrix(beta_fs_free) + data_ss[, colnames(capital)] %*% as.matrix(beta_k) + data_ss[, colnames(controls)] %*% as.matrix(beta_ss_controls) + as.matrix(omega_hat)
    lp <- (data_ss[, 1] - lp)
    
    
    return(lp)

  }
  
  #n_start = n_capital
  start <- rep(0, (k_length + switch((controls_length > 0) + 1 , 1, controls_length)))
  
  ss_reg <- minpack.lm::nls.lm(par = start, fn = objective, control = minpack.lm::nls.lm.control(maxiter = maxiter))
  
  assign("ss_reg", ss_reg, envir = parent.frame())
  
}