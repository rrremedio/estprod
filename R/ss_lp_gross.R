ss_lp_gross <- function(y, free, capital, proxy, controls, id, time, phi, myprobit_lag, k_length, proxy_length, controls_length, beta_fs_matrix, beta_fs_free, maxiter, ...) {
  
  #-----------------------------------------------------
  #Segundo estágio (ss) gross
  #-----------------------------------------------------
  #tenho q editar se tiver mais d um capital
  
  capital_lag <- panel_lag(capital, id, time, verify = FALSE)
  colnames(capital_lag) <- paste0("lag_", colnames(capital_lag))
  names_capital_lag <- colnames(capital_lag)
  
  controls_lag <- panel_lag(controls, id, time, verify = FALSE)
  colnames(controls_lag) <- paste0("lag_", colnames(controls_lag))
  names_controls_lag <- colnames(controls_lag)
  
  free_lag <- panel_lag(free, id, time, verify = FALSE)
  colnames(free_lag) <- paste0("lag_", colnames(free_lag))
  names_free_lag <- colnames(free_lag)
  
  proxy_lag <- panel_lag(proxy, id, time, verify = FALSE)
  colnames(proxy_lag) <- paste0("lag_", colnames(proxy_lag))
  names_proxy_lag <- colnames(proxy_lag)
  
  proxy_2_lag <- panel_lag(proxy_lag, id, time, verify = FALSE)
  colnames(proxy_2_lag) <- paste0("lag_", colnames(proxy_lag))
  names_proxy_2_lag <- colnames(proxy_2_lag)
  
  inst_names <- c(names_free_lag, names_proxy_lag, names_proxy_2_lag, names_capital_lag, colnames(capital))
  
  
  df_ss <- cbind(y  = c(y),
                 free,
                 capital,
                 capital_lag,
                 phi = c(phi),
                 phi_lag = c(panel_lag(phi, id, time, verify = FALSE)),
                 controls,
                 controls_lag,
                 free_lag,
                 proxy,
                 proxy_lag,
                 proxy_2_lag)
  
  df_ss <- df_ss[complete.cases(df_ss[, "phi_lag"]),]
  
  #rm(phi, free, capital, capital_lag, controls_lag, free_lag, proxy_lag, proxy_2_lag)
  
  #objective function = sum of residuals ^2
  objective <- function(start, data_ss = df_ss) {
    
    #constante: primeira variável. beta_k: (1 + 1) até o numero de k + 1  
    #beta_poly: as demais
    
    beta_k <- start[1:k_length]
    beta_ss_proxy <- start[(k_length + 1):(k_length + proxy_length)]
    beta_ss_controls <- start[-1:-(k_length + proxy_length)]
    
    #omega = phi - beta_k * k
    data_ss <- cbind(data_ss, 
                     res = c(data_ss[, "y"] - beta_fs_matrix[[1]] - data_ss[, colnames(free)] %*% as.matrix(beta_fs_free) - data_ss[, colnames(capital)] %*% as.matrix(beta_k) - data_ss[, colnames(proxy)] %*% as.matrix(beta_ss_proxy) - data_ss[, colnames(controls)] %*% as.matrix(beta_ss_controls)),
                     lag_omega = c(data_ss[, "phi_lag"] - data_ss[, names_capital_lag] %*% as.matrix(beta_k) - data_ss[, names_proxy_lag] %*% as.matrix(beta_ss_proxy) - data_ss[, names_controls_lag] %*% as.matrix(beta_ss_controls))) 
    
    
    omega_qr <- qr(data_ss[, "lag_omega"])
    omega_hat <- qr.fitted(omega_qr, data_ss[, "res"])
    
    data_ss <- as.matrix(data_ss)
    
    
    lp <- c(data_ss[, "res"] - omega_hat)
    
    lp <- colSums(data_ss[, inst_names] * lp, na.rm = TRUE)
    
    return(lp)
  }
  
  
  #n_start = n_capital
  start <- rep(0, (k_length + switch((controls_length > 0) + 1 , 1, controls_length) + proxy_length))
  
  ss_reg <- minpack.lm::nls.lm(par = start, fn = objective, control = minpack.lm::nls.lm.control(maxiter = maxiter))
  
  assign("ss_reg", ss_reg, envir = parent.frame())
}