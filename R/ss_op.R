ss_op <- function(y, free, capital, proxy, controls, id, time, phi, myprobit_lag, degree, k_length, proxy_length, controls_length, beta_fs_matrix, beta_fs_free, beta_fs_controls, maxiter, ...){
  
  #-----------------------------------------------------
  #Segundo estágio (ss)
  #-----------------------------------------------------

  capital_lag = panel_lag(capital, id, time, verify = FALSE)
  
  names_capital_lag <- paste0("lag_", colnames(capital_lag))
  
  colnames(capital_lag) <- names_capital_lag
  
  df_ss <- data.frame(y_ss = c(y - free %*% beta_fs_free - controls %*% beta_fs_controls - beta_fs_matrix[1]),
                      free,
                      capital,
                      capital_lag,
                      phi = c(phi),
                      phi_lag = c(panel_lag(phi, id, time, verify = FALSE)),
                      controls)
  
  df_ss <- df_ss[complete.cases(df_ss),]
  
  #rm(phi, capital_lag)

  #objective function = sum of residuals ^2
  objective <- function(start, data_ss = df_ss) {
    
    data_ss <- as.matrix(data_ss)
    
    #constante: primeira variável. beta_k: (1 + 1) até o numero de k + 1  
    #beta_poly: as demais
    beta_const <- start[1]
    beta_k <- start[2:(k_length + 1)]
    beta_poly <- start[-(1:(k_length + 1))]
    
    
    poly_df <- cbind(omega = data_ss[, "phi_lag"] - data_ss[, names_capital_lag] %*% as.matrix(beta_k), myprobit_lag)
    
    poly_df <- poly(poly_df, degree = degree[2], raw = TRUE)

    poly_df <- poly_df %*% diag(beta_poly, nrow = length(beta_poly))
    
    op <- qr(cbind(const = 1 * beta_const, poly_df))
    
    op <- qr.resid(op, data_ss[, "y_ss"] - data_ss[, colnames(capital)] %*% as.matrix(beta_k))
    
    return(op)

  }
  
  #n_start = n_capital + n_const + n_interactions
  with_exit <- ifelse(!is.null(myprobit_lag), 1, 0)
  n_start <- k_length + 1 + poly_elements(k_length + with_exit, d = degree[2])[[2]]
  
  start <- rep(0, n_start)
  
  
  ss_reg <- minpack.lm::nls.lm(par = start, fn = objective, control = minpack.lm::nls.lm.control(maxiter = maxiter))
 
  assign("ss_reg", ss_reg, envir = parent.frame())
  
}
