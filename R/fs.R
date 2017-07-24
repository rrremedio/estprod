if(getRversion() >= "2.15.1")  utils::globalVariables(c("phi"))
fs <- function(data, y, free, capital, proxy, controls, formula, exit, degree, id, time){
  
  # for (i in 1:length(prev_stage)){
  #   assign(names(prev_stage[i]), prev_stage[[i]])
  # }
  
  #-----------------------------------------------------
  #Primeiro estagio (fs): b=(X1'X1)^(-1)X1'Y
  #-----------------------------------------------------
  polyvars <- cbind(poly(cbind(capital, proxy), degree = degree[1],  raw = TRUE))
  
  x_fs_qr <- qr(cbind(const = 1, free, polyvars, controls))
  
  beta_fs_matrix <- qr.coef(x_fs_qr, y)
  
  #beta_fs_free <- beta_fs_matrix[2:(2 + free_length - 1)]
  beta_fs_free <- beta_fs_matrix[colnames(free),]
  
  if (length(formula)[2] == 4){
    beta_fs_controls <- beta_fs_matrix[colnames(controls),]
    names(beta_fs_controls) <- colnames(controls)
  } else {
    beta_fs_controls <- 0
  }
  
  
  predict_fs_matrix <- qr.fitted(x_fs_qr, y)
  
  residuals_fs_matrix <- qr.resid(x_fs_qr, y)
  
  
  phi <- predict_fs_matrix - free %*% beta_fs_free - beta_fs_matrix[[1]] - controls %*% beta_fs_controls
  
  #-----------------------------------------------------
  #Probit
  #-----------------------------------------------------
  #se a variável exit_fs foi definida, gere valores de probit
  if (!is.null(exit)){
    exit_formula <- formula(exit)
    exit <- as.matrix(model.frame(exit_formula, data = data))
    colnames(exit) <- labels(terms(formula(exit_formula)))
    
    if (dim(exit)[2] > 1){
      stop("There must be only one exit variable.")
    }
  }  
  
  
  
  if (!is.null(exit)){
    if (min(exit, na.rm = TRUE) < 0 | max(exit, na.rm = TRUE) > 1){
      stop("exit values must be 0 <= exit <= 1")
    } else if (min(exit, na.rm = TRUE) >= 0 & max(exit, na.rm = TRUE) <= 1){
      
      probit_df <- cbind(polyvars, controls)

      probit_df <- panel_lag(probit_df, id = id, time = time, lag = 1, verify = F)
      
      probit_df <- cbind(probit_df, exit)
      
      probit_df <- as.data.frame(probit_df[complete.cases(probit_df),])
      
      myprobit_lag <- suppressWarnings(glm(exit ~ . + 1, family = binomial(link = "probit"), data = probit_df, maxit = 100))
      
      myprobit_lag <- suppressWarnings(predict(myprobit_lag, type = "response"))
      
    }
    
  } else {
    
    myprobit_lag <- NULL
    
  }
  
  assign("phi", phi, envir = parent.frame())
  assign("beta_fs_free", beta_fs_free, envir = parent.frame())
  assign("beta_fs_controls", beta_fs_controls, envir = parent.frame())
  assign("beta_fs_matrix", beta_fs_matrix, envir = parent.frame())
  assign("myprobit_lag", myprobit_lag, envir = parent.frame())
  assign("residuals_fs_matrix", residuals_fs_matrix, envir = parent.frame())
  
}
