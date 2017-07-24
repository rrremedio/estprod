#' @title Panel data lag function
#' @description This function aims create the lags of a specified variable from panel data.
#' @param x A vector, data.frame, tibble or matrix.
#' @param id A character with the name of the indicator variable.
#' @param time A character with the name of the time variable.
#' @param lag Number of lags.
#' @param verify Check if panel is sorted by id and time variables.
#' @note Based on \href{https://bitbucket.org/paulschrimpf/}{Paul Schrimpf's} lag function.
#' @export
#' 
panel_lag  <-  function(x, id, time, lag = 1, verify = TRUE) {
  
  #se for tibble preciso converter elementos em vetores
  if (any(class(x) == "tbl")) {
    x <- as.data.frame(x)
  }
  if (!identical(order(id, time), 1:length(id)) & verify) {
    stop("inputs not sorted.")
  }
  if (is.matrix(x)) {
    return(apply(x, MARGIN=2, FUN=function(c) { panel_lag(c, id, time, lag, verify) }))
  }
  if (length(id) != length(x) || length(id) != length(time) ) {
    stop("Inputs not same length")
  }
  if (lag>0) {
    x.lag  <-  x[1:(length(x) - lag)]
    x.lag[id[1:(length(id) - lag)]!=id[(1 + lag):length(id)] ]  <-  NA
    x.lag[time[1:(length(id) - lag) + lag] !=time[(1 + lag):length(id)] ]  <-  NA
    return(c(rep(NA, lag), x.lag))
  } else if (lag<0) {
    lag  <-  abs(lag)
    x.lag  <-  x[(1 + lag):length(x)]
    x.lag[id[1:(length(id) - lag)]!=id[(1 + lag):length(id)] ]  <-  NA
    x.lag[time[1:(length(id) - lag) + lag]!=time[(1 + lag):length(id)] ]  <-  NA
    return(c(x.lag, rep(NA, lag)))
  } else { # lag=0
    return (x)
  }
}