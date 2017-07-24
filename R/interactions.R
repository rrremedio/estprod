#' @title Total number of \code{poly} elements.
#' @description This function aims calculates the total number of terms of a polynomial interactions.
#' @param n The number of variables.
#' @param d Degreess of the polynomial interaction.
#' @export
interactions <- function (n, d) {
  k <- choose(n, d)
  if (d > 1) k <- k + n * sum(choose(n-1, 0:(d-2)))
  return(k)
}