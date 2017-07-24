#' @title Number of \code{poly} elements.
#' @description This function aims calculate the number of terms of a polynomial interactions.
#' @param n The number of variables.
#' @param d Degreess of polynomial interaction.
#' @export
poly_elements <- function (n, d) {
  if (d < 1) return(0)
  component <- sapply(1:d, interactions, n = n)
  list(component = component, ncol = sum(component))
}