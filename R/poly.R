#' polynomial generator
#'
#' @param degree degree of polynomial
#' @param coefficients your given coefficients
#' @param x the given x value you want to evaluate the polynomial at
#'
#' @return a polynomial
#' @export
#'
#' @examples \dontrun{poly(5, [1, 2, 3, 4, 5, 6], x)}
poly <- function(degree, coefficients, x) {
  # degree: the degree of the polynomial
  # coefficients: a vector of coefficients
  # x: the value(s) for which to evaluate the polynomial

  polynomial <- sum(coefficients * (x ^ (0:degree)))
  return(polynomial)
}
