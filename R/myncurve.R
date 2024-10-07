#' @title myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a upper limit
#'
#' @importFrom graphics abline barplot curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @return the calculated mean, sd and area in a list
#' @export
#'
#' @examples myncurve(mu = 10, sigma = 5, a = 6)
myncurve <- function(mu, sigma, a) {
  # Plot the normal curve
  curve(dnorm(x = x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = expression(paste("Normal Curve: ", mu, "=", mu, ", ", sigma, "=", sigma)),
        ylab = "Density",
        xlab = "X")

  # Define the x values for the shaded area
  x_values <- seq(mu - 3 * sigma, a, length = 1000)

  # Calculate y values for the shaded area
  y_values <- dnorm(x_values, mean = mu, sd = sigma)

  # Shade the area under the curve from -∞ to a
  polygon(c(mu - 3 * sigma, x_values, a),
          c(0, y_values, 0),
          col = "lightblue",
          border = NA)

  # Calculate the area (probability)
  area <- pnorm(a, mean = mu, sd = sigma)

  # Return mean, sigma, and calculated area in a list
  list(mu = mu, sigma = sigma, area = area)
}
