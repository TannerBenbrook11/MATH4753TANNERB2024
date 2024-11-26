#' myci function
#'
#' @param x sample
#'
#' @return a confidence interval
#' @export
#'
#' @importFrom stats qt sd
#'
#' @examples myci(rnorm(30, mean = 10, sd = 12))
myci <- function(x) {
  # Calculate the sample mean
  sample_mean <- mean(x)

  # Calculate the standard error of the mean
  standard_error <- sd(x) / sqrt(length(x))

  # Determine the critical value for a 95% confidence interval
  critical_value <- qt(0.975, df = length(x) - 1)  # 0.975 for a two-tailed test

  # Calculate the margin of error
  margin_of_error <- critical_value * standard_error

  # Calculate the confidence interval
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error

  # Return the confidence interval as a vector
  return(c(lower_bound, upper_bound))
}

# Example usage:
# x <- c(10, 12, 15, 9, 13, 14, 11)
# myci(x)
