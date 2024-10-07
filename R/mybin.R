#' @title mybin function
#'
#' @param iter the iterator
#' @param n number of times it is tested
#' @param p the probability
#'
#' @return a table of relative frequencies
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve polygon
#'
#' @export
#'
#' @examples mybin(iter=10000,n=100, p=0.5)
mybin=function(iter=100, n=10, p=0.5){
  # Generate a matrix of samples, with rows = n and columns = iter
  sam.mat = matrix(sample(c(1, 0), n * iter, replace = TRUE, prob = c(p, 1 - p)), nrow = n)

  # Sum the number of successes (1s) for each iteration (column-wise)
  succ = colSums(sam.mat)

  # Create a table of frequencies for the number of successes
  succ.tab = table(factor(succ, levels = 0:n))

  # Plot the results
  barplot(succ.tab / iter, col = rainbow(n + 1), main = "Binomial simulation", xlab = "Number of successes")

  # Return the table of relative frequencies
  return(succ.tab / iter)
}
