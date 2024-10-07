#' ntickets
#'
#' @param N number of seats
#' @param gamma the probability that a plane will be truly overbooked
#' @param p probability that someone that got a ticket will show up
#'
#' @importFrom stats dbinom qnorm
#'
#' @return the number of tickets
#' @export
#'
#' @examples ntickets(N = 200, gamma = 0.02, p = 0.95)

# ntickets function now only calls find_nd_discrete and find_nc_normal
ntickets <- function(N, gamma, p) {

  find_nd_discrete <- function(N, gamma, p) {
    n <- N
    while (TRUE) {
      prob <- sum(dbinom(0:N, n, p))
      if (1 - prob <= gamma) {
        return(n)
      }

      n <- n + 1  # Keep increasing n if condition is not met
    }

    return(NULL)
  }

  find_nc_normal <- function(N, gamma, p) {
    mean <- N * p
    stddev <- sqrt(N * p * (1 - p))
    return(qnorm(1 - gamma, mean, stddev))
  }


  plot_objective <- function(N, gamma, p, is_discrete = TRUE) {
    n_values <- seq(N, N + 20)  # Increase the resolution by using smaller steps for n
    objective_values <- numeric(length(n_values))

    if (is_discrete) {
      # Discrete case logic (same as before)
      for (i in seq_along(n_values)) {
        prob <- sum(dbinom(0:N, floor(n_values[i]), p))  # Binomial probability
        objective_values[i] <- 1 - prob  # The objective is to get close to gamma
      }

      diff_values <- abs(objective_values - gamma)
      n_intersect <- n_values[which.min(diff_values)]

      plot(n_values, objective_values, type = "b", main = paste("Objective Vs n for Discrete Case\n",
                                                                "Optimal nd: ", n_intersect, "\ngamma=", gamma, " N=", N, sep=""),
           ylab = "Objective (1 - P(X <= N))", xlab = "n")
      abline(h = gamma, col = "red", lwd = 2)
      abline(v = n_intersect, col = "red", lwd = 2)

    } else {
      # Continuous case logic for normal approximation
      for (i in seq_along(n_values)) {
        mean <- n_values[i] * p
        stddev <- sqrt(n_values[i] * p * (1 - p))
        prob <- pnorm(N, mean = mean, sd = stddev)  # Normal approximation
        objective_values[i] <- 1 - prob  # The objective is to get close to gamma
      }

      diff_values <- abs(objective_values - gamma)  # Find the difference between objective and gamma
      n_intersect <- n_values[which.min(diff_values)]  # Get n where objective is closest to gamma

      plot(n_values, objective_values, type = "l", main = paste("Objective Vs n for Normal Approximation\n",
                                                                "Optimal nc: ", n_intersect, "\ngamma=", gamma, " N=", N, sep=""),
           ylab = "Objective (1 - P(X <= N))", xlab = "n")

      abline(h = gamma, col = "blue", lwd = 2)  # Horizontal line at gamma
      abline(v = n_intersect, col = "blue", lwd = 2)  # Vertical line at the intersection

    }
    return(n_intersect)
    ntickets(N = 200, gamma = 0.02, p = 0.95)
    plot_objective(N = 200, gamma = 0.02, p = 0.95, is_discrete = TRUE)
    plot_objective(N = 200, gamma = 0.02, p = 0.95, is_discrete = FALSE)


  nd <- find_nd_discrete(N, gamma, p)
  nc <- find_nc_normal(N, gamma, p)

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)

  # Return the named list
  return(result)

  ntickets(N = 200, gamma = 0.02, p = 0.95)
  plot_objective(N = 200, gamma = 0.02, p = 0.95, is_discrete = TRUE)
  plot_objective(N = 200, gamma = 0.02, p = 0.95, is_discrete = FALSE)

  }
}
