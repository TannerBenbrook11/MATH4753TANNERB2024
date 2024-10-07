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
  find_nc_distribution <- function(N, gamma, p) {
    mean <- N * p
    stddev <- sqrt(N * p * (1 - p))
    return(qnorm(1 - gamma, mean, stddev))
  }

  find_nd_distribution <- function(N, gamma, p) {
    n <- N
    while (TRUE) {
      prob <- sum(dbinom(0:N, n, p))
      if (1 - prob <= gamma) {
        return(n)
      }
      n <- n + 1
    }
    return(NULL)
  }

  plot_objective <- function(N, gamma, p, is_discrete = TRUE) {
    values <- seq(N, N + 20)
    obj_values <- numeric(length(values))

    if (is_discrete) {
      for (i in seq_along(values)) {
        prob <- sum(dbinom(0:N, floor(values[i]), p))
        obj_values[i] <- 1 - prob
      }

      difference <- abs(obj_values - gamma)
      n_intersect <- values[which.min(difference)]

      plot(values, obj_values, type = "b", main = paste("Objective Vs n for Discrete Case\n",
                                                    "Optimal nd: ", n_intersect, "\ngamma=", gamma, " N=", N, sep=""),
           ylab = "Objective (1 - P(X <= N))", xlab = "n")
      abline(h = gamma, col = "red", lwd = 2)
      abline(v = n_intersect, col = "red", lwd = 2)

    } else {
      for (i in seq_along(values)) {
        mean <- values[i] * p
        stddev <- sqrt(values[i] * p * (1 - p))
        prob <- pnorm(N, mean = mean, sd = stddev)
        obj_values[i] <- 1 - prob
      }

      difference <- abs(obj_values - gamma)
      n_intersect <- values[which.min(difference)]

      plot(values, obj_values, type = "l", main = paste("Objective Vs n for Normal Approximation\n",
                                                    "Optimal nc: ", n_intersect, "\ngamma=", gamma, " N=", N, sep=""),
           ylab = "Objective (1 - P(X <= N))", xlab = "n")

      abline(h = gamma, col = "blue", lwd = 2)
      abline(v = n_intersect, col = "blue", lwd = 2)

    }
    return(n_intersect)
  }

  nd <- plot_objective(N, gamma, p, is_discrete = TRUE)
  nc <- plot_objective(N, gamma, p, is_discrete = FALSE)

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  return(result)
}


