#' Sample from a mulitivariate normal distribution
#'
#' `rrpSample()` returns a matrix of sampled data
#'
#' @param N Number of observations.
#' @param mu A vector with the means of each variable.
#' @param Sigma A positive-definite covariance matrix of the variables.
#'
#' Stable matrix decomposition via `eigen()`.
#'
#' An `N` by `length(mu)` matrix with its rows representing one observation.
#'   If `N` is equal to 1, a vector of `length(mu)` is returned.
#'
#' @example
#' Sigma <- matrix(c(0, 1, 1, 0), 2, 2)
#' m <- rrSample(N = 100, mu = c(0, 10), Sigma = Sigma)

rrpSample <- function(N, mu, Sigma){
  nV <- length(mu)
   if (!all(dim(Sigma) == c(nV, nV)))
     stop("incompatible arguments")
  eigenSigma <- eigen(Sigma, symmetric = TRUE)
  eigenValues <- eigenSigma$values
  if (!all(eigenValues >= -1e-06 * abs(eigenValues[1L])))
    stop("'Sigma' is not positive definite")
  m <- matrix(rnorm(nV * N), N)
  m <- drop(mu) + eigenSigma$vectors %*% diag(sqrt(pmax(eigenValues, 0)), nV) %*% t(m)
  if (N == 1)
    m <- drop(m)
  else m <- t(m)
  return(m)
}


