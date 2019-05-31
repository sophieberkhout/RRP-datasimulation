#' Generate a vector related to another vector
#'
#' `sampleContRRP` returns a vector.
#'
#' @param x A vector to correlate with the new vector.
#' @param cor A numeric indicating the correlation between the vectors.
#' @param mean A numeric indicating the mean of the new vector.
#' @param min A numeric indicating the allowed minimum value for the new vector.
#' @param max A numeric indicating the maximum allowed value for the new vector.
#'
#' This function creates a generated vector which is correlated to another given
#'   vector.
#'
#'  A generated vector with a given mean, respecting minimum and maximum values.
#'
#' @example
#' x <- rnorm(100)
#' sampleContRRP(x = x, cor = .7, mean = 3, min = 1, max = 6)

sampleContRRP <- function(x, cor, mean, min, max){
  n <- length(x)
  m <- cbind(x, rnorm(n, mean))
  mc <- scale(m, center=TRUE, scale=FALSE)    # center columns
  q <- qr.Q(qr(mc[, 1, drop = FALSE]))        # QR-decomposition, just matrix Q
  p <- tcrossprod(q)                          # projection onto space defined by x1
  mo <- (diag(n)-p) %*% mc[, 2]       # x2ctr made orthogonal to x1ctr
  X <- cbind(mc[, 1], mo)
  Y <- X %*% diag(1 / sqrt(colSums(X ^ 2)))   # scale columns to length 1
  y <- Y[, 2] + (1 / tan(acos(cor))) * Y[, 1] # new vector with mean zero
  cv <- y * 10 + mean
  cv[cv < min | cv > max] <- mean(cv)
  return(cv)
}
