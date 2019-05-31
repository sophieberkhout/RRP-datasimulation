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
  theta <- acos(cor)             # corresponding angle
  N <- length(x)
  x2    <- rnorm(N, mean)      # new random data
  X     <- cbind(x, x2)         # matrix
  Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

  Id   <- diag(N)                               # identity matrix
  Q    <- qr.Q(qr(Xctr[, 1, drop = FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  x2o  <- (Id-P) %*% Xctr[, 2]                 # x2ctr made orthogonal to x1ctr
  Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
  Y    <- Xc2 %*% diag(1 / sqrt(colSums(Xc2 ^ 2)))  # scale columns to length 1

  y <- Y[, 2] + (1 / tan(theta)) * Y[, 1]     # final new vector
  cv <- y * 10 + mean
  cv[cv < min | cv > max] <- mean(cv)
  return(cv)
}
