mySample <- function(N, mu, Sigma){
  rm <- length(mu)
   if (!all(dim(Sigma) == c(rm, rm)))
     stop("incompatible arguments")
  eigenSigma <- eigen(Sigma, symmetric = TRUE)
  eigenValues <- eigenSigma$values
  if (!all(eigenValues >= -1e-06 * abs(eigenValues[1L])))
    stop("'Sigma' is not positive definite")
  m <- matrix(rnorm(rm * N), N)
  m <- drop(mu) + eigenSigma$vectors %*% diag(sqrt(pmax(eigenValues, 0)), rm) %*% t(m)
  if (N == 1)
    m <- drop(m)
  else m <- t(m)
  return(m)
}


