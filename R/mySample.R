mySample <- function(N, mu, Sigma){
  rm <- length(mu)
  if (!all(dim(Sigma) == c(rm, rm))) 
    stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -1e-06 * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(rm * N), N)
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), rm) %*% t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (N == 1) 
    drop(X)
  else t(X)
}
