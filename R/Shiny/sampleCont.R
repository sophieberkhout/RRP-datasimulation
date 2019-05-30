sampleCont <- function(N, x, rho, mean, min, max){
  r   <- rho                   # desired correlation = cos(angle)
  theta <- acos(r)             # corresponding angle

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
