createSigma <- function(design){
  ## Create variance-covariance matrix
  if(design == "2x3"){
    s <- matrix(0.7, 3, 3)
    diag(s) <- 1
    em <- matrix(0, 3, 3)
    Sigma <- cbind(rbind(s, em), rbind(em, s)) # 2x3
  } else {
      s <- matrix(0.7, 2, 2)
      diag(s) <- 1
      em <- matrix(0, 2, 2)
      Sigma <- cbind(rbind(s, em, em), rbind(em, s, em), rbind(em, em, s)) # 3x2
  }
  return(Sigma)
}
