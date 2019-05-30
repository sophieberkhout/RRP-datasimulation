createSigma <- function(design){
  ## Create variance-covariance matrix
  if(design == "2x3"){
    sD <- matrix(0.7, 3, 3)
    diag(sD) <- 1
    em <- matrix(0, 3, 3)
    SigmaD <- cbind(rbind(sD, em), rbind(em, sD)) # 2x3
    sM <- matrix(0.5, 3, 3)
    SigmaM <- cbind(rbind(sM, em), rbind(em, sM))
    Sigma <- cbind(rbind(SigmaD, SigmaM), rbind(SigmaM, SigmaD))
  } else {
      sD <- matrix(0.7, 2, 2)
      diag(sD) <- 1
      em <- matrix(0, 2, 2)
      SigmaD <- cbind(rbind(sD, em, em), rbind(em, sD, em), rbind(em, em, sD)) # 3x2
      sM <- matrix(0.5, 2, 2)
      SigmaM <- cbind(rbind(sM, em, em), rbind(em, sM, em), rbind(em, em, sM))
      Sigma <- cbind(rbind(SigmaD, SigmaM), rbind(SigmaM, SigmaD))
  }
  return(Sigma)
}
