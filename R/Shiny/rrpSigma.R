#' Create covariance matrix for the RRP mixed designs
#'
#' `rrpSigma` returns a positive-definite covariance matrix for two variables.
#'
#' @param design A character string (can be either `"2x3"` or `"3x2"`)
#'   indicating the research design. `"2x3"` is a design with two independent
#'   and threerepeated measures. `"3x2" has 3 independent and two repeated
#'   measures.
#' @param covWithin A numeric specifying the covariances within the variables.
#' @param covBetween A numeric specifying the covariances between the variables.
#'
#' The course Research and Report Practical (RRP) requires a design cotaining
#'   two variables (one dependent variable and one variable representing a
#'   manipulation check). This function returns the corresponding covariance
#'   matrix.
#'
#' A 12 x 12 matrix postivie definite covariance matrix, of which the first six
#'   rows and columns represent the first variable and the last six rows and
#'   columns represent the second variable.
#'
#' @example
#' Sigma <- rrpSigma(design = "2x3", corWithin = .7, corBetween = .5)

rrpSigma <- function(design, covWithin, covBetween){
  ## Create variance-covariance matrix
  if(design == "2x3"){
    a <- matrix(covWithin, 3, 3)
    diag(a) <- 1
    em <- matrix(0, 3, 3)
    b <- matrix(covBetween, 3, 3)
    Sigma <- cbind(rbind(a, em, b, em), rbind(em, a, em, b),
                   rbind(b, em, a, em), rbind(em, b, em, a))
  } else {
      a <- matrix(covWithin, 2, 2)
      diag(a) <- 1
      em <- matrix(0, 2, 2)
      b <- matrix(covBetween, 2, 2)
      Sigma <- cbind(rbind(a, em, em, b, em, em), rbind(em, a, em, em, b, em),
                     rbind(em, em, a, em, em, b), rbind(b, em, em, a, em, em),
                     rbind(em, b, em, em, a, em), rbind(em, em, b, em, em, a))
  }
  return(Sigma)
}
