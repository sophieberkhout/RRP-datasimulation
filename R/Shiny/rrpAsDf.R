#' Create a data frame with a group variable and named columns
#'
#' `rrpAsDf` returns a data frame.
#'
#' @param dat A matrix with 12 columns.
#' @param N The number of observations per group.
#' @param min A numeric indicating the allowed minimum value for the data frame.
#' @param max A numeric indicating the maximum allowed value for the data frame.
#' @param design A character string (can be either `"2x3"` or `"3x2"`)
#'   indicating the research design. `"2x3"` is a design with two independent
#'   and threerepeated measures. `"3x2" has 3 independent and two repeated
#'   measures.
#' @param names A character vector representing the names of the variables.
#'
#' This function creates a data frame out of a matrix. It is designed to
#'   create a data frame from a matrix sampled by `rrSample` and to use as
#'   input for `plotDataRRP`.
#'
#'  A data frame respecting minimum and maximum values and including a group
#'   variable and column names.
#'
#' @example
#' \dontrun{
#'   rrpAsDf(dat = data, N = 100, min = 0, max = 6, design = "2x3",
#'     names = c("Time 1", "Time 2", "Time 3"))
#' }

rrpAsDf <- function(dat, N, min, max, design, names){
  dat[dat < min | dat > max] <- mean(dat)
  if(design == "2x3"){
    dat <- as.data.frame(rbind(dat[, 1:3], dat[, 4:6]))
    names(dat) <- names
    dat$group <- rep(c(1, 2), each = N)
  } else {
    dat <- as.data.frame(rbind(dat[, 1:2], dat[, 3:4], dat[, 5:6]))
    names(dat) <- names[-3]
    dat$group <- rep(c(1, 2, 3), each = N)
  }
  return(dat)
}



