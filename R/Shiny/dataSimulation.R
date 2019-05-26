source(file = "mySample.R")

dataRRP <- function(dat, N, design, gender, age, minAge, maxAge, names){
  if(design == "2x3"){
    dat <- as.data.frame(rbind(dat[, 1:3], dat[, 4:6]))
    names(dat) <- names
    dat$Group <- rep(c(1, 2), each = N)
  } else {
    dat <- as.data.frame(rbind(dat[, 1:2], dat[, 3:4], dat[, 5:6]))
    names(dat) <- names[-3]
    dat$Group <- rep(c(1, 2, 3), each = N)
  }

  dat$Gender <- sample(1:2, N, replace = T, prob = c(gender/N, (1-gender/N)))
  dat$Age <- rnorm(N, age)
  if(is.na(minAge) & is.na(maxAge)){
    dat$Age[dat$Age < minAge | dat$Age > maxAge] <- mean(dat$Age)
  }

  return(dat)
 # return(dat[sample(nrow(dat)), ])
 # data$Age <- rnorm(100, 18) # truncated?
 # data$Cat <- sample(1:3, 100, replace = T, prob = c(2/4, 1/4, 1/4))
 # data$Con

}



