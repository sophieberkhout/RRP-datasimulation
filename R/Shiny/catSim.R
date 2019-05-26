catSim <- function(lvl, N, p){
  cat <- sample(1:lvl, N, replace = T, prob = p)
}

conSim <- function(dat, a, N){
  y <- rowMeans(dat) + a + rnorm(N)
}
