source(file = "R/mySample.R")

dataRRP <- function(N, means, min, max, Sigma){

    data <- mySample(N/2, means, min, max, Sigma)
    data <- as.data.frame(rbind(data[, -(4:6)], data[, 4:6]))
    data <- as.data.frame(rbind(test[, 1:2], test[, 3:4], test[, 5:6]))
    data$Group <- rep(c(0, 1), each = N/2)
    return(data)

}

# Input
## Create variance-covariance matrix
s <- matrix(0.7, 3, 3)
diag(s) <- 1
em <- matrix(0, 3, 3)
Sigma <- cbind(rbind(s, em), rbind(em, s)) # 2x3

s <- matrix(0.7, 2, 2)
diag(s) <- 1
em <- matrix(0, 2, 2)
Sigma <- cbind(rbind(s, em, em), rbind(em, s, em), rbind(em, em, s)) # 3x2

## Sample size and mean values
N <- 100
means <- c(20, 20, 20, 20, 25, 29) # 2x3
means <- c(20, 20, 20, 25, 20, 30) # 3x2
min <- 15
max <- 35

data <- dataRRP(N, means, min, max, Sigma)
head(data)

## Other variables
data$Gender <- sample(c(1, 2), 100, replace = T)
data$Age <- rnorm(100, 18) # truncated?


# Plot the data
library(tidyr)
library(ggplot2)
dataPlot <- data %>% gather(Time, Y, -Group) # reshape into long format
dataPlot$Group <- as.factor(dataPlot$Group)

ggplot(data = dataPlot,
     aes(x = Time,
         y = Y,
         group = Group,
         colour = Group)) +
stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
             geom="pointrange") +
stat_summary(fun.y = mean, geom = "line") +
theme_classic() +
scale_color_grey()


