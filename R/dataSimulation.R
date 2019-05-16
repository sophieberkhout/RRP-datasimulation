dataRRP <- function(N, means, Sigma){

    data <- MASS::mvrnorm(N, means, Sigma)
    data <- as.data.frame(rbind(data[, -(4:6)], data[, 4:6]))
    data$Group <- rep(c(0, 1), each = N)
    return(data)

}

# Input
## Create variance-covariance matrix
Sigma <- matrix(0.7, 6, 6) # empty matrix
diag(Sigma) <- 1 # all variances 1
Sigma[4:6, 1:3] <- 0
Sigma[upper.tri(Sigma)] <- rev(Sigma[lower.tri(Sigma)])

## Sample size and mean values
N <- 10
means <- c(20, 20, 20, 20, 25, 30)

data <- dataRRP(N, means, Sigma)
head(data)

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


