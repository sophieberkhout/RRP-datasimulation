simulateData <- function(N, meansInt){

    n <- N/2 # Samplze size per group
    meansCon <- rep(meansInt[1], 3) # Means for control group are equal to the first mean of meansInt

    # Create empty data frame
    data <- data.frame(Group = character(N),
                       Y1 = numeric(N),
                       Y2 = numeric(N),
                       Y3 = numeric(N))

    data$Group <- rep(c("Intervention", "Control"), each = n) # Add grouping variable

    # Create variance-covariance matrix
    Sigma <- matrix(.7, 3, 3) # all covariances .7
    diag(Sigma) <- 1 # all variances 1

    # Simulate data from multivariate normal distribution per group
    data[1:n, c("Y1", "Y2", "Y3")] <- MASS::mvrnorm(n, meansInt, Sigma) # Intervention group
    data[(n+1):N, c("Y1", "Y2", "Y3")] <- MASS::mvrnorm(n, meansCon, Sigma) # Control group

    # Shuffle data frame
    data <- data[sample(nrow(data)), ]
    # Add ID's
    data <<- cbind(ID = 1:N, data)

}

simulateData(N = 100, meansInt = c(30, 27, 25))
head(data)

# Plot the data
dataPlot <- data %>% tidyr::gather(Time, Y, c(-ID, -Group)) # reshape into long format

ggplot2::ggplot(data = dataPlot,
               aes(x = Time,
                   y = Y,
                   group = Group,
                   colour = Group)) +
          stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                       geom="pointrange") +
          stat_summary(fun.y = mean, geom = "line") +
          theme_classic() +
          scale_color_grey()


