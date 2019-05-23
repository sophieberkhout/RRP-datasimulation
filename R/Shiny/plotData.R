# Plot the data
library(tidyr)
library(ggplot2)

plotData <- function(data, ylab = "Y"){

  dataPlot <- data %>% gather(Time, Y, -Group, -Gender) # reshape into long format
  dataPlot$Group <- as.factor(dataPlot$Group)

  p <- ggplot(data = dataPlot,
              aes(x = Time,
                  y = Y,
                  group = Group,
                  colour = Group)) +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange") +
    stat_summary(fun.y = mean, geom = "line") +
    theme_classic() +
    scale_color_grey() +
    ylab(ylab)

  return(p)

}
