# Plot the data
library(tidyr)
library(ggplot2)

plotData <- function(data, ylab, glab, tixlab){

  dataPlot <- data %>% gather(Time, Y, -Group, -Gender) # reshape into long format
  dataPlot$Group <- as.factor(dataPlot$Group)
  dataPlot$Time <- factor(dataPlot$Time, levels = unique(dataPlot$Time))

  p <- ggplot(data = dataPlot,
              aes(x = Time,
                  y = Y,
                  group = Group,
                  colour = Group)) +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange") +
    stat_summary(fun.y = mean, geom = "line") +
    theme_classic() +
    scale_colour_grey(labels = glab) +
    scale_x_discrete(labels = tixlab) +
    ylab(ylab) +
    theme(axis.title.x = element_blank())

  return(p)

}
