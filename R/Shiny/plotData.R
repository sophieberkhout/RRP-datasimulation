# Plot the data
library(tidyr)
library(ggplot2)

plotData <- function(data, ylab, glab, tixlab){

  dataPlot <- data %>% gather(Time, Y, -Group) # reshape into long format
  dataPlot$Group <- as.factor(dataPlot$Group)
  dataPlot$Time <- factor(dataPlot$Time, levels = unique(dataPlot$Time))

  p <- ggplot(data = dataPlot,
              aes(x = Time,
                  y = Y,
                  group = Group,
                  colour = Group)) +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                 geom = "pointrange", size = .75, position = position_dodge(width = .1)) +
    stat_summary(fun.y = mean, geom = "line", size = .75, position = position_dodge(width = .1)) +
    theme_minimal() +
    scale_colour_brewer(labels = glab, palette = "Dark2") +
    scale_x_discrete(labels = tixlab) +
    ylab(ylab) +
    theme(axis.title.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.spacing.x = unit(1.0, 'cm'))

  return(p)

}
