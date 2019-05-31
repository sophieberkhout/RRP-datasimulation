#' Plot means for the RRP mixed designs.
#'
#' `plotDataRRP` returns a ggplot showing the means per group and per time
#'   measurement.
#'
#' @param data A wide format data frame.
#' @param ylab A character string specifying the title of the y-axis.
#' @param glab A character vector giving the group names for the legend.
#' @param tixlab A character vector giving the labels for each time point.
#'
#' This plot illustrates a summary of the simulated data.
#'
#' @example
#' \dontrun{
#' plotDataRRP(data = dat, ylab = "Y", glab = c("Group 1", "Group 2"),
#'   tixlab = c("Time 1", "Time 2", "Time 3"))
#' }

library(tidyr)
library(ggplot2)
library(Hmisc)

plotDataRRP <- function(data, ylab, glab, tixlab){
  dataPlot <- data %>% gather(Time, Y, -group) # reshape into long format
  dataPlot$group <- as.factor(dataPlot$group)
  dataPlot$Time <- factor(dataPlot$Time, levels = unique(dataPlot$Time))
  p <- ggplot(data = dataPlot,
              aes(x = Time,
                  y = Y,
                  group = group,
                  colour = group)) +
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
          legend.spacing.x = unit(1.0, 'cm'),
          panel.grid.major.x = element_blank())
  return(p)
}
