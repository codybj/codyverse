library(tidyverse)
library(ggplot2)

#' Standard Error of the Mean
#'
#'This function returns the standard error of the mean (SEM) of a given numeric vector.The SEM defined as the stadard deviation (SD) divided by the square root of the count of observations. In other words, SD(x)/sqrt(n).
#'
#' @param x Array of which you want to calculate the SEM.
#'
#' @return Returns a numeric SEM.
#' @export
#'
#' @examples
#' sem(c(3, 3, 3.5, 4))
#' sem(iris$Sepal.Length)
sem <- function(x) {
  sd(x) / sqrt(length(x))
}


#' Cody's preferred theme for ggplot2
#'
#'This builds upon theme_bw from ggplot2 to make figures look the way I prefer. Inspect the source for more info on this.
#'
#' @export
#'
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width)) +
#' geom_point() +
#' theme_cody()
theme_cody <- function(base.size = 14) {
  theme_bw(base_size = base.size) %+replace% # base_family = "Avenir" was messing up PDF output
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(size = 0.75, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.75, linetype = "solid", colour = "black"),
      axis.text = element_text(colour = "black")
    )
}

#' Convenience function for ggplot2
#'
#' This provides a quick way to generate a plot with ggplot2 using theme_cody and specifying several labels, as well as the font size, directly in the function call.
#'
#' @param ...
#' @param title Title to print on the plot (optional)
#' @param x.label Label for x axis (optional)
#' @param y.label Label for y axis (optional)
#' @param fill.label Label for fill legend (optional)
#' @param base.size Base font size (optional)
#'
#' @export
#'
#' @examples
#' cplot(iris, aes(x=Sepal.Length, y = Sepal.Width),
#'  title = "Iris Sepal Dimensions",
#'  x.label = "Length (cm)",
#'  y.label = "Width (cm)",
#'  base_size = 12) +
#'  geom_point()

cplot <- function(..., title = "", x.label = "", y.label = "", fill.label = "", base.size = 14) {
  ggplot(...) +
    theme_cody(base.size = base.size) +
    labs(title = title, x = x.label, y = y.label, fill = fill.label) +
    scale_y_continuous(expand = c(0, 0))
}

#' Cody's preferred geom_col
#'
#' This function is a wrapper for ggplot2::geom_col. It specifies some stylistic elements so they don't need to be typed out each time. It is meant to be used with geom_cerrorbar.
#'
#' @param ...
#' @param bar_width Width of the column. The same width should be provided to geom_cerrorbar, if used, so that dodging is aligned.
#' @param position see ggplot2::geom_col
#' @param color see ggplot2::geom_col
#' @param size see ggplot2::geom_col
#'
#' @return
#' @export
#'
#' @examples
geom_ccol <- function(..., bar_width = 0.6, position = position_dodge(), color = "black", size = 0.75) {
  geom_col(..., position = position, color = color, size = size, width = bar_width)
}

#' Cody's preferred geom_errorbar
#'
#' This calls ggplot2::geom_errorbar with some shortcuts for use with the other codyverse functions.
#'
#' @param ...
#' @param bar_width Width of the geom that cerrorbar will be added to.
#' @param position see ggplot2::geom_errorbar
#' @param color see ggplot2::geom_errorbar
#' @param width By default, 0.5*bar_width.
#' @param size see ggplot2::geom_errorbar
#'
#' @return
#' @export
#'
#' @examples
geom_cerrorbar <- function(..., bar_width = 0.6, position = position_dodge(width = bar_width), color = "black", width = bar_width * 0.5, size = 0.75) {
  geom_errorbar(..., position = position, color = color, width = width, size = size)
}

#' Pretty log 10
#'
#' Calls ggplot2::scale_y_log10 with labels for "pretty" exponents. Recommend to also add annotation_logticks(sides = "l")
#'
#' @param ... Any arguments to pass to ggplot2::scale_y_log10
#'
#' @return
#' @export
#'
#' @examples
scale_y_prettylog10 <- function(...) {
  scale_y_log10(..., labels = scales::trans_format("log10", scales::math_format(10^.x)))
}

#' Mean and SEM Calculator
#'
#' This gives a dataframe containing y, ymin, and ymax values corresponding to the mean, mean-SEM, and mean+SEM of the input data. It can be called as the fun.data from ggplot2::stat_summary
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mean_sem <- function (x)
{
  x <- stats::na.omit(x)
  mean <- mean(x)
  sem <- sd(x)/sqrt(length(x))
  data.frame(list(y = mean, ymin = mean - sem, ymax = mean +
                    sem))
}


#' Assign stars based on p-values.
#'
#' This gives a string with stars corresponding to conventional alpha values (0.001, 0.01, 0.05) based on the value of the input.
#'
#' @param pval a number
#'
#' @return
#' @export
#'
#' @examples
assign_stars <- function(pval){
  if (pval <= 0.001) "***"
  else if (pval <= 0.01) "**"
  else if (pval <= 0.05) "*"
  else ""
}