% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{cplot}
\alias{cplot}
\title{Convenience function for ggplot2}
\usage{
cplot(..., title = "", x.label = "", y.label = "", fill.label = "",
  base.size = 14)
}
\arguments{
\item{title}{Title to print on the plot (optional)}

\item{x.label}{Label for x axis (optional)}

\item{y.label}{Label for y axis (optional)}

\item{fill.label}{Label for fill legend (optional)}

\item{base.size}{Base font size (optional)}
}
\description{
This provides a quick way to generate a plot with ggplot2 using theme_cody and specifying several labels, as well as the font size, directly in the function call.
}
\examples{
cplot(iris, aes(x=Sepal.Length, y = Sepal.Width),
 title = "Iris Sepal Dimensions",
 x.label = "Length (cm)",
 y.label = "Width (cm)",
 base_size = 12) +
 geom_point()
}
