% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_image.R
\name{save_image}
\alias{save_image}
\title{Save Plot as Image}
\usage{
save_image(
  title,
  plott = last_plot(),
  folder = NULL,
  fig.pathinside = fig.path
)
}
\arguments{
\item{title}{A character string representing the title of the plot.}

\item{plott}{The plot object to save.}

\item{folder}{The folder where the image will be saved.}

\item{fig.pathinside}{The path for saving the figure.}
}
\value{
None
}
\description{
This function saves the current plot as an image in a specified folder.
}
