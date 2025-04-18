% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_coverage_analysis_dygraphs.R
\name{time_coverage_analysis_dygraphs}
\alias{time_coverage_analysis_dygraphs}
\title{Calculate and Visualize Time Coverage Using dygraphs}
\usage{
time_coverage_analysis_dygraphs(
  time_dimension_list_groupped,
  parameter_time_dimension,
  titre_1,
  titre_2,
  unique_analyse = FALSE
)
}
\arguments{
\item{time_dimension_list_groupped}{A list of data frames, each containing time dimension data.}

\item{parameter_time_dimension}{The time dimension parameter.}

\item{titre_1}{Title for the first dataset.}

\item{titre_2}{Title for the second dataset.}

\item{unique_analyse}{Logical value indicating whether the analysis is unique.}
}
\value{
A list containing dygraphs objects for visualizing the time coverage.
}
\description{
This function calculates the time coverage for different dimensions and provides interactive visualizations
of the values over time for each dataset using dygraphs.
}
\examples{
\dontrun{
time_coverage_analysis_dygraphs(time_dimension_list_groupped, "Year", "Dataset1", "Dataset2", FALSE)
}
}
\author{
Bastien Grasset, \email{bastien.grasset@ird.fr}
}
