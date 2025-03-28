% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pie_chart_2_default.R
\name{pie_chart_2_default}
\alias{pie_chart_2_default}
\title{Create Pie Charts from Data}
\usage{
pie_chart_2_default(
  dimension,
  first,
  second = NULL,
  topn = 5,
  titre_1 = "first",
  titre_2 = "second",
  title_yes_no = TRUE,
  dataframe = FALSE
)
}
\arguments{
\item{dimension}{A character string indicating the dimension for grouping.}

\item{first}{A data frame representing the first dataset.}

\item{second}{An optional second data frame.}

\item{topn}{An integer for the number of top categories to display.}

\item{titre_1}{A character string for the title of the first dataset.}

\item{titre_2}{A character string for the title of the second dataset.}

\item{title_yes_no}{Logical indicating if a title should be displayed.}

\item{dataframe}{Logical indicating if a data frame should be returned.}
}
\value{
A pie chart or a list containing the pie chart and data frame, if specified.
}
\description{
This function creates pie charts from measurement data for one or two datasets.
}
