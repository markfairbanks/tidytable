% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_with.R
\name{rename_with}
\alias{rename_with}
\title{Rename multiple columns}
\usage{
rename_with(.df, .fn = NULL, .cols = everything(), ...)
}
\arguments{
\item{.df}{A data.table or data.frame}

\item{.fn}{Function to transform the names with.}

\item{.cols}{Columns to rename. Defaults to all columns. \code{tidyselect} compatible.}

\item{...}{Other parameters to pass to the function}
}
\description{
Rename multiple columns with the same transformation
}
\examples{
df <- data.table(
  x = 1,
  y = 2,
  double_x = 2,
  double_y = 4
)

df \%>\%
  rename_with(toupper)

df \%>\%
  rename_with(~ toupper(.x))

df \%>\%
  rename_with(~ toupper(.x), .cols = c(x, double_x))
}
