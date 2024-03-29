% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unnest_longer.R
\name{unnest_longer}
\alias{unnest_longer}
\title{Unnest a list-column of vectors into regular columns}
\usage{
unnest_longer(
  .df,
  col,
  values_to = NULL,
  indices_to = NULL,
  indices_include = NULL,
  keep_empty = FALSE,
  names_repair = "check_unique",
  simplify = NULL,
  ptype = NULL,
  transform = NULL
)
}
\arguments{
\item{.df}{A data.table or data.frame}

\item{col}{Column to unnest}

\item{values_to}{Name of column to store values}

\item{indices_to}{Name of column to store indices}

\item{indices_include}{Should an index column be included?
Defaults to \code{TRUE} when \code{col} has inner names.}

\item{keep_empty}{Return \code{NA} for any \code{NULL} elements of the list column}

\item{names_repair}{Treatment of duplicate names. See \code{?vctrs::vec_as_names} for options/details.}

\item{simplify}{Currently not supported. Errors if not \code{NULL}.}

\item{ptype}{Optionally a named list of ptypes declaring the desired output type of each component.}

\item{transform}{Optionally a named list of transformation functions applied to each component.}
}
\description{
Turns each element of a list-column into a row.
}
\examples{
df <- tidytable(
  x = 1:3,
  y = list(0, 1:3, 4:5)
)

df \%>\% unnest_longer(y)
}
