% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separate_wider_delim.R
\name{separate_wider_delim}
\alias{separate_wider_delim}
\title{Separate a character column into multiple columns}
\usage{
separate_wider_delim(
  .df,
  cols,
  delim,
  ...,
  names = NULL,
  names_sep = NULL,
  names_repair = "check_unique",
  too_few = c("align_start", "error"),
  too_many = c("drop", "error"),
  cols_remove = TRUE
)
}
\arguments{
\item{.df}{A data frame}

\item{cols}{Columns to separate}

\item{delim}{Delimiter to separate on}

\item{...}{These dots are for future extensions and must be empty.}

\item{names}{New column names to separate into}

\item{names_sep}{Names separator}

\item{names_repair}{Treatment of duplicate names. See \code{?vctrs::vec_as_names} for options/details.}

\item{too_few}{What to do when too few column names are supplied}

\item{too_many}{What to do when too many column names are supplied}

\item{cols_remove}{Should old columns be removed}
}
\description{
Separates a single column into multiple columns
}
\examples{
df <- tidytable(x = c("a", "a_b", "a_b", NA))

df \%>\%
  separate_wider_delim(x, delim = "_", names = c("left", "right"))

df \%>\%
  separate_wider_delim(x, delim = "_", names_sep = "")
}
