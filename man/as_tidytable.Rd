% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_tidytable.R
\name{as_tidytable}
\alias{as_tidytable}
\title{Coerce an object to a data.table/tidytable}
\usage{
as_tidytable(x, ..., .name_repair = "unique", .keep_rownames = FALSE)
}
\arguments{
\item{x}{An R object}

\item{...}{Additional arguments to be passed to or from other methods.}

\item{.name_repair}{Treatment of duplicate names. See \code{?vctrs::vec_as_names} for options/details.}

\item{.keep_rownames}{Default is \code{FALSE}. If \code{TRUE}, adds the input object's names as a separate
column named \code{"rn"}. \code{.keep_rownames = "id"} names the column "id" instead.}
}
\description{
A tidytable object is simply a data.table with nice printing features.

Note that all tidytable functions automatically convert data.frames & data.tables to tidytables in the background.
As such this function will rarely need to be used by the user.
}
\examples{
df <- data.frame(x = -2:2, y = c(rep("a", 3), rep("b", 2)))

df \%>\%
  as_tidytable()
}
