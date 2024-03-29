% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first-last-nth.R
\name{first}
\alias{first}
\alias{last}
\alias{nth}
\title{Extract the first, last, or nth value from a vector}
\usage{
first(x, default = NULL, na_rm = FALSE)

last(x, default = NULL, na_rm = FALSE)

nth(x, n, default = NULL, na_rm = FALSE)
}
\arguments{
\item{x}{A vector}

\item{default}{The default value if the value doesn't exist.}

\item{na_rm}{If \code{TRUE} ignores missing values.}

\item{n}{For \code{nth()}, a number specifying the position to grab.}
}
\description{
Extract the first, last, or nth value from a vector.

Note: These are simple wrappers around \code{vctrs::vec_slice()}.
}
\examples{
vec <- letters

first(vec)
last(vec)
nth(vec, 4)
}
