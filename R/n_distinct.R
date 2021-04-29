#' Count the number of unique values in a vector
#'
#' @description
#' This is a faster version of `length(unique(x))` that calls `data.table::uniqueN()`.
#'
#' @param ... vectors of values
#' @param na.rm  If `TRUE` missing values don't count
#'
#' @export
#'
#' @examples
#' x <- sample(1:10, 1e5, rep = TRUE)
#' n_distinct.(x)
n_distinct. <- function(..., na.rm = FALSE) {
  # replace with uniqueN(c(...), na.rm = na.rm) when
  # data.table issue #3739 is fixed
  # https://github.com/Rdatatable/data.table/issues/3739
  x <- c(...)
  if (na.rm) {
    x <- x[!vec_equal_na(x)]
  }
  vec_unique_count(x)
}
