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
#' n_distinct(x)
n_distinct <- function(..., na.rm = FALSE) {
  dots <- list2(...)

  if (length(dots) == 1) {
    x <- dots[[1]]
  } else {
    x <- new_data_frame(dots)
  }

  uniqueN(x, na.rm = na.rm)
}

#' @export
#' @keywords internal
#' @inherit n_distinct
n_distinct. <- n_distinct
