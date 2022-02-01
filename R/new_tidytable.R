#' Create a tidytable from a list
#'
#' @description
#' Create a tidytable from a list
#'
#' @param x A named list of equal-length vectors. The lengths are not checked; it is the responsibility
#'   of the caller to make sure they are equal.
#' @param n Number of rows. If `NULL`, will be computed from the length of the first element of `x`.
#'
#' @export
#'
#' @examples
#' l <- list(x = 1:3, y = c("a", "a", "b"))
#'
#' new_tidytable(l)
new_tidytable <- function(x = list(), n = NULL) {
  new_data_frame(x, n, class = c("tidytable", "data.table"))
}
