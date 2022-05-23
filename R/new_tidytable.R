#' Create a tidytable from a list
#'
#' @description
#' Create a tidytable from a list
#'
#' @param x A named list of equal-length vectors. The lengths are not checked; it is the responsibility
#'   of the caller to make sure they are equal.
#'
#' @export
#'
#' @examples
#' l <- list(x = 1:3, y = c("a", "a", "b"))
#'
#' new_tidytable(l)
new_tidytable <- function(x = list()) {
  new_data_frame(x, class = c("tidytable", "data.table"))
}
