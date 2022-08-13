#' Convert values to NA
#'
#' @description
#' Convert values to `NA`.
#'
#' @param x An R object
#' @param y Value(s) to replace with `NA`
#'
#' @export
#'
#' @examples
#' vec <- 1:3
#' na_if.(vec, 3)
na_if. <- function(x, y) {
  if (length(y) == 1) {
    vec_assign(x, x == y, NA)
  } else {
    vec_assign(x, x %in% y, NA)
  }
}
