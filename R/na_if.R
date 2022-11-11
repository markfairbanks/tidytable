#' Convert values to `NA`
#'
#' @description
#' Convert values to `NA`.
#'
#' @param x A vector
#' @param y Value to replace with `NA`
#'
#' @export
#'
#' @examples
#' vec <- 1:3
#' na_if(vec, 3)
na_if <- function(x, y) {
  size <- vec_size(x)
  y <- vec_recycle(y, size)
  vec_assign(x, x == y, NA)
}

#' @export
#' @keywords internal
#' @inherit na_if
na_if. <- na_if
