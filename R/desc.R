#' Descending order
#'
#' @description
#' Arrange in descending order. Can be used inside of `arrange.()`
#'
#' @param x  Variable to arrange in descending order
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- tidytable(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' example_dt %>%
#'   arrange.(c, desc.(a))
desc. <- function(x) {
  '-'(x)
}
