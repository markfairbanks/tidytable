#' Deprecated
#'
#' @description
#' This function is deprecated.
#'
#' Arrange in descending order. Can be used inside of `arrange.()`
#'
#' @param x  Variable to arrange in descending order
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' test_df %>%
#'   arrange.(c, -a)
desc. <- function(x) {

  lifecycle::deprecate_stop("0.5.3", "tidytable::desc.()")

  '-'(x)
}
