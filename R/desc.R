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
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b")
#' )
#'
#' df %>%
#'   arrange.(c, desc.(a))
desc. <- function(x) {
  -xtfrm(x)
}
