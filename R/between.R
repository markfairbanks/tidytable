#' Do the values from x fall between the left and right bounds?
#'
#' @description
#' `between()` utilizes `data.table::between()` in the background
#'
#' @param x A numeric vector
#' @param left,right Boundary values
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:5,
#'   y = 1:5
#' )
#'
#' # Typically used in a filter()
#' df %>%
#'   filter(between(x, 2, 4))
#'
#' df %>%
#'   filter(x %>% between(2, 4))
#'
#' # Can also use the %between% operator
#' df %>%
#'   filter(x %between% c(2, 4))
between <- function(x, left, right) {
  data.table::between(x = x, lower = left, upper = right)
}

#' @export
#' @keywords internal
#' @inherit between
between. <- function(x, left, right) {
  deprecate_dot_fun()
  between(x, left, right)
}
