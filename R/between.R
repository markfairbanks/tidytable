#' Do the values from x fall between the left and right bounds?
#'
#' @description
#' `between.()` utilizes `data.table::between()` in the background
#'
#' @param x A numeric vector of values
#' @param left,right Boundary values
#'
#' @export
#'
#' @examples
#' between.(1:10, 5, 7)
#'
#' test_df <- data.table(
#'   x = sample(1:5, 10, replace = TRUE),
#'   y = sample(1:5, 10, replace = TRUE)
#' )
#'
#' # Typically used in a filter.()
#' test_df %>%
#'   filter.(between.(x, 1,3))
#'
#' # Can also use the %between% operator
#' test_df %>%
#'   filter.(x %between% c(1, 3))
between. <- function(x, left, right) {
  between(x = x, lower = left, upper = right)
}
