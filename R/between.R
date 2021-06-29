#' Do the values from x fall between the left and right bounds?
#'
#' @description
#' `between.()` utilizes `data.table::between()` in the background
#'
#' @param x A numeric vector
#' @param left,right Boundary values
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = 1:5,
#'   y = 1:5
#' )
#'
#' # Typically used in a filter.()
#' test_df %>%
#'   filter.(between.(x, 2, 4))
#'
#' test_df %>%
#'   filter.(x %>% between.(2, 4))
#'
#' # Can also use the %between% operator
#' test_df %>%
#'   filter.(x %between% c(2, 4))
between. <- function(x, left, right) {
  between(x = x, lower = left, upper = right)
}
