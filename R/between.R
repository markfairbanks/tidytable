#' Do the values from x fall between the left and right bounds?
#'
#' @description
#'  `between.()` utilizes `data.table::between()` in the background
#'
#' @param x A numeric vector of values
#' @param left,right Boundary values (must be scalars).
#'
#' @export
#'
#' @examples
#' between.(1:10, 5, 7)
#'
#' test_df <- data.frame(x = sample(1:5, 10, replace = TRUE), y = sample(1:5, 10, replace = TRUE))
#'
#' ## Use in a filter
#' test_df %>%
#'   filter.(between.(x, 1,3))
#'
#'  ## Use in a mutate
#'  test_df %>%
#'    mutate.(x_between = between.(x, 2,3),
#'    x_between_by_two = ifelse.(between.(x, 2,3), x * 2, 0))
#'
#'
between. <- function(x, left, right) {

  between(x = x, lower = left, upper = right)
}
