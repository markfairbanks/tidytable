#' Pipeable data.table call
#'
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt(, ':='(double_x = x * 2)) %>%
#'   dt(order(-double_x))
dt <- maditr::query_if
