#' Select or drop columns
#'
#' @description
#' Select or drop columns from a data.table
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select or drop
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,1,1),
#'   y = c(4,5,6),
#'   double_x = c(2,2,2),
#'   z = c("a","a","b"))
#'
#' example_dt %>%
#'   select(x, y)
#'
#' example_dt %>%
#'   select(x:z)
#'
#' example_dt %>%
#'   select(-y, -z)
#'
#' example_dt %>%
#'   select(dt_starts_with("x"), z)
#'
#' example_dt %>%
#'   select(is.character, x)
dt_select <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- dots_selector(.data, ...)

  eval_tidy(expr(
    .data[, list(!!!dots)]
  ))
}
