#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset. If bare column names are provided, `dt_count()` returns counts by group.
#'
#' @param .data A data.frame or data.table
#' @param ...
#'
#' @return A data.table
#' @export
#' @md
#'
#' @examples
#' example_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))
#'
#' example_df %>%
#'   dt_count(z)
#'
#' example_df %>%
#'   dt_count()
dt_count <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  eval_tidy(expr(
    .data[, .N, by = list(!!!dots)]
  ))
}
