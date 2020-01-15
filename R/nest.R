#' Nest data.tables
#'
#' @description
#' Nest data.tables by group
#'
#' @param .data A data.frame or data.table
#' @param ... bare column names to group by
#' @param .key Name of the new column created by nesting
#'
#' @return
#' @export
#'
#' @examples
#' test_df <- data.table(a = 1:10,
#'                       b = 11:20,
#'                       c = c(rep("a", 6), rep("b", 4)),
#'                       d = c(rep("a", 4), rep("b", 6)))
#'
#' test_df %>%
#'   dt_group_nest(c, d)
dt_group_nest <- function(.data, ..., .key = "data") {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  eval_tidy(expr(
    .data[, list(data = list(.SD)), by = list(!!!dots)] %>%
      dt_rename(!!.key := data)
  ))
}

