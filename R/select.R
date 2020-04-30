#' Select or drop columns
#'
#' @description
#' Select or drop columns from a data.table
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select or drop.
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.
#' `tidyselect` compatible.
#'
#' @export
#' @md
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,1,1),
#'   y = c(4,5,6),
#'   double_x = c(2,2,2),
#'   z = c("a","a","b"))
#'
#' example_dt %>%
#'   select.(x, y)
#'
#' example_dt %>%
#'   select.(x:z)
#'
#' example_dt %>%
#'   select.(-y, -z)
#'
#' example_dt %>%
#'   select.(starts_with.("x"), z)
#'
#' example_dt %>%
#'   select.(is.character, x)
#'
#' example_dt %>%
#'   select.(stuff = x, y)
select. <- function(.data, ...) {
  UseMethod("select.")
}

#' @export
select..data.frame <- function(.data, ...) {

  .data <- as_tidytable(.data)

  select_cols <- dots_selector_i(.data, ...)

  .data <- eval_expr(.data[, !!select_cols])

  .data <- set_names(.data, names(select_cols))

  .data
}

#' @export
#' @rdname select.
dt_select <- select.
