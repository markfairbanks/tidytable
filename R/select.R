#' Select or drop columns
#'
#' @description
#' Select or drop columns from a data.table
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select or drop. Use named arguments, e.g. new_name = old_name, to rename selected variables.
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
select..tidytable <- function(.data, ...) {

  select_cols <- as.character(dots_selector(.data, ...))

  # Using a character vector is faster for select
  .data <- eval_expr(.data[, !!select_cols])

  dots <- enexprs(...)
  need_rename <- have_name(dots)

  if (any(need_rename)) {

    rename_dots <- dots[need_rename]
    new_names <- names(rename_dots)
    old_names <- as.character(rename_dots)

    setnames(.data, old_names, new_names)
  }
  .data
}

#' @export
select..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  select.(.data, ...)
}

#' @export
#' @rdname select.
dt_select <- select.
