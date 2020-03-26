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
select. <- function(.data, ...) {
  UseMethod("select.")
}

#' @export
select..tidytable <- function(.data, ...) {

  select_cols <- as.character(dots_selector(.data, ...))

  # Using a character vector is faster for select
  eval_expr(
    .data[, !!select_cols]
  )
}

#' @export
select..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  select.(.data, ...)
}

#' @export
#' @rdname select.
dt_select <- select.
