#' Select distinct/unique rows
#'
#' @description
#' Retain only unique/distinct rows from an input df.
#'
#' Supports enhanced selection if dots are used
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   dt_distinct()
#'
#' example_dt %>%
#'   dt_distinct(z)
dt_distinct <- function(.data, ...) {
  UseMethod("dt_distinct")
}

#' @export
dt_distinct.tidytable <- function(.data, ...) {

  dots <- enexprs(...)

  if (length(dots) == 0) {
    unique(.data)
  } else {
    dots <- dots_selector(.data, ...)

    .data %>%
      dt_select(!!!dots) %>%
      unique()
  }
}

#' @export
dt_distinct.data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  dt_distinct(.data, ...)
}
