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
#'   distinct.()
#'
#' example_dt %>%
#'   distinct.(z)
distinct. <- function(.data, ...) {
  UseMethod("distinct.")
}

#' @export
distinct..tidytable <- function(.data, ...) {

  dots <- enexprs(...)

  if (length(dots) == 0) {
    unique(.data)
  } else {
    unique(dt_select(.data, ...))
  }
}

#' @export
distinct..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  distinct.(.data, ...)
}

#' @export
#' @rdname distinct.
dt_distinct <- distinct.
