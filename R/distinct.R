#' Select distinct/unique rows
#'
#' @description
#' Retain only unique/distinct rows from an input df.
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns.
#' `tidyselect` compatible.
#' @param .keep_all Only relevant if columns are provided to ... arg.
#' This keeps all columns, but only keeps the first row of each distinct
#' values of columns provided to ... arg.
#'
#' @export
#' @md
#'
#' @examples
#' example_dt <- tidytable(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   distinct.()
#'
#' example_dt %>%
#'   distinct.(z)
distinct. <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct.")
}

#' @export
distinct..data.frame <- function(.data, ..., .keep_all = FALSE) {

  .data <- as_tidytable(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    unique(.data)
  } else if (!.keep_all) {
    select_cols <- dots_selector_i(.data, ...)

    unique(.data, by = select_cols)[, ..select_cols]
  } else {
    select_cols <- dots_selector_i(.data, ...)

    unique(.data, by = select_cols)
  }
}

#' @export
#' @rdname distinct.
dt_distinct <- distinct.
