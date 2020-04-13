#' Select distinct/unique rows
#'
#' @description
#' Retain only unique/distinct rows from an input df.
#'
#' Supports enhanced selection if dots are used
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns
#' @param .keep_all If TRUE and if a combination of ... is not distinct, this keeps the first row of values.
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
distinct. <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct.")
}

#' @export
distinct..tidytable <- function(.data, ..., .keep_all = FALSE) {

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
distinct..data.frame <- function(.data, ..., .keep_all = FALSE) {
  .data <- as_tidytable(.data)

  distinct.(.data, ..., .keep_all = .keep_all)
}

#' @export
#' @rdname distinct.
dt_distinct <- distinct.
