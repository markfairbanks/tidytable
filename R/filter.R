#' Filter rows on one or more conditions
#'
#' @description
#' Filters a dataset to choose rows where conditions are true.
#'
#' @param .data A data.frame or data.table
#' @param ... Conditions to filter by
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6))
#'
#' example_dt %>%
#'   filter.(a >= 2, b >= 4)
filter. <- function(.data, ..., by = NULL) {
  UseMethod("filter.")
}

#' @export
filter..tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)

  by <- enexpr(by)

  if (is.null(by)) {
    for (dot in dots) {
      .data <- eval_expr(
        .data[!!dot]
      )
    }
  } else {
    by <- vec_selector_by(.data, !!by)

    .data <- eval_expr(
      .data[, .SD[reduce(list(!!!dots), .f = '&')], by = !!by]
    )
  }
  .data
}

#' @export
filter..data.frame <- function(.data, ..., by = NULL) {
  .data <- as_tidytable(.data)

  filter.(.data, ...)
}

#' @export
#' @rdname filter.
dt_filter <- filter.
