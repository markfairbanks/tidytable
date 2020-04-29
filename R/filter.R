#' Filter rows on one or more conditions
#'
#' @description
#' Filters a dataset to choose rows where conditions are true.
#'
#' @param .data A data.frame or data.table
#' @param ... Conditions to filter by
#' @param by Columns to group by if filtering with a summary function
#'
#' @export
#'
#' @examples
#' example_dt <- tidytable(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' example_dt %>%
#'   filter.(a >= 2, b >= 4)
#'
#' example_dt %>%
#'   filter.(b <= mean(b), by = c)
filter. <- function(.data, ..., by = NULL) {
  UseMethod("filter.")
}

#' @export
filter..data.frame <- function(.data, ..., by = NULL) {

  .data <- as_tidytable(.data)

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

    col_order <- names(.data)

    .data <- eval_expr(
      .data[, .SD[reduce(list(!!!dots), '&')], by = !!by]
    )

    setcolorder(.data, col_order)
  }
  .data
}

#' @export
#' @rdname filter.
dt_filter <- filter.
