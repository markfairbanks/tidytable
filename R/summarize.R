#' Aggregate data using summary statistics
#'
#' @description
#' Aggregate data using summary statistics such as mean or median. Can be calculated by group.
#'
#' @param .data A data.frame or data.table
#' @param ... Aggregations to perform
#' @param by Optional: `list()` of bare column names to group by
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"),
#'   d = c("a","a","b"))
#'
#' example_dt %>%
#'   summarize.(avg_a = mean(a),
#'                max_b = max(b),
#'                by = c)
#'
#' example_dt %>%
#'   summarize.(avg_a = mean(a),
#'                by = list(c, d))
summarize. <- function(.data, ..., by = NULL) {
  UseMethod("summarize.")
}

#' @export
summarize..tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)
  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  eval_expr(
    .data[, list(!!!dots), !!by]
  )
}

#' @export
summarize..data.frame <- function(.data, ..., by = NULL) {
  .data <- as_tidytable(.data)
  by <- enexpr(by)

  summarize.(.data, ..., by = !!by)
}

#' @export
#' @rdname summarize.
summarise. <- summarize.

#' @export
#' @rdname summarize.
dt_summarise <- summarize.

#' @export
#' @rdname summarize.
dt_summarize <- summarize.
