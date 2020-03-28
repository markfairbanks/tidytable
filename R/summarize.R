#' Aggregate data using summary statistics
#'
#' @description
#' Aggregate data using summary statistics such as mean or median. Can be calculated by group.
#'
#' @param .data A data.frame or data.table
#' @param ... Aggregations to perform
#' @param by Columns to group by.
#' * A single column can be passed with `by = d`.
#' * Multiple columns can be passed with `by = c(c, d)` or `by = list(c, d)`
#' * Enhanced selection can be used:
#'   + Single predicate: `by = is.character`
#'   + Multiple predicates: `by = c(is.character, is.factor)`
#'   + A combination of predicates and column names: `by = c(is.character, b)`
#'
#' @export
#' @md
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"),
#'   d = c("a","a","b"))
#'
#' example_dt %>%
#'   summarize.(avg_a = mean(a),
#'              max_b = max(b),
#'              by = c)
#'
#' example_dt %>%
#'   summarize.(avg_a = mean(a),
#'                by = c(c, d))
summarize. <- function(.data, ..., by = NULL) {
  UseMethod("summarize.")
}

#' @export
summarize..tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)
  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  # Needed so n.() works
  # Puts () around each summary function
  dots <- map.(dots, ~ parse_expr(str_c("(", deparse(.x), ")")))

  eval_expr(
    .data[, list(!!!dots), by = !!by]
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
