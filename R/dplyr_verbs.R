#' `dplyr` verbs for data.table
#'
#' @description
#' The core dplyr verbs can be used:
#'
#' * `dt_mutate()`
#' * `dt_select()`
#' * `dt_arrange()`
#' * `dt_filter()`
#' * `dt_summarize()`
#'
#' `dt_select()` supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... dots passed to underlying functions (see examples)
#' @param by `list()` of bare column names to group by
#'
#' @md
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
#'   dt_select(a, b, c, d) %>%
#'   dt_mutate(double_a = a * 2,
#'             double_b = b * 2) %>%
#'   dt_filter(double_a > 0, double_b > 0) %>%
#'   dt_arrange(-double_a) %>%
#'   dt_summarize(avg_a = mean(a),
#'                by = list(c, d))
dt_mutate <- function(.data, ..., by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)
  by <- enexpr(by)
  .data <- shallow(.data)

  all_names <- names(dots)

  for (i in seq_along(dots)) {
    eval_tidy(expr(
      .data[, ':='(all_names[[i]], !!dots[[i]]), !!by][]
    ))
  }
  .data
}

#' @export
#' @rdname dt_mutate
dt_filter <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  for (dot in dots) {
    .data <- eval_tidy(expr(
      .data[!!dot]
      ))
  }
  .data
}

#' @export
#' @rdname dt_mutate
dt_arrange <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  eval_tidy(expr(
    .data[order(!!!dots)]
  ))
}

#' @export
#' @rdname dt_mutate
dt_summarize <- function(.data, ..., by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)
  by <- enexpr(by)

  eval_tidy(expr(
    .data[, list(!!!dots), !!by]
  ))
}

#' @export
#' @rdname dt_mutate
dt_summarise <- dt_summarize

#' @export
#' @rdname dt_mutate
dt_select <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- dots_selector(.data, ...)

  eval_tidy(expr(
    .data[, list(!!!dots)]
  ))
}
