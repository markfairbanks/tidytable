#' `dplyr` verbs for data.table
#'
#' @description
#' The core dplyr verbs can be used in `tidydt`:
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
#' @param ... Values passed to `tidydt` functions
#' @param by `list()` of bare column names to group by
#'
#'
#' @import data.table
#' @md
#' @return A data.table
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3),
#'                          y = c(4,5,6),
#'                          z = c("a","a","b"),
#'                          a = c("a","a","b"))
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt_select(x, y, z, a) %>%
#'   dt_mutate(double_x = x * 2,
#'             double_y = y * 2) %>%
#'   dt_filter(double_x > 0, double_y > 0) %>%
#'   dt_arrange(-double_x) %>%
#'   dt_summarize(avg_x = mean(x), by = list(a, z))
dt_mutate <- function(.data, ..., by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)
  by <- enexpr(by)

  all_names <- names(dots)

  for (i in seq_along(dots)) {
    eval_tidy(expr(.data[, ':='(all_names[[i]], !!dots[[i]]), !!by][]))
  }
  .data
}

# dt_mutate <- function(.data, ..., by = NULL) {
#   if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
#   if (!is.data.table(.data)) .data <- as.data.table(.data)
#
#   dots <- enexprs(...)
#   by <- enexpr(by)
#
#   eval_tidy(expr(
#     .data[, ':='(!!!dots), !!by][]
#   ))
# }

#' @export
#' @rdname dt_mutate
dt_filter <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  for (dot in dots) {
    .data <- eval_tidy(expr(.data[!!dot]))
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
  dots <- dots_selector(.data, ...)

  eval_tidy(expr(
    .data[, list(!!!dots)]
  ))
}
