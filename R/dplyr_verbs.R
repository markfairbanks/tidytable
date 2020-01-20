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
#' @usage
#' dt_mutate(.data, ..., by = NULL)
#' dt_select(.data, ...)
#' dt_filter(.data, ...)
#' dt_arrange(.data, ...)
#' dt_summarize(.data, ..., by = NULL)
#'
#' @import data.table
#' @md
#' @return A data.table
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a","a","b"), a = c("a","a","b"))
#'
#' example_dt %>%
#'   dt_select(x, y, z) %>%
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
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  data_names <- colnames(.data)
  data_vars <- setNames(as.list(seq_along(.data)), data_names)

  integer_cols <- list(is.integer = seq_along(data_names)[dt_map_lgl(.data, is.integer)])
  double_cols <- list(is.double = seq_along(data_names)[dt_map_lgl(.data, is.double)])
  numeric_cols <- list(is.numeric = seq_along(data_names)[dt_map_lgl(.data, is.numeric)])
  character_cols <- list(is.character = seq_along(data_names)[dt_map_lgl(.data, is.character)])
  factor_cols <- list(is.factor = seq_along(data_names)[dt_map_lgl(.data, is.factor)])
  logical_cols <- list(is.logical = seq_along(data_names)[dt_map_lgl(.data, is.logical)])

  data_vars <- data_vars %>%
    append(integer_cols) %>%
    append(double_cols) %>%
    append(numeric_cols) %>%
    append(character_cols) %>%
    append(factor_cols) %>%
    append(logical_cols)

  select_vars <- enexprs(...)
  select_index <- unlist(eval(expr(c(!!!select_vars)), data_vars))

  keep_index <- unique(select_index[select_index > 0])
  if (length(keep_index) == 0) keep_index <- seq_along(.data)
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- setdiff(keep_index, drop_index)
  select_index <- enexpr(select_index)

  eval_tidy(expr(
    .data[, !!select_index]
  ))
}
