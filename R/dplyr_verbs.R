#' `dplyr` verbs for data.table
#'
#' @description
#' The core dplyr verbs can be used in `gdt`:
#'
#' * `dt_mutate()`
#' * `dt_select()`
#' * `dt_arrange()`
#' * `dt_filter()`
#' * `dt_summarize()`
#'
#' @param .data A data.frame or data.table
#' @param ... Values passed to `gdt` functions
#' @param by `list()` of bare column names to group by
#' @param keyby `list()` of bare column names to group by or key by
#'
#' @usage
#' dt_mutate(.data, ..., by, keyby)
#' dt_select(.data, ...)
#' dt_filter(.data, ...)
#' dt_arrange(.data, ...)
#' dt_summarize(.data, ..., by, keyby)
#'
#' @import data.table
#' @md
#' @return A data.table
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a","a","b"))
#'
#' example_dt %>%
#'   dt_select(x, y, z) %>%
#'   dt_mutate(double_x = x * 2,
#'             double_y = y * 2) %>%
#'   dt_filter(double_x > 0, double_y > 0) %>%
#'   dt_arrange(-double_x) %>%
#'   dt_summarize(avg_x = mean(x), by = z)
dt_mutate <- function(.data, ..., by, keyby) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  eval.parent(substitute(
    .data[, ':='(...), by, keyby][]
  ))
}

#' @export
#' @inherit dt_mutate
dt_filter <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  eval.parent(substitute(
    .data[Reduce(f = '&', list(...)), ]
  ))
}

#' @export
#' @inherit dt_mutate
dt_arrange <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  eval.parent(substitute(
    .data[order(...)]
  ))
}

#' @export
#' @inherit dt_mutate
dt_summarize <- function(.data, ..., by, keyby) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  eval.parent(substitute(
    .data[, list(...), by, keyby]
  ))
}

#' @export
#' @inherit dt_mutate
dt_summarise <- dt_summarize

#' @export
#' @inherit dt_mutate
dt_select <- function(.data, ...){
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  data_names <- colnames(.data)

  select_vars <- substitute(c(...))

  data_vars <- setNames(as.list(seq_along(.data)), data_names)

  select_index <- eval(select_vars, data_vars)
  keep_index <- unique(select_index[select_index > 0])
  if (length(keep_index) == 0) keep_index <- seq_along(.data)
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- setdiff(keep_index, drop_index)

  eval.parent(substitute(
    .data[, select_index]
  ))
}
