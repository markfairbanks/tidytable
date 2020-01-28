#' Mutate
#'
#' @description
#'
#' Edit existing columns. Supports enhanced selection.
#'
#' There are two variants:
#'
#' * `dt_mutate_all()`
#' * `dt_mutate_across()`: Replaces both `mutate_if()` & `mutate_at()`
#'
#' @import data.table
#' @md
#' @usage
#'
#'
#' @param .data A data.frame or data.table
#' @param .cols vector `c()` of bare column names for `dt_mutate_across()` to use
#' @param .funs Functions to pass
#' @param ... Other arguments for the passed function
#'
#' @return A data.table
#' @export
#'
#' @examples
#' library(data.table)
#'
#' example_dt <- data.table::data.table(
#'   x = c(1,1,1),
#'   y = c(2,2,2),
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt_mutate_across(is.numeric, as.character)
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt_mutate_across(c(x, y), ~ .x * 2)
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt_mutate_across(c(x, y), list(new = ~ .x * 2))
dt_mutate_if <- function(.data, .predicate, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)[dt_map_lgl(.data, .predicate)]

  if (length(.cols) > 0) {
    .data[, (.cols) := dt_map(.SD, .fun, ...), .SDcols = .cols][]
  } else {
    .data
  }
}

#' @export
#' @rdname dt_mutate_if
dt_mutate_at <- function(.data, .vars, .funs, ...) {

  .cols <- enexpr(.vars)
  .cols <- vec_selector(.data, !!.cols) %>%
    as.character()

  if (!is.list(.funs)) {
    if (length(.cols) > 0) {
      .data[, (.cols) := dt_map(.SD, .funs, ...), .SDcols = .cols][]
    } else {
      .data
    }
  } else {

    if (!is_named(.funs)) abort("functions passed in a list must be named")

    new_names <- names(.funs)
    for (.col in .cols) {
      for (i in seq_along(new_names)) {
        new_name <- paste0(.col, "_", new_names[[i]])
        old <- .col
        user_function <- anon_x(.funs[[i]])
        .data[, (new_name) := user_function(.data[[old]])][]
      }
    }
  }
  .data
}

#' @export
#' @rdname dt_mutate_if
dt_mutate_across <- function(.data, .cols, .funs, ...) {

  .cols <- enexpr(.cols)
  .cols <- vec_selector(.data, !!.cols) %>%
    as.character()

  if (!is.list(.funs)) {
    if (length(.cols) > 0) {
      .data[, (.cols) := dt_map(.SD, .funs, ...), .SDcols = .cols][]
    } else {
      .data
    }
  } else {

    if (!is_named(.funs)) abort("functions passed in a list must be named")

    new_names <- names(.funs)
    for (.col in .cols) {
      for (i in seq_along(new_names)) {
        new_name <- paste0(.col, "_", new_names[[i]])
        old <- .col
        user_function <- anon_x(.funs[[i]])
        .data[, (new_name) := user_function(.data[[old]])][]
      }
    }
  }
  .data
}

#' @export
#' @rdname dt_mutate_if
dt_mutate_all <- function(.data, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)

  .data[, (.cols) := dt_map(.SD, .fun, ...), .SDcols = .cols][]
}
