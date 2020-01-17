#' Mutate
#'
#' @description
#'
#' Edit existing columns. There are three variants:
#'
#' * `dt_mutate_if()`
#' * `dt_mutate_at()`
#' * `dt_mutate_all()`
#'
#' @import data.table
#' @md
#' @usage
#'
#' dt_mutate_if(.data, .predicate, .fun, ...)
#' dt_mutate_at(.data, .vars, .fun, ...)
#' dt_mutate_all(.data, .fun, ...)
#'
#' @param .data A data.frame or data.table
#' @param .predicate Predicate to specify columns for `dt_mutate_if()`
#' @param .vars vector `c()` of bare column names for `dt_rename_at()` to use
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @return A data.table
#' @import data.table
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   dt_mutate_if(is.double, as.character)
#'
#' example_dt %>%
#'   dt_mutate_at(list(x, y), function(.x) .x * 2)
#'
#' example_dt %>%
#'   dt_mutate_at(list(x, y), list(new = function(.x) .x * 2))
dt_mutate_if <- function(.data, .predicate, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)[dt_map_lgl(.data, .predicate)]

  if (length(.cols) > 0) {
    .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols][]
  } else {
    .data
  }
}

#' @export
#' @inherit dt_mutate_if
dt_mutate_at <- function(.data, .vars, .fun, ...) {

  .cols <- enexpr(.vars)
  .cols <- column_selector(.data, !!.cols)

  if (!is.list(.fun)) {
    if (length(.cols) > 0) {
      .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols][]
    } else {
      .data
    }
  } else {

    if (!is_named(.fun)) abort("functions passed in a list must be named")
    if (length(.fun) > 1) abort("only one function can be passed in dt_mutate_at()")

    new_names <- paste0(.cols, "_", names(.fun))
    user_function <- .fun[[1]]

    for (i in seq_along(new_names)) {
      .data[, new_names[[i]] := user_function(.data[[.cols[i]]])][]
    }
    .data
  }
}

#' @export
#' @inherit dt_mutate_if
dt_mutate_all <- function(.data, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)

  .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols][]
}
