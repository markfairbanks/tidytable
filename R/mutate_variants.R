#' Mutate
#'
#' @description
#'
#' Edit existing columns. There are two variants:
#'
#' * `dt_mutate_if()`
#' * `dt_mutate_at()`
#'
#' @import data.table
#' @md
#' @usage
#'
#' dt_mutate_if(.data, .predicate, .fun, ...)
#' dt_mutate_at(.data, .vars, .fun, ...)
#'
#' @param .data The data.table
#' @param .predicate Predicate to specify columns for `dt_mutate_if()`
#' @param .vars `list()` of variables for `dt_mutate_at()` to use
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
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
dt_mutate_if <- function(.data, .predicate, .fun, ...) {
  is.data.frame(.data) || stop("data must be a data frame")

  if (!is.data.table(.data)) {
    .data = as.data.table(.data)
  }

  .cols <- colnames(.data)[sapply(.data, .predicate)]

  if (length(.cols) > 0) {
    .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols]
  } else {
    .data
  }
}

#' @export
#' @inherit dt_mutate_if
dt_mutate_at <- function(.data, .vars, .fun, ...) {

  is.data.frame(.data) || stop("data must be a data frame")

  if (!is.data.table(.data)) {
    .data <- as.data.table(.data)
  }

  .cols <- as.character(substitute(.vars))[-1]

  if (length(.cols) > 0) {
    .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols]
  } else {
    .data
  }
}
