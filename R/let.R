#' Let
#'
#' @description
#'
#' Add columns. There are two variants of `let()`:
#'
#' * `let_if()`: Equivalent to `dplyr::mutate_if()`
#' * `let_at()`: Equivalent to `dplyr::mutate_at()`
#'
#' @md
#'
#' @usage
#'
#' let_if(.data, .predicate, .fun, ...) \cr
#' let_at(.data, .vars, .fun, ...)
#'
#' @param .data The data.table
#' @param .predicate Predicate to specify columns for `let_if()`
#' @param .vars Variables passed to `let_at()`
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @return
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))
#'
#' example_dt[, let(double_x = x * 2)]
#'
#' example_dt %>%
#'   let_if(is.double, as.character)
#'
#' example_dt %>%
#'   let_at(vars(x, y), function(.x) .x * 2)
let_if <- function(.data, .predicate, .fun, ...) {
  is.data.frame(.data) || stop("data must be a data frame")

  if (!is.data.table(.data)) {
    .data = as.data.table(.data)
  }

  .cols <- colnames(.data)[sapply(.data, .predicate)]

  .data <- .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols]

  .data
}

#' @export
let_at <- function(.data, .vars, .fun, ...) {

  is.data.frame(.data) || stop("data must be a data frame")

  if (!is.data.table(.data)) {
    .data = as.data.table(.data)
  }

  .vars <- as.character(substitute(.vars)[-1])

  .cols <- sapply(seq_along(.vars), function(x) .vars[[x]])

  .data[, (.cols) := lapply(.SD, .fun, ...), .SDcols = .cols]
}
