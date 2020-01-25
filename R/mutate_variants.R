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
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @return A data.table
#' @import data.table
#' @export
#'
#' @examples
#' library(data.table)
#'
#' example_dt <- data.table(x = c(1,1,1),
#'                          y = c(2,2,2),
#'                          z = c("a", "a", "b"))
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
#'
#' @import data.table
#' @importFrom rlang enexprs
#' @importFrom rlang eval_tidy
#' @importFrom rlang expr
#' @importFrom rlang is_named
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
dt_mutate_at <- function(.data, .cols, .fun, ...) {

  .cols <- enexpr(.cols)
  .cols <- vec_selector(.data, !!.cols) %>%
    as.character()

  if (!is.list(.fun)) {
    if (length(.cols) > 0) {
      .data[, (.cols) := dt_map(.SD, .fun, ...), .SDcols = .cols][]
    } else {
      .data
    }
  } else {

    if (!is_named(.fun)) abort("functions passed in a list must be named")
    if (length(.fun) > 1) abort("only one function can be passed in dt_mutate_at()")

    new_names <- paste0(.cols, "_", names(.fun))
    user_function <- anon_x(.fun[[1]])

    for (i in seq_along(new_names)) {
      new <- new_names[i]
      old <- .cols[i]
      .data[, (new) := user_function(.data[[old]])][]
    }
    .data
  }
}

#' @export
#' @rdname dt_mutate_if
dt_mutate_across <- function(.data, .cols, .fun, ...) {

  .cols <- enexpr(.cols)
  .cols <- vec_selector(.data, !!.cols) %>%
    as.character()

  if (!is.list(.fun)) {
    if (length(.cols) > 0) {
      .data[, (.cols) := dt_map(.SD, .fun, ...), .SDcols = .cols][]
    } else {
      .data
    }
  } else {

    if (!is_named(.fun)) abort("functions passed in a list must be named")
    if (length(.fun) > 1) abort("only one function can be passed in dt_mutate_at()")

    new_names <- paste0(.cols, "_", names(.fun))
    user_function <- anon_x(.fun[[1]])

    for (i in seq_along(new_names)) {
      new <- new_names[i]
      old <- .cols[i]
      .data[, (new) := user_function(.data[[old]])][]
    }
    .data
  }
}

#' @export
#' @rdname dt_mutate_if
dt_mutate_all <- function(.data, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)

  .data[, (.cols) := dt_map(.SD, .fun, ...), .SDcols = .cols][]
}
