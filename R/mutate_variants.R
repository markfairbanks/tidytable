#' Depcrecated mutate helpers
#'
#' @description
#' These helpers have been deprecated. Please use `mutate_across.()`
#'
#' @md
#'
#' @param .data A data.frame or data.table
#' @param .predicate predicate for `mutate_if.()` to use
#' @param .vars vector `c()` of bare column names for `mutate_at.()` to use
#' @param .funs Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param by Columns to group by
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,1,1),
#'   y = c(2,2,2),
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   mutate_across.(is.numeric, as.character)
#'
#' example_dt %>%
#'   mutate_across.(c(x, y), ~ .x * 2)
#'
#' example_dt %>%
#'   mutate_across.(everything.(), as.character)
#'
#' example_dt %>%
#'   mutate_across.(c(x, y), list(new = ~ .x * 2))
mutate_if. <- function(.data, .predicate, .funs, ..., by = NULL) {
  UseMethod("mutate_if.")
}

#' @export
mutate_if..default <- function(.data, .predicate, .funs, ..., by = NULL) {
  .predicate <- enexpr(.predicate)
  by <- enexpr(by)

  mutate_across.(.data, !!.predicate, .funs, ..., by = !!by)
}

#' @export
#' @rdname mutate_if.
mutate_at. <- function(.data, .vars, .funs, ..., by = NULL) {
  UseMethod("mutate_at.")
}

#' @export
mutate_at..default <- function(.data, .vars, .funs, ..., by = NULL) {
  .vars <- enexpr(.vars)
  by <- enexpr(by)

  mutate_across.(.data, !!.vars, .funs, ..., by = !!by)
}

#' @export
#' @rdname mutate_if.
mutate_all. <- function(.data, .funs, ..., by = NULL) {
  UseMethod("mutate_all.")
}

#' @export
mutate_all..default <- function(.data, .funs, ..., by = NULL) {

  by <- enexpr(by)

  mutate_across.(.data, everything.(), .funs, ..., by = !!by)
}

#' @export
#' @rdname mutate_if.
dt_mutate_if <- mutate_if.

#' @export
#' @rdname mutate_if.
dt_mutate_at <- mutate_at.

#' @export
#' @rdname mutate_if.
dt_mutate_all <- mutate_all.
