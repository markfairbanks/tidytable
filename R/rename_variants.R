#' Deprecated rename helpers
#'
#' @description
#' These helpers have been deprecated. Please use `rename_with.()`
#'
#' @param .data A data.frame or data.table
#' @param .vars vector `c()` of bare column names for `rename_at.()` to use
#' @param .predicate Predicate to pass to `rename_if.()`
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4)
#'
#' test_df %>%
#'   rename_with.(~ sub("x", "stuff", .x))
#'
#' test_df %>%
#'   rename_with.(~ sub("x", "stuff", .x), .cols = c(x, double_x))
rename_all. <- function(.data, .fun, ...) {
  UseMethod("rename_all.")
}

#' @export
rename_all..default <- function(.data, .fun, ...) {

  deprecate_stop("0.5.0", "tidytable::rename_all.()", "rename_with.()")

  rename_with.(.data, .fn = .fun, .cols = everything(), ...)
}

#' @export
#' @rdname rename_all.
rename_at. <- function(.data, .vars, .fun, ...) {
  UseMethod("rename_at.")
}

#' @export
rename_at..default <- function(.data, .vars, .fun, ...) {

  deprecate_stop("0.5.0", "tidytable::rename_at.()", "rename_with.()")

  .vars <- enexpr(.vars)

  rename_with.(.data, .fn = .fun, .cols = !!.vars, ...)
}

#' @export
#' @rdname rename_all.
rename_if. <- function(.data, .predicate, .fun, ...) {
  UseMethod("rename_if.")
}

#' @export
rename_if..default <- function(.data, .predicate, .fun, ...) {

  deprecate_stop("0.5.0", "tidytable::rename_if.()", "rename_with.()")

  .predicate <- enexpr(.predicate)

  rename_with.(.data, .fn = .fun, .cols = where(!!.predicate))
}
