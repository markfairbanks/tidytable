#' Deprecated rename helpers
#'
#' @description
#' These helpers have been deprecated. Please use `rename_with.()`
#'
#' @param .data A data.frame or data.table
#' @param .cols vector `c()` of bare column names for `rename_across.()` to use
#' @param .vars vector `c()` of bare column names for `rename_at.()` to use
#' @param .predicate Predicate to pass to `rename_if.()`
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4)
#'
#' example_dt %>%
#'   rename_with.(~ sub("x", "stuff", .x))
#'
#' example_dt %>%
#'   rename_with.(~ sub("x", "stuff", .x), .cols = c(x, double_x))
rename_all. <- function(.data, .fun, ...) {
  UseMethod("rename_all.")
}

#' @export
rename_all..default <- function(.data, .fun, ...) {

  rename_across.(.data, everything.(), .fun, ...)
}

#' @export
#' @rdname rename_all.
rename_at. <- function(.data, .vars, .fun, ...) {
  UseMethod("rename_at.")
}

#' @export
rename_at..default <- function(.data, .vars, .fun, ...) {

  .vars <- enexpr(.vars)

  rename_across.(.data, !!.vars, .fun, ...)
}

#' @export
#' @rdname rename_all.
rename_across. <- function(.data, .cols, .fun, ...) {
  UseMethod("rename_across.")
}

#' @export
rename_across..tidytable <- function(.data, .cols, .fun, ...) {

  .cols <- enexpr(.cols)
  .cols <- vec_selector(.data, !!.cols) %>%
    as.character()

  .data <- shallow(.data)

  .fun <- as_function(.fun)

  if (length(.cols) > 0) {

    new_names <- .fun(.cols, ...)
    setnames(.data, .cols, new_names)

    .data
  } else {
    .data
  }
}

#' @export
rename_across..data.frame <- function(.data, .cols, .fun, ...) {
  .data <- as_tidytable(.data)
  .cols <- enexpr(.cols)

  rename_across.(.data, .cols = !!.cols, .fun = .fun, ...)
}

#' @export
#' @rdname rename_all.
rename_if. <- function(.data, .predicate, .fun, ...) {
  UseMethod("rename_if.")
}

#' @export
rename_if..default <- function(.data, .predicate, .fun, ...) {

  .predicate <- enexpr(.predicate)

  rename_across.(.data, !!.predicate, .fun, ...)
}

#' @export
#' @rdname rename_all.
dt_rename_across <- rename_across.

#' @export
#' @rdname rename_all.
dt_rename_all <- rename_all.

#' @export
#' @rdname rename_all.
dt_rename_if <- rename_if.

#' @export
#' @rdname rename_all.
dt_rename_at <- rename_at.
