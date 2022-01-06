#' Nest data.tables
#'
#' @description
#' Nest data.tables
#'
#' @param .df A data.table or data.frame
#' @param ... Columns to be nested.
#' @param .names_sep If NULL, the names will be left alone. If a string,
#' the names of the columns will be created by pasting together the inner
#' column names and the outer column names.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:10,
#'   b = 11:20,
#'   c = c(rep("a", 6), rep("b", 4)),
#'   d = c(rep("a", 4), rep("b", 6))
#' )
#'
#' df %>%
#'   nest.(data = c(a, b))
#'
#' df %>%
#'   nest.(data = where(is.numeric))
nest. <- function(.df, ..., .names_sep = NULL) {
  UseMethod("nest.")
}

#' @export
nest..tidytable <- function(.df, ..., .names_sep = NULL) {
  if (!is.null(.names_sep)) vec_assert(.names_sep, character(), 1)

  dots <- enquos(...)

  if (length(dots) > 1) {
    abort("Currently only one nesting can be passed at a time.")
  }

  if (!is_named(dots)) {
    abort("All elements of `...` must be named. For example `nest.(data = c(x, y))`")
  }

  .key <- names(dots)

  dots <- unname(dots)

  if (!is.null(.names_sep)) {
    nest_cols <- tidyselect_names(.df, !!!dots)

    new_names <- paste(.key, nest_cols, sep = .names_sep)

    .df <- shallow(.df)

    setnames(.df, nest_cols, new_names)

    dots <- syms(new_names)
  }

  nest_by.(.df, -c(!!!dots), .key = .key)
}

#' @export
nest..data.frame <- function(.df, ..., .names_sep = NULL) {
  .df <- as_tidytable(.df)
  nest.(.df, ..., .names_sep = .names_sep)
}
