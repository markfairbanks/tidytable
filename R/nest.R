#' Nest columns into a list-column
#'
#' @description
#' Nest columns into a list-column
#'
#' @param .df A data.table or data.frame
#' @param ... Columns to be nested.
#' @param .by Columns to nest by
#' @param .key New column name if `.by` is used
#' @param .names_sep If NULL, the names will be left alone. If a string,
#'   the names of the columns will be created by pasting together the inner
#'   column names and the outer column names.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 1:3,
#'   c = c("a", "a", "b"),
#'   c = c("a", "a", "b")
#' )
#'
#' df %>%
#'   nest(data = c(a, b))
#'
#' df %>%
#'   nest(data = where(is.numeric))
#'
#' df %>%
#'   nest(.by = c(c, d))
nest <- function(.df, ..., .by = NULL, .key = NULL, .names_sep = NULL) {
  nest.(.df, ..., .by = {{ .by }}, .key = .key, .names_sep = .names_sep)
}

#' @export
#' @keywords internal
#' @inherit nest
nest. <- function(.df, ..., .by = NULL, .key = NULL, .names_sep = NULL) {
  UseMethod("nest.")
}

#' @export
nest..tidytable <- function(.df, ..., .by = NULL, .key = NULL, .names_sep = NULL) {
  if (!is.null(.names_sep)) {
    vec_assert(.names_sep, character(), 1)
  }

  dots <- enquos(...)

  if (length(dots) > 1) {
    abort("Currently only one nesting can be passed at a time.")
  }

  if (!is_named(dots)) {
    abort("All elements of `...` must be named. For example `nest(data = c(x, y))`")
  }

  .by <- enquo(.by)

  no_dots <- length(dots) == 0

  null_by <- quo_is_null(.by)

  if (no_dots) {
    .key <- .key %||% "data"
  } else {
    .key <- names(dots)
  }

  if (no_dots && null_by) {
    .by <- character()
    cols <- names(.df)
  } else if (no_dots) {
    .by <- tidyselect_names(.df, !!.by)
    cols <- tidyselect_names(.df, -any_of(.by))
  } else if (null_by) {
    cols <- tidyselect_names(.df, !!dots[[1]])
    .by <- tidyselect_names(.df, -any_of(cols))
  } else {
    .by <- tidyselect_names(.df, !!.by)
    cols <- tidyselect_names(.df, !!dots[[1]])
  }

  if (!is.null(.names_sep)) {
    new_names <- paste(.key, cols, sep = .names_sep)

    .df <- df_set_names(.df, new_names, cols)

    cols <- new_names
  }

  dt_j(.df, .(!!.key := list(.SD)), by = .by, .SDcols = cols)
}

#' @export
nest..grouped_tt <- function(.df, ..., .by = NULL, .key = NULL, .names_sep = NULL) {
  .groups <- group_vars(.df)
  out <- ungroup(.df)
  out <- nest(out, ..., .by = any_of(.groups), .key = .key, .names_sep = .names_sep)
  group_by(out, all_of(.groups))
}

#' @export
nest..data.frame <- function(.df, ..., .by = NULL, .key = NULL, .names_sep = NULL) {
  .df <- as_tidytable(.df)
  nest.(.df, ..., .by = {{ .by }}, .key = .key, .names_sep = .names_sep)
}

