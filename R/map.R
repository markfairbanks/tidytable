#' Apply a function to each element of a vector
#'
#' @description
#' The map functions transform their input by applying a function to each element and returning
#' a list/vector/data.table.
#' * `map.()` returns a list
#' * `_lgl.()`, `_int.()`, `_dbl.()`,`_chr.()`, `_df.()` variants return their specified type
#' * `_dfr.()` & `_dfc.()` Return all data frame results combined utilizing row or column binding
#'
#' @param .x A list or vector
#' @param .y A list or vector
#' @param .f A function
#' @param ... Other arguments to pass to a function
#' @param .id Whether `map_dfr.()` should add an id column to the finished dataset
#'
#' @md
#' @export
#'
#' @examples
#' map.(c(1,2,3), ~ .x + 1)
#'
#' map_dbl.(c(1,2,3), ~ .x + 1)
#'
#' map_chr.(c(1,2,3), as.character)
map. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  lapply(.x, .f, ...)
}

#' @export
#' @rdname map.
dt_map <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map()", "map.()")

  map.(.x, .f, ...)
}

#' @export
#' @rdname map.
map_lgl. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, logical(1), ...)
}

#' @export
#' @rdname map.
dt_map_lgl <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map_lgl()", "map_lgl.()")

  map_lgl.(.x, .f, ...)
}

#' @export
#' @rdname map.
map_int. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, integer(1), ...)
}

#' @export
#' @rdname map.
dt_map_int <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map_int()", "map_int.()")

  map_int.(.x, .f, ...)
}

#' @export
#' @rdname map.
map_dbl. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, double(1), ...)
}

#' @export
#' @rdname map.
dt_map_dbl <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map_dbl()", "map_dbl.()")

  map_dbl.(.x, .f, ...)
}

#' @export
#' @rdname map.
map_chr. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, character(1), ...)
}

#' @export
#' @rdname map.
dt_map_chr <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map_chr()", "map_chr.()")

  map_chr.(.x, .f, ...)
}

#' @export
#' @rdname map.
map_dfc. <- function(.x, .f, ...) {
  .f <- as_function(.f)
  result_list <- map.(.x, .f, ...)
  bind_cols.(result_list)
}

#' @export
#' @rdname map.
dt_map_dfc <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map_dfc()", "map_dfc.()")

  map_dfc.(.x, .f, ...)
}

#' @export
#' @rdname map.
map_dfr. <- function(.x, .f, ..., .id = NULL) {
  .f <- as_function(.f)

  result_list <- map.(.x, .f, ...)

  bind_rows.(result_list, .id = .id)
}

#' @export
#' @rdname map.
dt_map_dfr <- function(.x, .f, ..., .id = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_map_dfr()", "map_dfr.()")

  map_dfr.(.x, .f, ..., .id = .id)
}

#' @export
#' @rdname map.
map_df. <- map_dfr.

#' @export
#' @rdname map.
dt_map_df <- function(.x, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map_df()", "map_df.()")

  map_df.(.x, .f, ...)
}

#' @export
#' @rdname map.
walk. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  map.(.x, .f, ...)

  invisible(.x)
}
