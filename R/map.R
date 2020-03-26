#' Apply a function to each element of a vector
#'
#' @description
#' The dt_map functions transform their input by applying a function to each element and returning a vector the same length as the input.
#' * `map.()` returns a list
#' * `_lgl.()`, `_int.()`, `_dbl.()`,`_chr.()`, `_df.()` variants return their specified type
#' * `_dfr.()` & `_dfc.()` Return data frame results binded together
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
dt_map <- map.

#' @export
#' @rdname map.
map_lgl. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, logical(1), ...)
}

#' @export
#' @rdname map.
dt_map_lgl <- map_lgl.

#' @export
#' @rdname map.
map_int. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, integer(1), ...)
}

#' @export
#' @rdname map.
dt_map_int <- map_int.

#' @export
#' @rdname map.
map_dbl. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, double(1), ...)
}

#' @export
#' @rdname map.
dt_map_dbl <- map_dbl.

#' @export
#' @rdname map.
map_chr. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  vapply(.x, .f, character(1), ...)
}

#' @export
#' @rdname map.
dt_map_chr <- map_chr.

#' @export
#' @rdname map.
map_dfc. <- function(.x, .f, ...) {
  .f <- as_function(.f)
  result_list <- map.(.x, .f, ...)
  bind_cols.(result_list)
}

#' @export
#' @rdname map.
dt_map_dfc <- map_dfc.

#' @export
#' @rdname map.
map_dfr. <- function(.x, .f, ..., .id = NULL) {
  .f <- as_function(.f)

  result_list <- map.(.x, .f, ...)

  bind_rows.(result_list, .id = .id)
}

#' @export
#' @rdname map.
dt_map_dfr <- map_dfr.

#' @export
#' @rdname map.
map_df. <- map_dfr.

#' @export
#' @rdname map.
dt_map_df <- map_df.

#' @export
#' @rdname map.
walk. <- function(.x, .f, ...) {
  .f <- as_function(.f)

  map.(.x, .f, ...)

  invisible(.x)
}
