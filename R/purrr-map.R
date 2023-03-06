#' Apply a function to each element of a vector or list
#'
#' @description
#' The map functions transform their input by applying a function to each element and returning
#' a list/vector/data.table.
#' * `map()` returns a list
#' * `_lgl()`, `_int`, `_dbl`,`_chr`, `_df` variants return their specified type
#' * `_dfr` & `_dfc` Return all data frame results combined utilizing row or column binding
#'
#' @param .x A list or vector
#' @param .y A list or vector
#' @param .l A list to use in `pmap`
#' @param .f A function
#' @param ... Other arguments to pass to a function
#' @param .id Whether `map_dfr()` should add an id column to the finished dataset
#' @param .ptype ptype for resulting vector in `map_vec()`
#'
#' @export
#'
#' @examples
#' map(c(1,2,3), ~ .x + 1)
#'
#' map_dbl(c(1,2,3), ~ .x + 1)
#'
#' map_chr(c(1,2,3), as.character)
map <- function(.x, .f, ...) {
  .f <- as_function(.f)
  lapply(.x, .f, ...)
}

#' @export
#' @keywords internal
#' @inherit map
map. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  map(.x, .f, ...)
}

#' @export
#' @rdname map
map_lgl <- function(.x, .f, ...) {
  .f <- as_function(.f)
  vapply(.x, .f, logical(1), ...)
}

#' @export
#' @keywords internal
#' @inherit map
map_lgl. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  map_lgl(.x, .f, ...)
}

#' @export
#' @rdname map
map_int <- function(.x, .f, ...) {
  .f <- as_function(.f)
  vapply(.x, .f, integer(1), ...)
}

#' @export
#' @keywords internal
#' @inherit map
map_int. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  map_int(.x, .f, ...)
}

#' @export
#' @rdname map
map_dbl <- function(.x, .f, ...) {
  .f <- as_function(.f)
  vapply(.x, .f, double(1), ...)
}

#' @export
#' @keywords internal
#' @inherit map
map_dbl. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  map_dbl(.x, .f, ...)
}

#' @export
#' @rdname map
map_chr <- function(.x, .f, ...) {
  .f <- as_function(.f)
  vapply(.x, .f, character(1), ...)
}

#' @export
#' @keywords internal
#' @inherit map
map_chr. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  map_chr(.x, .f, ...)
}

#' @export
#' @rdname map
map_dfc <- function(.x, .f, ...) {
  result_list <- map(.x, .f, ...)
  bind_cols(result_list)
}

#' @export
#' @keywords internal
#' @inherit map
map_dfc. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  map_dfc(.x, .f, ...)
}

#' @export
#' @rdname map
map_dfr <- function(.x, .f, ..., .id = NULL) {
  result_list <- map(.x, .f, ...)
  bind_rows(result_list, .id = .id)
}

#' @export
#' @rdname map
map_df <- map_dfr

#' @export
#' @keywords internal
#' @inherit map
map_dfr. <- function(.x, .f, ..., .id = NULL) {
  deprecate_dot_fun()
  map_dfr(.x, .f, ..., .id = .id)
}

#' @export
#' @keywords internal
#' @inherit map
map_df. <- function(.x, .f, ..., .id = NULL) {
  deprecate_dot_fun()
  map_df(.x, .f, ..., .id = .id)
}

#' @export
#' @rdname map
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

#' @export
#' @keywords internal
#' @inherit map
walk. <- function(.x, .f, ...) {
  deprecate_dot_fun()
  walk(.x, .f, ...)
}

#' @export
#' @rdname map
map_vec <- function(.x, .f, ..., .ptype = NULL) {
  out <- map(.x, .f, ...)
  list_simplify(out, .ptype)
}

# Simplify a list of values to a vector of their common type
# Ensures every element has size 1
list_simplify <- function(x, ptype = NULL) {
  list_check_all_size(x, 1)
  list_unchop(x, ptype = ptype)
}
