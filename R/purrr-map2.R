#' @export
#' @rdname map
map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f)
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}

#' @export
#' @keywords internal
#' @inherit map
map2. <- function(.x, .y, .f, ...) {
  deprecate_dot_fun()
  map2(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_lgl <- function(.x, .y, .f, ...) {
  as.logical(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_lgl. <- function(.x, .y, .f, ...) {
  deprecate_dot_fun()
  map2_lgl(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_int <- function(.x, .y, .f, ...) {
  as.integer(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_int. <- function(.x, .y, .f, ...) {
  deprecate_dot_fun()
  map2_int(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_dbl <- function(.x, .y, .f, ...) {
  as.double(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_dbl. <- function(.x, .y, .f, ...) {
  deprecate_dot_fun()
  map2_dbl(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_chr <- function(.x, .y, .f, ...) {
  as.character(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_chr. <- function(.x, .y, .f, ...) {
  deprecate_dot_fun()
  map2_chr(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_dfc <- function(.x, .y, .f, ...) {
  result_list <- map2(.x, .y, .f, ...)
  bind_cols(result_list)
}

#' @export
#' @keywords internal
#' @inherit map
map2_dfc. <- function(.x, .y, .f, ...) {
  deprecate_dot_fun()
  map2_dfc(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  result_list <- map2(.x, .y, .f, ...)
  bind_rows(result_list, .id = .id)
}

#' @export
#' @keywords internal
#' @inherit map
map2_dfr. <- function(.x, .y, .f, ..., .id = NULL) {
  deprecate_dot_fun()
  map2_dfr(.x, .y, .f, ...)
}

#' @export
#' @rdname map
map2_df <- map2_dfr

#' @export
#' @keywords internal
#' @inherit map
map2_df. <- function(.x, .y, .f, ..., .id = NULL) {
  deprecate_dot_fun()
  map2_df(.x, .y, .f, ..., .id = .id)
}

#' @export
#' @rdname map
map2_vec <- function(.x, .y, .f, ..., .ptype = NULL) {
  out <- map2(.x, .y, .f, ...)
  list_simplify(out, .ptype)
}
