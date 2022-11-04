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
map2. <- map2

#' @export
#' @rdname map
map2_lgl <- function(.x, .y, .f, ...) {
  as.logical(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_lgl. <- map2_lgl

#' @export
#' @rdname map
map2_int <- function(.x, .y, .f, ...) {
  as.integer(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_int. <- map2_int

#' @export
#' @rdname map
map2_dbl <- function(.x, .y, .f, ...) {
  as.double(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_dbl. <- map2_dbl

#' @export
#' @rdname map
map2_chr <- function(.x, .y, .f, ...) {
  as.character(map2(.x, .y, .f, ...))
}

#' @export
#' @keywords internal
#' @inherit map
map2_chr. <- map2_chr

#' @export
#' @rdname map
map2_dfc <- function(.x, .y, .f, ...) {
  result_list <- map2(.x, .y, .f, ...)
  bind_cols(result_list)
}

#' @export
#' @keywords internal
#' @inherit map
map2_dfc. <- map2_dfc

#' @export
#' @rdname map
map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  result_list <- map2(.x, .y, .f, ...)
  bind_rows(result_list, .id = .id)
}

#' @export
#' @keywords internal
#' @inherit map
map2_dfr. <- map2_dfr

#' @export
#' @rdname map
map2_df <- map2_dfr

#' @export
#' @keywords internal
#' @inherit map
map2_df. <- map2_df

#' @export
#' @rdname map
map2_vec <- function(.x, .y, .f, ..., .ptype = NULL) {
  out <- map2(.x, .y, .f, ...)
  list_simplify(out, .ptype)
}
