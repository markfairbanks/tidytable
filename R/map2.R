#' @export
#' @rdname map.
map2. <- function(.x, .y, .f, ...) {
  .f <- as_function(.f)

  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2.
dt_map2 <- function(.x, .y, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map2()", "map2.()")

  map2.(.x, .y, .f, ...)
}

#' @export
#' @rdname map.
map2_lgl. <- function(.x, .y, .f, ...) {
  as.logical(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2_lgl.
dt_map2_lgl <- function(.x, .y, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_lgl()", "map2_lgl.()")

  map2_lgl.(.x, .y, .f, ...)
}

#' @export
#' @rdname map.
map2_int. <- function(.x, .y, .f, ...) {
  as.integer(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2_int.
dt_map2_int <- function(.x, .y, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_int()", "map2_int.()")

  map2_int.(.x, .y, .f, ...)
}

#' @export
#' @rdname map.
map2_dbl. <- function(.x, .y, .f, ...) {
  as.double(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2_dbl.
dt_map2_dbl <- function(.x, .y, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_dbl()", "map2_dbl.()")

  map2_dbl.(.x, .y, .f, ...)
}

#' @export
#' @rdname map.
map2_chr. <- function(.x, .y, .f, ...) {
  as.character(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2_chr.
dt_map2_chr <- function(.x, .y, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_chr()", "map2_chr.()")

  map2_chr.(.x, .y, .f, ...)
}

#' @export
#' @rdname map.
map2_dfc. <- function(.x, .y, .f, ...) {
  result_list <- map2.(.x, .y, .f, ...)

  bind_cols.(result_list)
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2_dfc.
dt_map2_dfc <- function(.x, .y, .f, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_dfc()", "map2_dfc.()")

  map2_dfc.(.x, .y, .f, ...)
}

#' @export
#' @rdname map.
map2_dfr. <- function(.x, .y, .f, ..., .id = NULL) {
  result_list <- map2.(.x, .y, .f, ...)

  bind_rows.(result_list, .id = .id)
}

#' @export
#' @rdname dt_verb
#' @inheritParams map2_dfr.
dt_map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_dfr()", "map2_dfr.()")

  map2_dfr.(.x, .y, .f, ..., .id = .id)
}

#' @export
#' @rdname map.
map2_df. <- map2_dfr.

#' @export
#' @rdname dt_verb
#' @inheritParams map2_df.
dt_map2_df <- function(.x, .y, .f, ..., .id = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_map2_df()", "map2_df.()")

  map2_df.(.x, .y, .f, ..., .id = NULL)
}
