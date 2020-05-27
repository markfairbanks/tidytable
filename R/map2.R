#' @export
#' @rdname map.
map2. <- function(.x, .y, .f, ...) {
  .f <- as_function(.f)

  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

#' @export
#' @rdname map.
dt_map2 <- map2.

#' @export
#' @rdname map.
map2_lgl. <- function(.x, .y, .f, ...) {
  as.logical(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname map.
dt_map2_lgl <- map2_lgl.

#' @export
#' @rdname map.
map2_int. <- function(.x, .y, .f, ...) {
  as.integer(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname map.
dt_map2_int <- map2_int.

#' @export
#' @rdname map.
map2_dbl. <- function(.x, .y, .f, ...) {
  as.double(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname map.
dt_map2_dbl <- map2_dbl.

#' @export
#' @rdname map.
map2_chr. <- function(.x, .y, .f, ...) {
  as.character(map2.(.x, .y, .f, ...))
}

#' @export
#' @rdname map.
dt_map2_chr <- map2_chr.

#' @export
#' @rdname map.
map2_dfc. <- function(.x, .y, .f, ...) {
  result_list <- map2.(.x, .y, .f, ...)

  bind_cols.(result_list)
}

#' @export
#' @rdname map.
dt_map2_dfc <- map2_dfc.

#' @export
#' @rdname map.
map2_dfr. <- function(.x, .y, .f, ..., .id = NULL) {
  result_list <- map2.(.x, .y, .f, ...)

  bind_rows.(result_list, .id = .id)
}

#' @export
#' @rdname map.
dt_map2_dfr <- map2_dfc.

#' @export
#' @rdname map.
map2_df. <- map2_dfr.

#' @export
#' @rdname map.
dt_map2_df <- map2_df.
