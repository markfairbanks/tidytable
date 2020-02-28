#' Apply a function to each element of a vector
#'
#' @description
#' The dt_map functions transform their input by applying a function to each element and returning a vector the same length as the input.
#' * `dt_map()` returns a list
#' * `_lgl()`, `_int()`, `_dbl()`,`_chr()`, `_df()` variants return their specified type
#' * `_dfr()` & `_dfc()` Return data frame results binded together
#'
#' @param .x A list or vector
#' @param .y A list or vector
#' @param .f A function
#' @param ... Other arguments to pass to a function
#' @param .id Whether `dt_map_dfr()` should add an id column to the finished dataset
#'
#' @md
#' @export
#'
#' @examples
#' dt_map(c(1,2,3), ~.x + 1)
#'
#' dt_map_dbl(c(1,2,3), ~.x + 1)
#'
#' dt_map_chr(c(1,2,3), as.character)
dt_map <- function(.x, .f, ...) {
  .f <- anon_x(.f)

  lapply(.x, .f, ...)
}

#' @export
#' @rdname dt_map
dt_map_lgl <- function(.x, .f, ...) {
  .f <- anon_x(.f)

  vapply(.x, .f, logical(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_int <- function(.x, .f, ...) {
  .f <- anon_x(.f)

  vapply(.x, .f, integer(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_dbl <- function(.x, .f, ...) {
  .f <- anon_x(.f)

  vapply(.x, .f, double(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_chr <- function(.x, .f, ...) {
  .f <- anon_x(.f)

  vapply(.x, .f, character(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_dfc <- function(.x, .f, ...) {
  .f <- anon_x(.f)
  result_list <- dt_map(.x, .f, ...)
  dt_bind_cols(result_list)
}

#' @export
#' @rdname dt_map
dt_map_dfr <- function(.x, .f, ..., .id = NULL) {
  .f <- anon_x(.f)

  result_list <- dt_map(.x, .f, ...)

  dt_bind_rows(result_list, .id = .id)
}

#' @export
#' @rdname dt_map
dt_map_df <- dt_map_dfr

#' @export
#' @rdname dt_map
dt_walk <- function(.x, .f, ...) {
  .f <- anon_x(.f)

  dt_map(.x, .f, ...)

  invisible(.x)
}

#' @export
#' @rdname dt_map
dt_map2 <- function(.x, .y, .f, ...) {
  .f <- anon_xy(.f)

  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

#' @export
#' @rdname dt_map
dt_map2_lgl <- function(.x, .y, .f, ...) {
  .f <- anon_xy(.f)

  as.logical(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_int <- function(.x, .y, .f, ...) {
  .f <- anon_xy(.f)

  as.integer(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_dbl <- function(.x, .y, .f, ...) {
  .f <- anon_xy(.f)

  as.double(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_chr <- function(.x, .y, .f, ...) {
  .f <- anon_xy(.f)

  as.character(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_dfc <- function(.x, .y, .f, ...) {
  .f <- anon_xy(.f)

  result_list <- dt_map2(.x, .y, .f, ...)
  dt_bind_cols(result_list)
}

#' @export
#' @rdname dt_map
dt_map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  .f <- anon_xy(.f)

  result_list <- dt_map2(.x, .y, .f, ...)
  dt_bind_rows(result_list, .id = .id)
}

#' @export
#' @rdname dt_map
dt_map2_df <- dt_map2_dfr

anon_x <- function(fn) {
  if (is_formula(fn)) {
    fn %>%
      deparse(200L) %>%
      str_replace("^~", "function(.x)") %>%
      parse_expr() %>%
      eval()
  } else {
    fn
  }
}

anon_xy <- function(fn) {
  if (is_formula(fn)) {
    fn %>%
      deparse(200L) %>%
      str_replace("^~", "function(.x,.y)") %>%
      parse_expr() %>%
      eval()
  } else {
    fn
  }
}
