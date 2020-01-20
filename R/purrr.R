#' Apply a function to each element of a vector
#'
#' @description
#' The dt_map functions transform their input by applying a function to each element and returning a vector the same length as the input.
#' * `dt_map()` returns a list
#' * `_lgl()`, `_int()`, `_dbl()` and `_chr()` variants return their specified type
#' * `_dfr()` & `_dfc()` Return data frame results binded together
#'
#' *Note: the '~' alias will not work with any of the `dt_map()` functions*
#' @usage
#'
#' dt_map(.x, .f, ...)
#' dt_map_lgl(.x, .f, ...)
#' dt_map_chr(.x, .f, ...)
#' dt_map_dbl(.x, .f, ...)
#' dt_map_dfr(.x, .f, ...)
#' dt_map_dfc(.x, .f, ...)
#' dt_walk(.x, .f, ...)
#' @param .x A list or vector
#' @param .f A function
#' @param ... Other arguments to pass to a function
#'
#' @md
#' @return
#' @export
#'
#' @examples
#' 1:10 %>% map(rnorm, n = 10)
dt_map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

#' @export
#' @rdname dt_map
dt_map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_dbl <- function(.x, .f, ...) {
  vapply(.x, .f, double(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

#' @export
#' @rdname dt_map
dt_map_dfc <- function(.x, .f, ...) {
  result_list <- dt_map(.x, .f, ...)
  do.call(cbind, result_list)
}

#' @export
#' @rdname dt_map
dt_map_dfr <- function(.x, .f, ..., .id = NULL) {
  result_list<- dt_map(.x, .f, ...)
  rbindlist(result_list, idcol = .id)
}

#' @export
#' @rdname dt_map
dt_walk <- function(.x, .f, ...) {
  dt_map(.x, .f, ...)
  invisible(.x)
}

#' @export
#' @rdname dt_map
dt_map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

#' @export
#' @rdname dt_map
dt_map2_lgl <- function(.x, .y, .f, ...) {
  as.logical(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_int <- function(.x, .y, .f, ...) {
  as.integer(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_dbl <- function(.x, .y, .f, ...) {
  as.double(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_chr <- function(.x, .y, .f, ...) {
  as.character(dt_map2(.x, .y, .f, ...))
}

#' @export
#' @rdname dt_map
dt_map2_dfc <- function(.x, .y, .f, ...) {
  result_list <- dt_map2(.x, .y, .f, ...)
  do.call(cbind, result_list)
}

#' @export
#' @rdname dt_map
dt_map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  result_list <- dt_map2(.x, .y, .f, ...)
  rbindlist(result_list, idcol = .id)
}
