# Implementation from: https://github.com/r-lib/rlang/blob/master/R/compat-purrr.R

#' @export
#' @rdname map
pmap <- function(.l, .f, ...) {
  .f <- as_function(.f)
  args <- .args_recycle(.l)
  out <- do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
  if (obj_is_list(args)) {
    if (is_named(args[[1]])) {
      names(out) <- names(args[[1]])
    }
  }
  out
}

#' @export
#' @rdname map
pmap_lgl <- function(.l, .f, ...) {
  as.logical(pmap(.l, .f, ...))
}

#' @export
#' @rdname map
pmap_int <- function(.l, .f, ...) {
  as.integer(pmap(.l, .f, ...))
}

#' @export
#' @rdname map
pmap_dbl <- function(.l, .f, ...) {
  as.double(pmap(.l, .f, ...))
}

#' @export
#' @rdname map
pmap_chr <- function(.l, .f, ...) {
  as.character(pmap(.l, .f, ...))
}

#' @export
#' @rdname map
pmap_dfc <- function(.l, .f, ...) {
  result_list <- pmap(.l, .f, ...)
  bind_cols(result_list)
}

#' @export
#' @rdname map
pmap_dfr <- function(.l, .f, ..., .id = NULL) {
  result_list <- pmap(.l, .f, ...)
  bind_rows(result_list, .id = .id)
}

#' @export
#' @rdname map
pmap_df <- pmap_dfr

#' @export
#' @rdname map
pmap_vec <- function(.l, .f, ..., .ptype = NULL) {
  out <- pmap(.l, .f, ...)
  list_simplify(out, .ptype)
}

.args_recycle <- function(args) {
  args <- as.list(args)
  sizes <- list_sizes(args)
  n <- max(sizes)

  args <- map(args, vec_recycle, n)

  args
}
