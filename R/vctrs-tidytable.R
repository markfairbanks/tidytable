#' Internal vctrs methods
#'
#' These methods are the extensions that allow tidytable objects to
#' work with vctrs.
#'
#' @keywords internal
#' @name tidytable-vctrs
NULL

# See https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.html#arguments

tt_ptype2 <- function(x, y, ...) {
  as_tidytable(df_ptype2(x, y, ...))
}
tt_cast <- function(x, to, ...) {
  as_tidytable(df_cast(x, to, ...))
}

## vec_ptype2 -------------------------
#' @export
vec_ptype2.tidytable.tidytable <- function(x, y, ...) {
  tt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tidytable.data.frame <- function(x, y, ...) {
  tt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.tidytable <- function(x, y, ...) {
  tt_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tidytable.tbl_df <- vec_ptype2.tidytable.data.frame

#' @export
vec_ptype2.tbl_df.tidytable <- vec_ptype2.data.frame.tidytable

## vec_cast -------------------------
#' @export
vec_cast.tidytable.tidytable <- function(x, to, ...) {
  tt_cast(x, to, ...)
}
#' @export
vec_cast.tidytable.data.frame <- function(x, to, ...) {
  # `x` is a data.frame to be converted to a tidytable
  tt_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.tidytable <- function(x, to, ...) {
  # `x` is a tidytable to be converted to a data.frame
  df_cast(x, to, ...)
}

#' @export
vec_cast.tbl_df.tidytable <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_cast.tidytable.tbl_df <- vec_cast.tidytable.data.frame

