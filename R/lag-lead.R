#' Get lagging or leading values
#'
#' @description
#' Find the "previous" or "next" values in a vector.
#' Useful for comparing values behind or ahead of the current values.
#'
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by
#' @param default value used for non-existent rows. Defaults to NA.
#'
#' @examples
#' x <- 1:5
#'
#' leads.(x, 1)
#' lags.(x, 1)
#'
#' # Also works inside of `mutate.()`
#' df <- tidytable(x = 1:5)
#'
#' df %>%
#'   mutate.(lag_x = lags.(x))
#'
#' @export
lags. <- function(x, n = 1L, default = NA) {
  shift(x = x, n = n, fill = default, type = "lag", give.names = FALSE)
}

#' @export
#' @rdname lags.
leads. <- function(x, n = 1L, default = NA) {
  shift(x = x, n = n, fill = default, type = "lead", give.names = FALSE)
}
