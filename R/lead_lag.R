#' Lead and Lag
#'
#' Find the "next" or "previous" values in a vector. Useful for comparing values ahead of or behind the current values.
#'
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by
#' @param default value used for non-existent rows. Defaults to NA.
#'
#' @examples
#' x <- 1:5
#'
#' lead.(x, 1)
#' lagg.(x, 1)
#'
#' test_df <- tidytable(x = 1:5)
#'
#' test_df %>%
#'   mutate.(lead_x = lead.(x))
#'
#' @export
lead. <- function(x, n = 1L, default = NA) {
  shift(x = x, n = n, fill = default, type = "lead", give.names = FALSE)
}

#' @rdname lead.
#' @export
lagg. <- function(x, n = 1L, default = NA) {
  shift(x = x, n = n, fill = default, type = "lag", give.names = FALSE)
}
