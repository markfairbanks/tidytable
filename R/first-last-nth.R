#' Extract the first, last, or nth value from a vector
#'
#' @description
#' Extract the first, last, or nth value from a vector.
#'
#' Note: These are simple wrappers around `vctrs::vec_slice()`.
#'
#' @param x A vector
#' @param n For `nth.()`, a number specifying the position to grab.
#' @param default The default value if the value doesn't exist.
#'
#' @export
#'
#' @examples
#' vec <- letters
#'
#' first.(vec)
#' last.(vec)
#' nth.(vec, 4)
first. <- function(x, default = NA) {
  nth.(x, 1L, default)
}

#' @rdname first.
#' @export
last. <- function(x, default = NA) {
  nth.(x, -1L, default)
}

#' @rdname first.
#' @export
nth. <- function(x, n, default = NA) {
  size <- vec_size(x)
  if (n < 0) {
    n <- size + n + 1
  }

  if (n > size || n == 0) {
    vec_cast(default, vec_ptype(x))
  } else {
    vec_slice(x, n)
  }
}
