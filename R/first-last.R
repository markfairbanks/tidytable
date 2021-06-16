#' Extract the first or last value from a vector
#'
#' @description
#' Extract the first or last value from a vector.
#'
#' @param x A vector
#'
#' @export
#'
#' @examples
#' vec <- letters
#'
#' first.(vec)
#' last.(vec)
first. <- function(x) {
  vec_slice(x, 1L)
}

#' @rdname first.
#' @export
last. <- function(x) {
  vec_slice(x, vec_size(x))
}
