#' Extract the first, last, or nth value from a vector
#'
#' @description
#' Extract the first, last, or nth value from a vector.
#'
#' Note: These are simple wrappers around `vctrs::vec_slice()`.
#'
#' @param x A vector
#' @param n For `nth()`, a number specifying the position to grab.
#' @param default The default value if the value doesn't exist.
#' @param na_rm If `TRUE` ignores missing values.
#'
#' @export
#'
#' @examples
#' vec <- letters
#'
#' first(vec)
#' last(vec)
#' nth(vec, 4)
first <- function(x, default = NULL, na_rm = FALSE) {
  nth(x, 1L, default, na_rm)
}

#' @export
#' @keywords internal
#' @inherit first
first. <- function(x, default = NULL, na_rm = FALSE) {
  deprecate_dot_fun()
  first(x, default, na_rm)
}

#' @export
#' @rdname first
last <- function(x, default = NULL, na_rm = FALSE) {
  nth(x, -1L, default, na_rm)
}

#' @export
#' @keywords internal
#' @inherit first
last. <- function(x, default = NULL, na_rm = FALSE) {
  deprecate_dot_fun()
  last(x, default, na_rm)
}

#' @export
#' @rdname first
nth <- function(x, n, default = NULL, na_rm = FALSE) {
  if (na_rm) {
    x <- x[vec_detect_complete(x)]
  }

  size <- vec_size(x)
  if (n < 0) {
    n <- size + n + 1
  }

  if (n > size || n == 0) {
    nth_default(x, default)
  } else {
    vec_slice2(x, n)
  }
}

#' @export
#' @keywords internal
#' @inherit first
nth. <- function(x, n, default = NULL, na_rm = FALSE) {
  deprecate_dot_fun()
  nth(x, n, default, na_rm)
}

nth_default <- function(x, default) {
  if (obj_is_list(x)) {
    out <- default
  } else {
    default <- default %||% NA
    out <- vec_cast(default, vec_ptype(x))
  }
  out
}

vec_slice2 <- function(x, n) {
  if (obj_is_list(x)) {
    .subset2(x, n)
  } else {
    vec_slice(x, n)
  }
}
