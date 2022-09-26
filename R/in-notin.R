#' `%in%` and `%notin%` operators
#'
#' @description
#' Check with values in a vector are in or not in another vector.
#'
#' Built using `data.table::%chin%` and `vctrs::vec_in` for performance.
#'
#' @param x vector or NULL
#' @param y vector or NULL
#'
#' @details
#' Falls back to base::`%in%` when x and y don't share a common type.
#' This means that the behaviour of base::`%in%` is preserved (e.g. "1" %in% c(1, 2) is TRUE)
#' but loses the speedup provided by vec_in.
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = 1:4, y = 1:4)
#'
#' df %>%
#'   filter(x %in% c(2, 4))
#'
#' df %>%
#'   filter(x %notin% c(2, 4))
#' @rdname in-notin
'%in%' <- function(x, y) {
  if (is.character(x) && is.character(y)) {
    x %chin% y
  } else if (vec_ptype_common(x, y)) {
    vec_in(x, y)
  } else {
    base::'%in%'(x, y)
  }
}

#' @export
#' @rdname in-notin
'%notin%' <- function(x, y) {
  !x %in% y
}


