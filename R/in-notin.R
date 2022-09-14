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
  } else if (is.list(y)) {
    # https://github.com/markfairbanks/tidytable/issues/565
    base::'%in%'(x, y)
  } else {
    vec_in(x, y)
  }
}

#' @export
#' @rdname in-notin
'%notin%' <- function(x, y) {
  !x %in% y
}


