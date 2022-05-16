#' notin operator
#'
#' @description
#' "not in" operator - works best when used inside `filter.()`
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
#'   filter.(x %notin% c(2, 4))
'%notin%' <- function(x, y) {
  !x %f_in% y
}

'%f_in%' <- function(x, y) {
  if (is.character(x)) {
    x %chin% y
  } else {
    vec_in(x, y)
  }
}
