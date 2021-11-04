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
#' test_df <- tidytable(x = 1:4, y = 1:4)
#'
#' test_df %>%
#'   filter.(x %notin% c(2, 4))
'%notin%' <- function(x, y){
  if (is.character(x) && is.character(y)) {
    !x %chin% y
  } else {
    !vec_in(x, y)
  }
}
