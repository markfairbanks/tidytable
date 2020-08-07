#' notin operator
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
'%notin%' <- function(x,y){
  # vctrs is faster than base %in%, even inside data.table filter
  !vec_in(x, y)
}
