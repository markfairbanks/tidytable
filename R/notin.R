#' notin operator
#'
#' @param x vector or NULL
#' @param y vector or NULL
#'
#' @export
#'
#' @examples
#' c(1,3,11) %notin% 1:10
'%notin%' <- function(x,y){
  # vctrs is faster than base %in%, even inside data.table filter
  !vec_in(x, y)
}
