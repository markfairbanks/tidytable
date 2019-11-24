#' Helpers
#'
#' Helper functions to capture ..., list(), or c() inputs from the user.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
enlist_dots <- function(...) {
  as.list(substitute(list(...)))[-1]
}

#' @export
characterize <- function(vec_list_expr) {
  vle_length <- length(vec_list_expr)
  if (vle_length == 1) {
    as.character(vec_list_expr)
  } else if (as.character(vec_list_expr)[1] == "-"){
    as.character(vec_list_expr)
  } else {
    as.character(vec_list_expr)[-1]
  }
}
