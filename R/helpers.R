#' Helpers
#'
#' Helper functions to capture ..., list(), or c() inputs from the user.
#'
#' @param ... Dots
#'
#' @export
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

insert_empty_i = function(expr){
  call_list = as.list(expr)
  call_list =  c(call_list[1:2], list(substitute()), call_list[-(1:2)])
  as.call(call_list)
}

insert_empty_j <- function(expr) {
  call_list = as.list(expr)
  call_list = c(call_list[1:3], list(substitute()), call_list[-(1:3)])
  as.call(call_list)
}
