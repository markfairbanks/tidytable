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
characterize_list <- function(vec) {
  as.character(substitute(vec))[-1]
}
