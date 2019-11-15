#' Enlist Dots
#'
#' Helper function to capture ... inputs from the user.
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
