#' Enlist Dots
#'
#' Helper function to capture ... inputs from the user.
#'
#' @param ...
#'
#' @return list Returns a list for delayed evaluation in a function
#' @export
#'
#' @examples
enlist_dots <- function(...) {
  as.list(substitute(list(...)))[-1]
}
