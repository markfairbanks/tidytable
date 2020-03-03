#' Coerce to tidytable
#'
#' @description
#' A tidytable object is simply a data.table with nice printing features.
#'
#' Note that all tidytable functions automatically convert data.frames & data.tables to tidytables in the background.
#' As such this function will rarely need to be used by the user.
#'
#' @param x An R object
#'
#' @export
#'
#' @examples
#' data.frame(x = 1:3) %>%
#'   as_tidytable()
as_tidytable <- function(x) {
  UseMethod("as_tidytable")
}
#' @export
as_tidytable.default <- function(x) {
  if (is_tidytable(x)) {
    x
  } else if (is.data.table(x)) {
    add_class(x)
  } else {
    add_class(as.data.table(x))
  }
}

# Add dt class to an object
add_class <- function(.data) {
  class(.data) <- c("tidytable", "data.table", "data.frame")
  .data
}
