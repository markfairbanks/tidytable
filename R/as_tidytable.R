#' Coerce an object to a data.table/tidytable
#'
#' @description
#' A tidytable object is simply a data.table with nice printing features.
#' As such it can be used exactly like a data.table would be used.
#'
#' Note that all tidytable functions automatically convert data.frames & data.tables to tidytables in the background.
#' As such this function will rarely need to be used by the user.
#'
#' @param x An R object
#'
#' @export
#'
#' @examples
#' test_df <- data.frame(x = -2:2, c(rep("a", 3), rep("b", 2)))
#'
#' test_df %>%
#'   as_tidytable()
as_tidytable <- function(x) {
  UseMethod("as_tidytable")
}

#' @export
as_tidytable.tidytable <- function(x) {
  x
}

#' @export
as_tidytable.data.table <- function(x) {
  add_class(x)
}

#' @export
as_tidytable.default <- function(x) {
  add_class(as.data.table(x))
}

# Add tidytable class to a data.table
add_class <- function(.df) {

  class(.df) <- c("tidytable", "data.table", "data.frame")

  .df

}

#' Deprecated tidytable conversion
#'
#' @description
#' Deprecated way to convert an object to a tidytable. Please use `as_tidytable()`.
#'
#' @param x An R object
#'
#' @export
#'
#' @examples
#' test_df <- data.frame(x = -2:2, c(rep("a", 3), rep("b", 2)))
#'
#' test_df %>%
#'   as_tidytable()
as_dt <- function(x) {
  deprecate_stop("0.5.5", "tidytable::as_dt()", "as_tidytable()")

  as_tidytable(x)
}
