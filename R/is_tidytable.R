#' Test if the object is a tidytable
#'
#' @description
#' This function returns TRUE for tidytables or subclasses of tidytables, and FALSE for all other objects.
#'
#' @param x An object
#'
#' @export
#'
#' @examples
#' dt <- data.table::data.table(x = 1)
#'
#' is_tidytable(dt)
#'
#' df <- tidytable(x = 1)
#'
#' is_tidytable(df)
is_tidytable <- function(x) {
  inherits(x, "tidytable")
}
