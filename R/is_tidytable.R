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
#' df <- data.frame(x = 1:3, y = 1:3)
#'
#' is_tidytable(df)
#'
#' df <- tidytable(x = 1:3, y = 1:3)
#'
#' is_tidytable(df)
is_tidytable <- function(x) {
  inherits(x, "tidytable")
}
