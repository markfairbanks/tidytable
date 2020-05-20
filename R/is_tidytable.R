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
#' dt <- data.table(x = 1)
#'
#' is_tidytable(dt) # Returns FALSE
#'
#' df <- tidytable(x = 1)
#'
#' is_tidytable(df) # Returns TRUE
is_tidytable <- function(x) {
  inherits(x, "tidytable")
}
