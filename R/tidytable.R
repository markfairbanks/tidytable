#' Build a data.table
#'
#' @description
#' `tidytable()` constructs a data.table, but one with nice printing features.
#' As such it can be effectively used as a data.table would be used.
#'
#' @param ... Arguments passed to `data.table()`
#'
#' @md
#' @export
#'
#' @examples
#' `tidytable(x = c(1,2,3), y = c(4,5,6))`
tidytable <- function(...) {
  as_tidytable(data.table(...))
}
