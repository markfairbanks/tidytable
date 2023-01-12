#' Split a string into rows
#'
#' @description
#' If a column contains observations with multiple delimited values, separate
#' them each into their own row.
#'
#' @param .df A data.frame or data.table
#' @param cols Columns to separate
#' @param delim Separator delimiting collapsed values
#' @inheritParams rlang::args_dots_empty
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:3,
#'   y = c("a", "d,e,f", "g,h"),
#'   z = c("1", "2,3,4", "5,6")
#' )
#'
#' df %>%
#'   separate_longer_delim(c(y, z), ",")
separate_longer_delim <- function(.df, cols, delim, ...) {
  separate_rows(.df, {{ cols }}, sep = delim)
}
