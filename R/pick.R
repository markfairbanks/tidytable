#' Selection version of `across()`
#'
#' @description
#' Select a subset of columns from within functions like `mutate()`, `summarize()`, or `filter()`.
#'
#' @param ... Columns to select. Tidyselect compatible.
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   mutate(row_sum = rowSums(pick(x, y)))
pick <- function(...) {
  abort("`pick()` can only work inside of tidytable verbs")
}
