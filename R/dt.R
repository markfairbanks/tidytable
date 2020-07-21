#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call
#'
#' Note: This function does not use data.table's modify-by-reference
#'
#' @param .df A data.frame or data.table
#' @param ... Arguments passed to data.table call. See ?data.table::`[.data.table`
#'
#' @examples
#' test_df <- tidytable(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a", "a", "b"))
#'
#' test_df %>%
#'   dt(, double_x := x * 2) %>%
#'   dt(order(-double_x))
#' @export
dt <- function(.df, ...) {
  UseMethod("dt")
}

#' @export
dt.data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  dots <- substitute(list(...))

  needs_copy <- str_detect.(expr_text(dots), ":=")

  if (needs_copy) .df <- copy(.df)

  .df[...]
}
