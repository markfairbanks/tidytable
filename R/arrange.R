#' Arrange/reorder rows by variables
#'
#' @description Order rows in ascending or descending order
#'
#' @param .df A data.frame or data.table
#' @param ... Variables to arrange by
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' test_df %>%
#'   arrange.(c, -a)
arrange. <- function(.df, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  eval_quo(
    .df[order(!!!dots)]
  )
}

#' @export
#' @rdname arrange.
dt_arrange <- function(.df, ...) {
  deprecate_soft("0.5.2", "tidytable::dt_arrange()", "arrange.()")

  arrange.(.df, ...)
}
