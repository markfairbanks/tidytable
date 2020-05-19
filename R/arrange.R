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
#'
#' test_df %>%
#'   arrange.(c, desc.(a))
arrange. <- function(.df, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  eval_tidy(quo_squash(quo(
    .df[order(!!!dots)]
  )), .df)
}

#' @export
#' @rdname arrange.
dt_arrange <- arrange.
