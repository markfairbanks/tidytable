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
#' test_df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' test_df %>%
#'   dt(, double_x := x * 2) %>%
#'   dt(order(-double_x))
#' @export
dt <- function(.df, ...) {
  UseMethod("dt")
}

#' @export
dt.tidytable <- function(.df, ...) {
  # TODO: Add test that let() doesn't modify-by-reference
    ## once 1.14.4 is released
  dots <- as.list(substitute(...()))
  dots_names <- names(dots)

  if (length(dots) > 1 | "j" %chin% dots_names) {
    if ("j" %chin% dots_names) {
      j <- dots[["j"]]
    } else {
      j <- dots[[2]]
    }

    if (is_call(j, c(":=", "let"))) {
      .df <- copy(.df)
    }
  }

  .df[...]
}

#' @export
dt.data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  dt(.df, ...)
}
