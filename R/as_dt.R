#' Deprecated for as_tidytable()
#'
#' @description
#' Lifecycle: soft-deprecated
#'
#' Please use as_tidytable() instead
#'
#'
#' @param .data A data.frame or data.table
#' @export
#'
#' @examples
#' example_df <- data.frame(x = 1:10)
#'
#' example_df %>%
#'   as_tidytable() %>%
#'   dt_mutate(double_x = x * 2)
as_dt <- function(.data) {
  UseMethod("as_dt")
}

#' @export
as_dt.default <- function(.data) {
  warning("as_dt() is deprecated, please use `as_tidytable()")

  as_tidytable(.data)
}
