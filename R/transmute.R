#' Add new variables and drop all others
#'
#' @description
#' Unlike `mutate()`, `transmute()` keeps only the variables that you create
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to create/modify
#' @param .by Columns to group by
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b")
#' )
#'
#' df %>%
#'   transmute(double_a = a * 2)
transmute <- function(.df, ..., .by = NULL) {
  mutate(.df, ..., .by = {{ .by }}, .keep = "none")
}

#' @export
#' @keywords internal
#' @inherit transmute
transmute. <- function(.df, ..., .by = NULL) {
  deprecate_dot_fun()
  transmute(.df, ..., .by = {{ .by }})
}

