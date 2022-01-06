#' Apply a function across a selection of columns
#'
#' @description
#' Apply a function across a selection of columns. For use in `arrange.()`,
#' `mutate.()`, and `summarize.()`.
#'
#' @param .cols vector `c()` of unquoted column names. `tidyselect` compatible.
#' @param .fns Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param .names A glue specification that helps with renaming output columns.
#' `{.col}` stands for the selected column, and `{.fn}` stands for the name of the function being applied.
#' The default (`NULL`) is equivalent to `"{.col}"` for a single function case and `"{.col}_{.fn}"`
#' when a list is used for `.fns`.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = rep(1, 3),
#'   y = rep(2, 3),
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   mutate.(across.(c(x, y), ~ .x * 2))
#'
#' df %>%
#'   summarize.(across.(c(x, y), ~ mean(.x, na.rm = TRUE)), .by = z)
#'
#' df %>%
#'   arrange.(across.(c(y, z)))
across. <- function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
  abort("across.() can only work inside of tidytable verbs")
}
