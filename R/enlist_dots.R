#' Enlist dots input by the user
#'
#' @description
#' Enlist dots input by the user. Equivalent to `rlang::enexprs()`
#'
#'
#' @param ... User inputs dots
#'
#'
#' @export
#' @md
#'
#' @examples
#'
#' new_fn <- function(.data, ...) {
#'   dots <- enlist_dots(...)
#' }
enlist_dots <- function(...) substitute(...())
