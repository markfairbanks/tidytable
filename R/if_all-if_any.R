#' Create conditions on a selection of columns
#'
#' @description
#' Helpers to apply a filter across a selection of columns.
#'
#' @param .cols Selection of columns
#' @param .fns Function to create filter conditions
#' @param ... Other arguments passed to the function
#'
#' @export
#'
#' @examples
#' iris %>%
#'   filter(if_any(ends_with("Width"), ~ .x > 4))
#'
#' iris %>%
#'   filter(if_all(ends_with("Width"), ~ .x > 2))
if_all <- function(.cols = everything(), .fns = NULL, ...) {
  abort("if_all() can only work inside of tidytable verbs")
}

#' @export
#' @rdname if_all
if_any <- function(.cols = everything(), .fns = NULL, ...) {
  abort("if_any() can only work inside of tidytable verbs")
}

#' @export
#' @keywords internal
#' @inherit if_all
if_all. <- if_all

#' @export
#' @keywords internal
#' @inherit if_all
if_any. <- if_any
