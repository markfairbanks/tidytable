#' Add new variables and drop all others
#'
#' @description
#' Unlike `mutate.()`, `transmute.()` keeps only the variables that you create
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to create/modify
#' @param by Columns to group by
#'
#' @md
#' @examples
#' mtcars %>%
#'   transmute.(displ_l = disp / 61.0237)
#'
#' @export
transmute. <- function(.data, ..., by = NULL) {
  UseMethod("transmute.")
}

#' @export
transmute..data.frame <- function(.data, ..., by = NULL) {

  .data <- as_tidytable(.data)

  dots <- enexprs(...)
  by <- enexpr(by)
  keep_names <- names(dots)

  .data <- mutate.(.data, ..., by = !!by)

  .data[, ..keep_names]
}

#' @export
#' @rdname transmute.
dt_transmute <- transmute.


